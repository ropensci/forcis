#' Zenodo Concept record identifier
#'
#' @return Character string with the concept record ID
#' @noRd
zenodo_conceptrecid <- function() "7390791"

#' Zenodo Record identifier (for v10)
#'
#' @return Character string with the record ID
#' @noRd
zenodo_record_id <- function() "12724286"

#' Zenodo Page Size
#'
#' @return Numeric value for page size
#' @noRd
zenodo_page_size <- function() 999

#' Zenodo API base URL
#'
#' @return Character string with the Zenodo API base URL
#' @noRd
zenodo_api_url <- function() "https://zenodo.org/api"

#' Zenodo Records endpoint URL
#'
#' @return Character string with the Zenodo records endpoint URL
#' @noRd
zenodo_records_endpoint <- function() paste0(zenodo_api_url(), "/records")

#' Zenodo Record Versions endpoint URL
#'
#' @return Character string with the Zenodo record versions endpoint URL
#' @noRd
zenodo_record_versions_url <- function() {
  paste0(zenodo_records_endpoint(), "/", zenodo_record_id(), "/versions")
}

#' Zenodo Latest Version of the record endpoint URL
#'
#' @return Character string with the Zenodo latest version endpoint URL
#' @noRd
zenodo_latest_ver_url <- function() {
  paste0(zenodo_record_versions_url(), "/latest")
}

#' Error message when version is unvailable
#'
#' @return Character string with the error message
#' @noRd
err_msg_missing_version <- function() {
  paste(
    "Required version missing.",
    "Check available versions using get_available_versions()."
  )
}



#' Check Zenodo version
#'
#' @param version Version string to validate
#' @return NULL invisibly, raises an error for invalid version
#' @noRd
check_version <- function(version) {
  if (missing(version)) {
    stop("Argument 'version' is required", call. = FALSE)
  }

  if (!is.null(version)) {
    if (!is.character(version)) {
      stop("Argument 'version' must be character", call. = FALSE)
    }

    if (length(version) != 1) {
      stop("Argument 'version' must be character of length 1", call. = FALSE)
    }

    # Check if version is "latest" or a number
    if (
      version != "latest" &&
        version != "all" &&
        is.na(suppressWarnings(as.numeric(version)))
    ) {
      stop(
        "Argument 'version' must be \"latest\", \"all\" or a number",
        call. = FALSE
      )
    }
  }
}

#' Get Zenodo latest version
#'
#' @param res API single response from Zenodo
#' @return Character string with the latest version number
#' @noRd
get_latest_version <- function(res = NULL) {
  if (is.null(res)) {
    res <- get_metadata(version = "latest")
  }

  versions <- extract_single_version(res)
  versions <- versions$version
}

#' Set Zenodo version to latest is missing
#'
#' @param version Requested version or NULL for current/latest
#' @param ask Whether to prompt for upgrading to latest version
#' @return Character string with the version to use
#' @noRd
set_version <- function(version, ask = TRUE) {
  check_version(version)

  versions <- get_available_versions()
  latest_version <- get_latest_version()

  if (is.null(version)) {
    version <- get_current_version()

    if (is.null(version)) {
      version <- latest_version
    }
  } else {
    if (!(version %in% versions$"version")) {
      stop(
        err_msg_missing_version(),
        call. = FALSE
      )
    }
  }

  if (ask) {
    if (version != latest_version) {
      answer <- readline(paste0(
        "A newer version of the FORCIS database is ",
        "available. ",
        "Do you want to download it [Y/n]? "
      ))

      if (answer == "") answer <- "y"

      answer <- tolower(answer)

      if (!(answer %in% c("y", "n"))) {
        stop("Please type 'y' or 'n'", call. = FALSE)
      }

      if (answer == "y") {
        version <- latest_version
      }
    }
  }

  save_version(version)

  version
}

#' Set/update local database version number in an hidden file .forcis
#'
#' @param version Version string to save
#' @return NULL invisibly
#' @noRd
save_version <- function(version) {
  saved_version <- get_current_version()

  if (is.null(saved_version) || saved_version != version) {
    version <- paste0("FORCIS_VERSION=", version)
    cat(version, file = ".forcis", append = FALSE, sep = "\n")
  }

  invisible(NULL)
}

#' Get Records metadata by making call to zenodo API
#'
#' @param version Version to retrieve metadata for
#' @return List with JSON response from Zenodo API
#' @noRd
get_metadata <- function(version = NULL) {
  if (is.null(version)) version <- "latest" # tmp to avoid breaking it
  check_version(version)
  tryCatch(
    {
      log_message("Zenodo API: Loading metadata...")
      http_request <- build_http_request(version)
      http_response <- httr2::req_perform(http_request)
      httr2::resp_check_status(http_response)
      log_message("Zenodo API: metadata has been loaded successfully!")
      httr2::resp_body_json(http_response)
    },
    error = function(e) {
      stop("Zenodo API: Failed to retrieve metadata.",
        e$message,
        call. = FALSE
      )
    }
  )
}

#' Build HTTP request to zenodo API
#'
#' @param version Version to build request for
#' @return httr2 request object
#' @noRd
build_http_request <- function(version) {
  switch(version,
    "latest" = httr2::request(zenodo_latest_ver_url()),
    "all" = httr2::request(zenodo_record_versions_url()) |>
      httr2::req_url_query(size = zenodo_page_size(), sort = "version"),
    {
      check_version(version)
      httr2::request(zenodo_records_endpoint()) |>
        httr2::req_url_query(
          q = paste0(
            "conceptrecid:",
            zenodo_conceptrecid(),
            " AND version:",
            version
          ),
          all_versions = "true",
          size = zenodo_page_size()
        )
    }
  )
}

#' Extract version information from a single API response
#'
#' @param res API response from Zenodo
#' @return Data frame with publication date, version, and access right
#' @noRd
extract_single_version <- function(res) {
  validate_zenodo_response(res, c("metadata"))
  data.frame(
    "publication_date" = res$"metadata"$"publication_date",
    "version" = res$"metadata"$"version",
    "access_right" = res$"metadata"$"access_right"
  )
}

#' Extract version information from multiple API responses
#'
#' @param res API response containing multiple hits from Zenodo
#' @return Data frame with publication dates, versions, and access rights
#' @noRd
extract_versions <- function(res) {
  validate_zenodo_response(res, c("hits", "hits$hits"))
  versions <- lapply(res$"hits"$"hits", function(x) {
    data.frame(
      "publication_date" = x$"metadata"$"publication_date",
      "version" = x$"metadata"$"version",
      "access_right" = x$"metadata"$"access_right"
    )
  })
  do.call(rbind.data.frame, versions)
}

#' Determine the position of a version in the versions list
#'
#' @param version Version to find
#' @param versions Data frame of available versions
#' @return Numeric index of the version position
#' @noRd
determine_version_position <- function(version, versions) {
  if (is.null(version) || version == "latest") {
    pos <- which.max(as.Date(versions$"publication_date"))
  } else {
    pos <- which(versions$"version" == version)
    if (length(pos) == 0) {
      stop(
        err_msg_missing_version()
      )
    }
  }

  if (length(pos) == 0) {
    stop("No valid version found in the metadata.")
  }

  pos
}

#' Extract metadata from API response
#'
#' @param res API response from Zenodo
#' @param pos Position of the version in multiple responses
#' @param version Version string
#' @return List with processed metadata
#' @noRd
extract_metadata <- function(res, pos, version) {
  if (version == "latest") {
    validate_zenodo_response(res, c("metadata"))
    meta <- res$"metadata"
  } else {
    validate_zenodo_response(res, c("hits", "hits$hits"))
    meta <- lapply(res$"hits"$"hits", function(x) x$"metadata")
    meta <- meta[[pos]]
  }
  meta$"keywords" <- unlist(meta$"keywords")
  meta$"license" <- unlist(meta$"license"$"id")
  meta$"creators" <- extract_creators(meta$"creators")
  meta
}

#' Extract creator information from metadata
#'
#' @param creators List of creators from metadata
#' @return Data frame with creator details
#' @noRd
extract_creators <- function(creators) {
  creators <- lapply(creators, function(x) {
    data.frame(
      "name" = x$name,
      "affiliation" = ifelse(is.null(x$affiliation), NA, x$affiliation),
      "orcid" = ifelse(is.null(x$orcid), NA, x$orcid)
    )
  })
  do.call(rbind.data.frame, creators)
}

#' Extract file information from API response
#'
#' @param res API response from Zenodo
#' @param pos Position of the version in multiple responses
#' @param version Version string
#' @return Data frame with file details
#' @noRd
extract_files <- function(res, pos, version) {
  if (version == "latest") {
    files <- res$"files"
  } else {
    files <- lapply(res$"hits"$"hits", function(x) x$"files")
    files <- files[[pos]]
  }
  files <- lapply(files, function(x) {
    data.frame(
      "id" = x$id,
      "key" = x$key,
      "size" = x$size,
      "checksum" = x$checksum,
      "self" = x$links$self
    )
  })
  do.call(rbind.data.frame, files)
}

#' Clean metadata by removing unnecessary fields
#'
#' @param meta Metadata list from Zenodo API
#' @return Cleaned metadata list
#' @noRd
clean_metadata <- function(meta) {
  meta <- remove_unnecessary_fields(meta, c("relations", "dates"))
  meta
}

#' Remove specified fields from a data structure
#'
#' @param data Data structure from which to remove fields
#' @param fields Character vector of field names to remove
#' @return Modified data structure with fields removed
#' @noRd
remove_unnecessary_fields <- function(data, fields) {
  for (field in fields) {
    pos <- which(names(data) == field)
    if (length(pos) > 0) {
      data <- data[-pos]
    }
  }
  data
}

#' Extract metadata for a specific version from a Zenodo API response
#'
#' This function searches through a Zenodo API response to find metadata
#' for a specific version of a record.
#'
#' @param res A list containing the Zenodo API response. Must contain
#'        nested elements 'hits' and 'hits$hits'.
#' @param version A character string representing
#'                the version number to search for.
#'
#' @return A list containing the complete metadata for the specified version
#'         if found, or NULL if no matching version is found.
#' @noRd
extract_version_metadata <- function(res, version) {
  validate_zenodo_response(res, c("hits", "hits$hits"))
  hits <- res$hits$hits
  for (hit in hits) {
    validate_zenodo_response(hit, c("metadata"))
    if (!is.null(hit$metadata$version) && hit$metadata$version == version) {
      return(hit)
    }
  }
  NULL
}

#' Get file URLs from record metadata
#'
#' Extracts file information from a Zenodo record's metadata
#'
#' @param metadata The record metadata
#' @param prefix_filter Optional prefix to filter filenames (default: NULL)
#' @return A data frame with file information (filename, url, size, checksum)
#' @noRd
get_files_info <- function(metadata, prefix_filter = NULL) {
  # Extract files information
  files <- metadata$files

  # Filter files by prefix if needed
  if (!is.null(prefix_filter)) {
    files <- files[vapply(
      files,
      function(x) startsWith(x$key, prefix_filter), logical(1)
    )]
  }

  # Create data frame with file information
  file_info <- data.frame(
    filename = vapply(files, function(x) x$key, character(1)),
    url = vapply(files, function(x) x$links$self, character(1)),
    size = vapply(files, function(x) x$size, numeric(1)),
    checksum = vapply(files, function(x) x$checksum, character(1)),
    stringsAsFactors = FALSE
  )

  file_info
}
