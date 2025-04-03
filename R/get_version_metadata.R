#' Print information of a specific version of the FORCIS database
#'
#' @description
#' Prints information of a specific version of the FORCIS database by querying
#' the Zenodo API (\url{https://developers.zenodo.org}).
#'
#' @param version a `character` of length 1. The label of the version. Use
#'   [get_available_versions()] to list available versions. If `NULL` (default)
#'   the latest version is used.
#'
#' @return A `list` with all information about the version, including: `title`,
#' `doi`, `publication_date`, `description`, `access_right`, `creators`,
#' `keywords`, `version`, `resource_type`, `license`, and `files`.
#'
#' @export
#'
#' @examples
#' # Attach the package ----
#' library("forcis")
#'
#' # Get information for the latest version of the FORCIS database ----
#' get_version_metadata()

get_version_metadata <- function(version = NULL) {
  check_version(version)
  
  # Retrieve metadata
  res <- get_metadata(version)
  
  # Extract versions and determine position
  if (version == "latest") {
    versions <- extract_single_version(res)
    pos <- 1  # Since it's a single item
  } else {
    versions <- extract_versions(res)
    pos <- determine_version_position(version, versions)
  }
  
  # Extract metadata and files
  meta <- extract_metadata(res, pos, version)
  files <- extract_files(res, pos, version)
  
  meta$"files" <- files
  meta$"resource_type" <- meta$"resource_type"$"type"
  
  meta <- clean_metadata(meta)
  meta
}

extract_single_version <- function(res) {
  data.frame(
    "publication_date" = res$"metadata"$"publication_date",
    "version" = res$"metadata"$"version",
    "access_right" = res$"metadata"$"access_right"
  )
}

extract_versions <- function(res) {
  versions <- lapply(res$"hits"$"hits", function(x) {
    data.frame(
      "publication_date" = x$"metadata"$"publication_date",
      "version" = x$"metadata"$"version",
      "access_right" = x$"metadata"$"access_right"
    )
  })
  do.call(rbind.data.frame, versions)
}

determine_version_position <- function(version, versions) {
  if (is.null(version) || version == "latest") {
    pos <- which.max(as.Date(versions$"publication_date"))
  } else {
    pos <- which(versions$"version" == version)
    if (length(pos) == 0) {
      stop("The required version is not available. Please run 'get_available_versions()' to list available versions.")
    }
  }
  
  if (length(pos) == 0) {
    stop("No valid version found in the metadata.")
  }
  
  pos
}

extract_metadata <- function(res, pos, version) {
  if (version == "latest") {
    meta <- res$"metadata"
  } else {
    meta <- lapply(res$"hits"$"hits", function(x) x$"metadata")
    meta <- meta[[pos]]
  }
  meta$"keywords" <- unlist(meta$"keywords")
  meta$"license" <- unlist(meta$"license"$"id")
  meta$"creators" <- extract_creators(meta$"creators")
  meta
}

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

clean_metadata <- function(meta) {
  meta <- remove_unnecessary_fields(meta, c("relations", "dates"))
  meta
}

remove_unnecessary_fields <- function(data, fields) {
  for (field in fields) {
    pos <- which(names(data) == field)
    if (length(pos) > 0) {
      data <- data[-pos]
    }
  }
  data
}