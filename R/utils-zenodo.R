#' Record identifier in the Zenodo database
#'
#' @noRd

zenodo_id <- function() "7390791"


#' Check Zenodo version
#'
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
  }

  invisible(NULL)
}


#' Set Zenodo version to latest is missing
#'
#' @noRd

set_version <- function(version, ask = TRUE) {
  check_version(version)

  versions <- get_available_versions()
  latest_version <- get_latest_version()

  if (is.null(version)) {
    version <- latest_version
  } else {
    if (!(version %in% versions$"version")) {
      stop(
        "The required version is not available. Please run ",
        "'get_available_versions()' to list available versions.",
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

  version
}


#' Get Zenodo latest version
#'
#' @noRd

get_latest_version <- function() {
  versions <- get_available_versions()

  versions[
    which.max(as.Date(versions$"publication_date")),
    "version",
    drop = TRUE
  ]
}


#' Retrieve Zenodo repo metadata
#'
#' @noRd

get_metadata <- function() {
  ## Prepare request ----

  endpoint <- "https://zenodo.org/api/records/"

  http_request <- httr2::request(endpoint) |>
    httr2::req_url_query(q = paste0("conceptrecid:", zenodo_id())) |>
    httr2::req_url_query(all_versions = "true")

  ## Send HTTP request  ----

  http_response <- httr2::req_perform(http_request)

  ## Check response status ----

  httr2::resp_check_status(http_response)

  ## Return content ----
  httr2::resp_body_json(http_response)
}
