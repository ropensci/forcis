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
  ## Check arguments ----

  check_version(version)

  ## Retrieve information ----

  res <- get_metadata(version = version)

  ## Get all versions information ----

  versions <- lapply(res$"hits"$"hits", function(x) {
    data.frame(
      "publication_date" = x$"metadata"$"publication_date",
      "version" = x$"metadata"$"version",
      "access_right" = x$"metadata"$"access_right"
    )
  })

  versions <- do.call(rbind.data.frame, versions)

  ## Subset version ----

  if (is.null(version) || version == "latest") {
    pos <- which.max(as.Date(versions$"publication_date"))
  } else {
    pos <- which(versions$"version" == version)

    if (length(pos) == 0) {
      stop(
        "The required version is not available. Please run ",
        "'get_available_versions()' to list available versions."
      )
    }
  }

  meta <- res$"hits"$"hits"$"metadata"
  files <- res$"hits"$"hits"$"files"

  meta <- lapply(res$"hits"$"hits", function(x) x$"metadata")
  meta <- meta[[pos]]

  meta$"keywords" <- unlist(meta$"keywords")
  meta$"license" <- unlist(meta$"license"$"id")

  meta$"creators" <- lapply(meta$"creators", function(x) {
    data.frame(
      "name" = x$name,
      "affiliation" = ifelse(is.null(x$affiliation), NA, x$affiliation),
      "orcid" = ifelse(is.null(x$orcid), NA, x$orcid)
    )
  })

  meta$"creators" <- do.call(rbind.data.frame, meta$"creators")

  files <- lapply(res$"hits"$"hits", function(x) x$"files")
  files <- files[[pos]]

  files <- lapply(files, function(x) {
    data.frame(
      "id" = x$id,
      "key" = x$key,
      "size" = x$size,
      "checksum" = x$checksum,
      "self" = x$links$self
    )
  })

  files <- do.call(rbind.data.frame, files)

  ## Clean output ----

  pos <- which(names(meta) == "relations")

  if (length(pos) > 0) {
    meta <- meta[-pos]
  }

  pos <- which(names(meta) == "dates")

  if (length(pos) > 0) {
    meta <- meta[-pos]
  }

  meta$"resource_type" <- meta$"resource_type"$"type"

  meta$"files" <- files

  meta
}
