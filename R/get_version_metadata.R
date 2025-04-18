#' Print information of a specific version of the FORCIS database
#'
#' @description
#' Prints information of a specific version of the FORCIS database by querying
#' the Zenodo API (\url{https://developers.zenodo.org}).
#'
#' @param version a `character` of length 1. The label of the version. Use
#'   [get_available_versions()] to list available versions.
#'   If `latest` (default) the latest version is used.
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

get_version_metadata <- function(version = "latest") {
  check_version(version)

  # Retrieve metadata
  res <- get_metadata(version)

  # Extract versions and determine position
  if (version == "latest") {
    versions <- extract_single_version(res)
    pos <- 1 # Since it's a single item
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
