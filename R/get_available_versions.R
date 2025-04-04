#' Get available versions of the FORCIS database
#'
#' @description
#' Gets all available versions of the FORCIS database by querying the Zenodo API
#' (\url{https://developers.zenodo.org}).
#'
#' @return A `tibble` with three columns:
#'   - `publication_date`: the date of the release of the version
#'   - `version`: the label of the version
#'   - `access_right`: is the version open or restricted?
#'
#' @export
#'
#' @examples
#' # Attach the package ----
#' library("forcis")
#'
#' # Versions of the FORCIS database ----
#' get_available_versions()
get_available_versions <- function() {
  ## Retrieve information ----

  meta <- get_metadata(version = "all")
  meta <- extract_versions(meta)

  ## Clean output ----

  meta <- meta[order(as.Date(meta$"publication_date"), decreasing = TRUE), ]

  rownames(meta) <- NULL

  tibble::as_tibble(meta)
}
