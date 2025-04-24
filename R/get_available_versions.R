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
#' # Versions of the FORCIS database ----
#' get_available_versions()

get_available_versions <- function() {
  ## Retrieve information ----

  meta <- get_metadata()
  meta <- lapply(meta$"hits"$"hits", function(x) {
    data.frame(
      "publication_date" = x$"metadata"$"publication_date",
      "version" = x$"metadata"$"version",
      "access_right" = x$"metadata"$"access_right"
    )
  })

  meta <- do.call(rbind.data.frame, meta)

  ## Clean output ----

  meta <- meta[order(as.Date(meta$"publication_date"), decreasing = TRUE), ]

  rownames(meta) <- NULL

  tibble::as_tibble(meta)
}
