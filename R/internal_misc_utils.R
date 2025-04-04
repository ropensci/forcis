#' Date format used in raw data
#'
#' @return Character string with date format
#' @noRd
date_format <- function() "%d/%m/%Y"

#' Robinson coordinate system
#'
#' @return Character string with Robinson coordinate projection
#' @noRd
crs_robinson <- function() {
  paste0(
    "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84",
    " +units=m +no_defs"
  )
}
