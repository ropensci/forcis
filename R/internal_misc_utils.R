#' Get app author
#'
#' Returns the app author name
#'
#' @return The app author name as a string
#' @noRd
get_app_author <- function() {
  "frb-cesab"
}

#' Get app name
#'
#' Returns the app name
#'
#' @return The app name as a string
#' @noRd
get_app_name <- function() {
  "forcis"
}

#' Get app data subdirectory
#'
#' Returns the name of the data subdirectory
#'
#' @return The name of the data subdirectory as a string
#' @noRd
get_app_data_subdir <- function() {
  "data"
}

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
