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

#' Get package option
#'
#' Retrieves a package option or returns default value
#'
#' @param name The option name
#' @param default The default value if option is not set
#' @return The option value
#' @noRd
get_option <- function(name, default = NULL) {
  # Full option name
  full_name <- paste0(get_app_name(), ".", name)

  # Get option value
  value <- getOption(full_name, default)

  return(value)
}

#' Set package option
#'
#' Sets a package option
#'
#' @param name The option name
#' @param value The value to set
#' @return Invisibly returns the old value
#' @noRd
set_option <- function(name, value) {
  # Full option name
  full_name <- paste0(get_app_name(), ".", name)

  # Get current value
  old_value <- getOption(full_name)

  # Set new value
  options(list = structure(list(value), names = full_name))

  return(invisible(old_value))
}

#' Log message with timestamp
#'
#' Logs a message with a timestamp
#'
#' @param ... Message components, passed to message()
#' @param level The log level (default: "INFO")
#' @return Invisibly returns NULL
#' @noRd
log_message <- function(..., level = "INFO") {
  # Only log if debug mode is on
  if (get_option("debug", FALSE) || level == "ERROR") {
    # Get timestamp
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    # Create message
    msg <- paste0("[", timestamp, "] [", level, "] ", ..., collapse = "")

    # Print message
    message(msg)
  }

  return(invisible(NULL))
}
