#' Plankton nets file name
#'
#' @return Character string with file name prefix for plankton nets
#' @noRd
plankton_net_filename <- function() "FORCIS_net_"

#' Pumps file name
#'
#' @return Character string with file name prefix for pumps
#' @noRd
pump_filename <- function() "FORCIS_pump_"

#' CPR North file name
#'
#' @return Character string with file name prefix for CPR North
#' @noRd
cpr_north_filename <- function() "FORCIS_cpr_north_"

#' CPR South file name
#'
#' @return Character string with file name prefix for CPR South
#' @noRd
cpr_south_filename <- function() "FORCIS_cpr_south_"

#' Sediment Traps file name
#'
#' @return Character string with file name prefix for sediment traps
#' @noRd
sediment_trap_filename <- function() "FORCIS_trap_"

#' Vector of device types available in FORCIS database
#'
#' @return Character vector of available data types
#' @noRd
data_types <- function()
  c("Net", "Pump", "Sediment trap", "CPR South", "CPR North")

#' Add a column 'data_type' in data.frame (if required)
#'
#' @param data A data.frame
#' @param type Value to set for data_type column
#' @return Modified data.frame with data_type column
#' @noRd
add_data_type <- function(data, type) {
  check_if_df(data)
  check_if_character(type)

  if ("data_type" %in% colnames(data)) {
    data$"data_type" <- type
  } else {
    data <- data.frame("data_type" = type, data)
  }

  data
}

#' Retrieve data type
#'
#' @param data A data.frame with data_type column
#' @return The unique data_type value
#' @noRd
get_data_type <- function(data) {
  check_if_df(data)
  check_field_in_data(data, "data_type")

  data_type <- unique(data$"data_type")

  if (length(data_type) != 1) {
    stop(
      "The column 'data_type' cannot contain different values",
      call. = FALSE
    )
  }

  data_type
}