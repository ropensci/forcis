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

data_types <- function() {
  c("Net", "Pump", "Sediment trap", "CPR South", "CPR North")
}

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

#' FORCIS datasets information
#'
#' Create a structured list with information for all FORCIS datasets
#'
#' @return A named list with information for each dataset type
#' @noRd

forcis_datasets_info <- function() {
  # Define all information in a single list structure
  metadata <- list(
    net = list(
      name = "Net",
      filename_prefix = "FORCIS_net_",
      columns = NULL # NULL means use standard species columns
    ),
    pump = list(
      name = "Pump",
      filename_prefix = "FORCIS_pump_",
      columns = NULL
    ),
    sediment_trap = list(
      name = "Sediment trap",
      filename_prefix = "FORCIS_trap_",
      columns = NULL
    ),
    cpr_south = list(
      name = "CPR South",
      filename_prefix = "FORCIS_cpr_south_",
      columns = NULL
    ),
    cpr_north = list(
      name = "CPR North",
      filename_prefix = "FORCIS_cpr_north_",
      columns = c("count_bin_min", "count_bin_max") # Special column handling
    )
  )

  # Get dataset entries only (exclude helper functions)
  dataset_entries <- function() {
    setdiff(names(metadata), c("types", "names", "filename_prefixes"))
  }

  # Add convenience functions - with swapped naming
  metadata$names <- function() dataset_entries() # Returns "net", "pump", etc.
  metadata$types <- function() {
    vapply(
      metadata[dataset_entries()],
      function(x) x$name,
      FUN.VALUE = character(1)
    )
  } # Returns "Net", "Pump", etc.
  metadata$filename_prefixes <- function() {
    vapply(
      metadata[dataset_entries()],
      function(x) x$filename_prefix,
      FUN.VALUE = character(1)
    )
  }

  metadata
}
