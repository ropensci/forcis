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
    sapply(
      metadata[dataset_entries()],
      function(x) x$name
    )
  } # Returns "Net", "Pump", etc.
  metadata$filename_prefixes <- function() {
    sapply(
      metadata[dataset_entries()],
      function(x) x$filename_prefix
    )
  }

  return(metadata)
}

# TODO: read_dataset_by_name -> load_dataset() or load_forcis()

#' Read a FORCIS dataset by name
#'
#' @param path The directory path to read from
#' @param version Database version to use
#' @param check_for_update Whether to check for database updates
#' @param name Key identifying the dataset ("net", "pump", etc.)
#'
#' @return A tibble containing the dataset
#' @noRd
read_dataset_by_name <- function(
    name,
    path = ".",
    version = options()$"forcis_version",
    check_for_update = options()$"forcis_check_for_update",
    cached = TRUE) {
  # Check if name is valid
  validate_dataset_name(name = name)

  # Extract dataset-specific information
  metadata <- forcis_datasets_info()
  dataset_info <- metadata[[name]]
  display_name <- dataset_info$name
  file_pattern <- dataset_info$filename_prefix
  columns_to_process <- dataset_info$columns

  ## Check args ----
  check_if_character(path)
  check_version(version)

  ## Check/set version ----
  version <- set_version(version, ask = FALSE)


  # TODO: download only files related to the specified dataset
  # TODO: debate whether to download the extra files like the boubdries file

  ## Check local database ----
  path <- file.path(path, "forcis-db", paste0("version-", version))

  if (!dir.exists(path)) {
    stop(
      "The directory '", path,
      "' does not exist. Please check the ",
      "argument 'path' or use the function 'download_forcis_db()'.",
      call. = FALSE
    )
  }

  ## Check file ----

  file_name <- list.files(path, pattern = file_pattern)

  if (!length(file_name)) {
    stop(
      "The ", display_name, " dataset does not exist. Please use the function ",
      "'download_forcis_db()'.",
      call. = FALSE
    )
  }

  ## Check for update ----
  # TODO: remove the check for update
  if (is.null(check_for_update)) {
    check_for_update <- TRUE
  }

  if (check_for_update) {
    if (version != get_latest_version()) {
      message(
        "A newer version of the FORCIS database is available. Use ",
        "'download_forcis_db(version = NULL)' to download it."
      )
    }
  }

  ## Read data ----
  data <- vroom::vroom(
    file.path(path, file_name),
    delim = ";",
    altrep = FALSE,
    show_col_types = FALSE
  )

  data <- as.data.frame(data)
  data <- add_data_type(data, display_name)

  ## Check and convert columns ----
  # If we have specific columns to process, use them
  # Otherwise, get species names from data
  if (is.null(columns_to_process)) {
    columns_to_process <- get_species_names(data)
  } else {
    # For explicit columns, verify they exist
    for (col in columns_to_process) {
      check_field_in_data(data, col)
    }
  }

  # Process all columns
  for (col in columns_to_process) {
    data[[col]] <- as.numeric(data[[col]])
  }

  tibble::as_tibble(data)
}
