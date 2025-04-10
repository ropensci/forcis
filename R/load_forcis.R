# TODO: custom path should be NULL by default (R_user_dir)

#' Load a FORCIS dataset by name
#'
#' @param path The directory path to read from
#' @param version Database version to use (TODO: should be NULL by default)
#' @param check_for_update Whether to check for database updates
#'                         (TODO: to be removed)
#' @param name Key identifying the dataset ("net", "pump", etc.)
#' @param cached TODO: for later use (cache dataset on local storage)
#'
#' @return A tibble containing the dataset
#' @keywords internal
load_forcis <- function(
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
