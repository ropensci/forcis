# TODO: custom path should be NULL by default (R_user_dir)

#' Load a FORCIS dataset by name
#'
#' @param path The directory path to read from
#' @param version Database version to use (TODO: should be NULL by default)
#' @param name Key identifying the dataset ("net", "pump", etc.)
#' @param cached TODO: for later use (cache dataset on local storage)
#'
#' @return A tibble containing the dataset
#' @noRd
load_forcis_test <- function(
    name,
    version = NULL,
    path = NULL,
    cached = FALSE) {
  # Check if name is valid
  validate_dataset_name(name = name)

  # Extract dataset-specific information
  metadata <- forcis_datasets_info()
  dataset_info <- metadata[[name]]
  display_name <- dataset_info$name
  file_pattern <- dataset_info$filename_prefix
  columns_to_process <- dataset_info$columns

  ## Check args ----
  # check_if_character(path)
  if (!is.null(version)) {
    check_version(version)
  }

  ## Get metadata from Zenodo
  zenodo_metadata <- get_metadata(version = version)


  if (is.null(version) || version == "latest") {
    # zenodo_metadata is a single response (latest endpoint)
    validate_zenodo_response(zenodo_metadata, c("metadata"))
    latest_version <- zenodo_metadata$metadata$version
    log_message("Current version: ", latest_version)
    version_to_use <- latest_version
  } else {
    # zenodo_metadata is a multi response (search endpoint)
    # metadata could contain one or zero hit(s)
    # either check "total" or "hits" length
    # if "total" is 0 mean version doesn't exist (no hit)
    # if "total" is 1 mean version exist (a hit) need to verify version
    # if "total" > 1 something wrong :)
    validate_zenodo_response(zenodo_metadata, c("hits", "hits$hits"))
    if (zenodo_metadata$hits$total == 1) {
      log_message("Version exist: ", version)
    } else {
      log_message("Version doen't exist: ", version)
      # List available versions
      available_versions <- get_available_versions()
      stop(
        "Version \"", version, "\" doesn't exist.\n",
        "Available versions are: ",
        paste(
          mapply(
            function(ver, access, date) {
              sprintf("\n\"%s\" (%s - %s)", ver, access, date)
            },
            available_versions$version,
            available_versions$access_right,
            available_versions$publication_date
          ),
          collapse = ", "
        ),
        call. = FALSE
      )
    }
  }




  # # TODO: download only files related to the specified dataset
  # # TODO: debate whether to download the extra files like the boubdries file

  # ## Check local database ----
  # path <- file.path(path, "forcis-db", paste0("version-", version))

  # if (!dir.exists(path)) {
  #   stop(
  #     "The directory '", path,
  #     "' does not exist. Please check the ",
  #     "argument 'path' or use the function 'download_forcis_db()'.",
  #     call. = FALSE
  #   )
  # }

  # ## Check file ----

  # file_name <- list.files(path, pattern = file_pattern)

  # if (!length(file_name)) {
  #   stop(
  #     "The ", display_name, " dataset does not exist. Please use the function ",
  #     "'download_forcis_db()'.",
  #     call. = FALSE
  #   )
  # }

  # ## Check for update ----
  # # TODO: remove the check for update
  # if (is.null(check_for_update)) {
  #   check_for_update <- TRUE
  # }

  # if (check_for_update) {
  #   if (version != get_latest_version()) {
  #     message(
  #       "A newer version of the FORCIS database is available. Use ",
  #       "'download_forcis_db(version = NULL)' to download it."
  #     )
  #   }
  # }

  # ## Read data ----
  # data <- vroom::vroom(
  #   file.path(path, file_name),
  #   delim = ";",
  #   altrep = FALSE,
  #   show_col_types = FALSE
  # )

  # data <- as.data.frame(data)
  # data <- add_data_type(data, display_name)

  # ## Check and convert columns ----
  # # If we have specific columns to process, use them
  # # Otherwise, get species names from data
  # if (is.null(columns_to_process)) {
  #   columns_to_process <- get_species_names(data)
  # } else {
  #   # For explicit columns, verify they exist
  #   for (col in columns_to_process) {
  #     check_field_in_data(data, col)
  #   }
  # }

  # # Process all columns
  # for (col in columns_to_process) {
  #   data[[col]] <- as.numeric(data[[col]])
  # }

  # tibble::as_tibble(data)
}
