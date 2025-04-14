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
  dataset_file_pattern <- dataset_info$filename_prefix
  columns_to_process <- dataset_info$columns

  ## Check args ----

  if (!is.null(version)) {
    check_version(version)
  }

  ## Get metadata from Zenodo ---

  zenodo_metadata <- get_metadata(version = version)

  ## Verify version & extract single metadata ---

  if (is.null(version) || version == "latest") {
    # zenodo_metadata is a single response (latest endpoint)
    validate_zenodo_response(zenodo_metadata, c("metadata"))
    latest_version <- zenodo_metadata$metadata$version
    log_message("Current version: ", latest_version)
    version_to_use <- latest_version
  } else {
    # zenodo_metadata is a multi response (search endpoint)
    validate_zenodo_response(zenodo_metadata, c("hits", "hits$hits"))
    # Extract metadata of a single hit
    zenodo_metadata <- extract_version_metadata(
      res = zenodo_metadata,
      version = version
    )
    # verify version for example: API could return version 09 & 9 for version 9
    if (!is.null(zenodo_metadata)) {
      log_message("Version exist: ", version)
      version_to_use <- version
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

  ## Check cache and files to download ---

  log_message("Dataset version to use: ", version_to_use)

  # Get version directory
  version_cache_dir <- get_data_dir(
    version = version_to_use,
    path = path,
    create = TRUE
  )
  log_message("Dataset cache path: ", version_cache_dir)

  # Get dataset files information
  dataset_files_info <- get_files_info(zenodo_metadata,
    prefix_filter = dataset_file_pattern
  )

  # Check local files
  file_status <- check_local_files(dataset_files_info, version_cache_dir)

  # Download missing or tampered files
  if (any(file_status$needs_download)) {
    files_to_download <- file_status[file_status$needs_download, ]
    download_missing_files(files_to_download, version_cache_dir)

    # Re-check local files (update status after download)
    file_status <- check_local_files(dataset_files_info, version_cache_dir)
  }

  ## Read dataset data ---

  # Dataset file has been downloaded & should be valid
  data <- NULL
  if (file_status$exists && file_status$valid) {
    ## Read data ----
    data <- vroom::vroom(
      file.path(version_cache_dir, file_status$filename),
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
  } else {
    message(
      "Oops! something went wrong!",
      "Please retry to load data again..."
    )
  }

  # Clean up if not caching
  if (!cached) {
    log_message("Cache disabled, cleaning up files")
    clean_cache(
      version = version_to_use,
      path = path,
      filename = file_status$filename
    )
  }

  tibble::as_tibble(data)
}
