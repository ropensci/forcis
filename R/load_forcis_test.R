#' Load a FORCIS dataset by name
#'
#' @param path The directory path to read from
#' @param version Database version to use, NULL by default
#' @param name Key identifying the dataset ("net", "pump", etc.)
#' @param cached Logical, TRUE by default to cache dataset file & metadata
#'
#' @return A tibble containing the dataset
#' @noRd
load_forcis_test <- function(
    name,
    version = NULL,
    path = NULL,
    cached = TRUE) {
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

  # Initialize zenodo_metadata to NULL
  zenodo_metadata <- NULL

  # Only do cache operations if caching is enabled
  if (cached) {
    # Resolve version
    effective_version <- if (is.null(version) || version == "latest") {
      get_option("latest_version")
    } else {
      version
    }

    # Check cache if we have a valid version
    if (!is.null(effective_version)) {
      meta_cache_path <- get_meta_cache_path(
        version = effective_version,
        path = path
      )

      # Load from cache if the file exists
      if (file.exists(meta_cache_path)) {
        zenodo_metadata <- readRDS(meta_cache_path)
      }
    }
  }

  # Fall back to API call if not loaded from cache
  if (is.null(zenodo_metadata) || !cached) {
    zenodo_metadata <- get_metadata(version = version)
  }


  ## Verify version & extract single metadata ---

  if (is.null(version) || version == "latest") {
    ## zenodo_metadata is a single response (latest endpoint) ---

    validate_zenodo_response(zenodo_metadata, c("metadata"))
    set_option("latest_version", zenodo_metadata$metadata$version)

    version_to_use <- get_option("latest_version")
    log_message("Latest version: ", version_to_use)
  } else {
    ## zenodo_metadata is a multi response (search endpoint) ---

    # cached metadata is a single hit
    meta_cache_exist <- !is.null(meta_cache_path) &&
      !file.exists(meta_cache_path)
    if (!cached || meta_cache_exist) {
      # Extract metadata of a single hit
      zenodo_metadata <- extract_version_metadata(
        res = zenodo_metadata,
        version = version
      )
    }

    # Verify version and access rights
    # Example: API may return version 09 & 9 for v 9

    # Check if the version exists and matches first
    not_found <- is.null(zenodo_metadata) ||
      zenodo_metadata$metadata$version != version
    if (not_found) {
      log_message("Version doesn't exist: ", version)
      stop(
        "Error: Version \"", version, "\" doesn't exist.\n\n",
        "Available options:\n",
        "• Use `get_available_versions()` to retrieve version data\n",
        "• Use `print_available_versions()` to display a formatted list ",
        "of versions\n",
        call. = FALSE
      )
    }

    # If the version exists, check access rights
    if (zenodo_metadata$metadata$access_right != "open") {
      stop(
        "Error: Version \"", version, "\" exists but does not have open access!"
      )
    }

    # If both checks pass, log and assign the version
    log_message("Version found and has open access: ", version)
    version_to_use <- version
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

    # Re-check the status of files that were downloaded
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

    data <- add_data_type(data, display_name)

    ## Check and convert columns ----
    # If we have specific columns to process, use them
    # Otherwise, get species names from data
    if (is.null(columns_to_process)) {
      columns_to_process <- get_species_names(data)
    } else {
      # Verify all columns exist at once
      missing_cols <- setdiff(columns_to_process, names(data))
      if (length(missing_cols) > 0) {
        stop("Missing columns in data: ",
          paste(missing_cols, collapse = ", "),
          call. = FALSE
        )
      }
    }

    # Process all columns
    data[columns_to_process] <- lapply(
      data[columns_to_process],
      as.numeric
    )
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
  } else {
    # Save version metadata
    saveRDS(
      zenodo_metadata,
      get_meta_cache_path(version = version_to_use, path = path)
    )
  }

  tibble::as_tibble(data)
}
