#' Load a FORCIS dataset
#'
#' Retrieves and processes a specified FORCIS dataset containing global
#' foraminifera distribution data. The function handles downloading from
#' Zenodo, caching and data processing automatically.
#'
#' @param name Key identifying the dataset. Available options:
#'   \itemize{
#'     \item \code{"net"} - Plankton Net data
#'     \item \code{"pump"} - Pump data
#'     \item \code{"sediment_trap"} - Sediment trap data
#'     \item \code{"cpr_south"} - CPR (South) data
#'     \item \code{"cpr_north"} - CPR (North) data
#'   }
#' @param version Database version to use. Options:
#'   \itemize{
#'     \item \code{NULL} or \code{"latest"} - Use the latest available version
#'   (default)
#'     \item Character string - Specific version number (e.g., "01", "02")
#'   }
#' @param path The directory path to store or read cached data files.
#'   If \code{NULL}, uses a default location in the user's cache directory.
#' @param cached Logical, whether to use cached data files
#'   (\code{TRUE} by default).
#'   When \code{FALSE}, files will be downloaded fresh and cleaned up after use.
#'
#' @return A tibble containing the requested dataset with appropriate column
#'   types. Numeric values for species abundance data, categorical values
#'   for metadata.
#'
#' @examples
#' \dontrun{
#' # Load the latest version of the plankton net dataset
#' net_data <- load_forcis_test("net")
#'
#' # Load a specific version of the pump dataset without caching
#' pump_data <- load_forcis_test("pump", version = "02", cached = FALSE)
#'
#' # Load sediment trap data with custom cache location
#' st_data <- load_forcis_test("sediment_trap", path = ".")
#' }
#'
#' @details
#' The function handles downloading data from Zenodo, verifying file integrity,
#' and converting columns to appropriate types. When caching is enabled, files
#' are saved locally to speed up future access to the same dataset.
#'
#' The forcis database provides global distribution data for planktonic
#' foraminifera collected using various methods.
#'
#' @noRd
load_forcis_test <- function(
    name,
    version = NULL,
    path = NULL,
    cached = TRUE) {
  # TODO: remove '_test' from the function name

  ## Check args ----

  # Check if name is valid
  validate_dataset_name(name = name)

  # Check version
  if (!is.null(version)) {
    check_version(version)
  }

  # Extract dataset-specific information
  metadata <- forcis_datasets_info()
  dataset_info <- metadata[[name]]
  display_name <- dataset_info$name
  dataset_file_pattern <- dataset_info$filename_prefix
  columns_to_process <- dataset_info$columns

  ## Get metadata from Zenodo ---

  # Initialize zenodo_metadata to NULL
  zenodo_metadata <- NULL

  # Resolve version for caching setting
  effective_version <- if (is.null(version) || version == "latest") {
    get_option("latest_version")
  } else {
    version
  }

  # Calculate meta_cache_path once
  meta_cache_path <- NULL
  if (!is.null(effective_version)) {
    meta_cache_path <- get_meta_cache_path(
      version = effective_version,
      path = path
    )
  }

  # Handle caching based on settings
  if (cached && !is.null(meta_cache_path)) {
    # Load from cache if the file exists
    if (file.exists(meta_cache_path)) {
      zenodo_metadata <- readRDS(meta_cache_path)
    }
  } else if (!cached && !is.null(meta_cache_path)) {
    # If not cached, clean up any metadata cache files
    if (file.exists(meta_cache_path)) {
      log_message("Removing cached metadata file: ", meta_cache_path)
      file.remove(meta_cache_path)
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

    version_to_use <- version

    # cached metadata is a single hit
    meta_cache_path <- get_meta_cache_path(
      version = version_to_use,
      path = path
    )
    if (!cached || !file.exists(meta_cache_path)) {
      # Extract metadata of a single hit
      zenodo_metadata <- extract_version_metadata(
        res = zenodo_metadata,
        version = version_to_use
      )
    }

    # Verify version and access rights
    # Example: API may return version 09 & 9 for v 9

    # Check if the version exists and matches first
    if (!is.null(zenodo_metadata)) {
      short_ver <- zenodo_metadata$metadata$version
    }

    if (is.null(zenodo_metadata) || short_ver != version_to_use) {
      log_message("Version doesn't exist: ", version_to_use)
      stop(
        "Error: Version \"", version_to_use, "\" doesn't exist.\n\n",
        "Available options:\n",
        "- Use `get_available_versions()` to retrieve version data\n",
        "- Use `print_available_versions()` to print versions list \n",
        call. = FALSE
      )
    }

    # If the version exists, check access rights
    if (zenodo_metadata$metadata$access_right != "open") {
      stop(
        "Error: Version \"", version, "\" exists but does not have open access!"
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
    # corrupted data due to network maybe O_O
    stop(
      "Ahoy, matey! Trouble on the high seas of data!",
      "Our data loading ship has encountered a rogue wave. ",
      "Please retry to brave the ocean again!"
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

  # vroom return a tibble but just in case
  tibble::as_tibble(data)
}
