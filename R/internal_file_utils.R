#' Download a csv file
#'
#' @param url URL of the file to download
#' @param path Path where to save the file
#' @param file Name of the file
#' @param overwrite Whether to overwrite if file exists
#' @param timeout Timeout value for download in seconds
#' @return NULL invisibly
#' @noRd
download_file <- function(url, path, file, overwrite = FALSE, timeout = 60) {
  url <- utils::URLencode(url)
  file <- gsub("\\s", "_", file)

  ## Check if the file already exists ----

  destination <- file.path(path, file)

  if (!overwrite && file.exists(destination)) {
    message(
      "The file '",
      file,
      "' already exists. If you want to download ",
      "again this file please use the argument 'overwrite'."
    )
    return(invisible(NULL))
  }

  ## Download the file if 'overwrite' is TRUE or it doesn't exist ----

  # change timeout for large file and slow connection
  user_opts <- options()
  on.exit(options(user_opts))
  options(timeout = max(timeout, getOption("timeout")))

  tryCatch(
    {
      utils::download.file(url = url, destfile = destination, mode = "wb")

      message("The file '", file, "' has been successfully downloaded")
    },
    error = function(e) {
      message("Download error: ", e$message)

      if (file.exists(destination)) {
        file.remove(destination)

        message("Temporary file deleted")
      }
    }
  )

  invisible(NULL)
}

#' Check if a path exists
#'
#' @param path Directory path to check
#' @return NULL invisibly, raises an error if path doesn't exist
#' @noRd
check_if_path_exists <- function(path) {
  check_if_character(path)

  if (!dir.exists(path)) {
    stop("The directory '", path, "' does not exist", call. = FALSE)
  }

  invisible(NULL)
}

## Preapare cache helper functions

#' Get directory path
#'
#' Returns the path to a directory for the package,
#' either the data directory or a specific version directory
#'
#' @param version Optional version string for the data subfolder
#' @param path Optional custom base path. If NULL (default),
#'             uses the standard user data directory
#' @param create Whether to create the directory if it doesn't exist
#' @return The path to the requested directory
#' @noRd
get_data_dir <- function(version = NULL, path = NULL, create = FALSE) {
  appname <- get_app_name()
  appdata_subdir <- get_app_data_subdir()

  # Use custom path if provided, otherwise use standard location
  data_dir <- if (!is.null(path)) {
    file.path(path, appname, appdata_subdir)
  } else {
    user_data_dir <- R_user_dir(
      package = appname,
      which = "cache"
    )
    file.path(user_data_dir, appdata_subdir)
  }

  # Determine the target directory
  target_dir <- if (!is.null(version)) {
    version <- sanitize_version(version = version)
    file.path(data_dir, version)
  } else {
    data_dir
  }

  # Create if needed
  if (create && !dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  }

  target_dir
}

## TODO: export list_cached_versions() (if the user want to make a check)

#' List locally cached versions
#'
#' Lists all versions that have been downloaded and cached
#'
#' @param path Optional custom path for the data directory. If NULL (default),
#'        uses the standard user data directory
#' @return A character vector of version strings
#' @noRd
list_cached_versions <- function(path = NULL) {
  # Get data directory
  data_dir <- get_data_dir(path = path)

  # Check if directory exists
  if (!dir.exists(data_dir)) {
    return(character(0))
  }

  # List subdirectories (versions)
  versions <- list.dirs(data_dir, full.names = FALSE, recursive = FALSE)

  versions
}

## TODO: export clean_cache() (seperate script)

#' Clean cache
#'
#' Removes downloaded files from cache
#'
#' @param version Specific version to clean (NULL to clean all)
#' @param path Optional custom path for the data directory. If NULL (default),
#'        uses the standard user data directory
#' @return invisible TRUE if successful
#' @noRd
clean_cache <- function(version = NULL, path = NULL, filename = NULL) {
  # Get appropriate directory
  if (!is.null(version)) {
    # Get version directory
    target_dir <- get_data_dir(version = version, path = path)

    # Check if version exists
    if (!dir.exists(target_dir)) {
      warning("Version ", version, " is not cached")
      return(FALSE)
    }

    if (!is.null(filename)) {
      # Remove dataset file
      file_path <- file.path(target_dir, filename)
      unlink(file_path)
      message("Cleaned cache file: ", filename)
    } else {
      # Remove version directory
      unlink(target_dir, recursive = TRUE)
      message("Cleaned cache for version ", version)
    }
  } else {
    # For cleaning everything
    data_dir <- get_data_dir(path = path)

    if (!dir.exists(data_dir)) {
      message("Cache directory does not exist, nothing to clean")
      return(TRUE)
    }

    # Remove the appname directory and everything under it
    unlink(data_dir, recursive = TRUE)
    message("Cleaned all cached data: ", data_dir)
  }

  invisible(TRUE)
}

#' Clean a version string for use as a directory name
#'
#' Removes or replaces characters that are problematic in directory names.
#'
#' @param version Character string representing the version
#'                (e.g., "v1.2.3", "release-2024")
#' @param separator Character to replace special characters with
#'                    (default: "_")
#' @return A string with special characters removed or replaced
#'
#' @noRd
sanitize_version <- function(version, separator = "_") {
  check_version(version)

  # Replace characters that are problematic in directory names
  # This includes: / \ : * ? " < > | and spaces
  cleaned <- gsub("[\\/:*?\"<>|\\s]", separator, version)

  cleaned
}

#' Verify file integrity using checksum
#'
#' Compares a file's checksum with the expected value
#'
#' @param file_path The path to the file
#' @param expected_checksum The expected checksum value
#' @return TRUE if checksum matches, FALSE otherwise
#' @noRd
verify_file_checksum <- function(file_path, expected_checksum) {
  # Extract checksum algorithm and value
  checksum_parts <- strsplit(expected_checksum, ":")[[1]]
  algorithm <- checksum_parts[1]
  expected_value <- checksum_parts[2]

  # Calculate checksum based on algorithm
  if (algorithm == "md5") {
    actual_value <- tools::md5sum(file_path)
  } else {
    # Default to md5 if algorithm is not supported
    warning(
      "Unsupported checksum algorithm: ", algorithm,
      ". Falling back to MD5."
    )
    actual_value <- tools::md5sum(file_path)
  }

  # Compare checksums
  is_valid <- tolower(actual_value) == tolower(expected_value)
  is_valid
}

#' Check if version files exist locally
#'
#' Checks if all files for a specific version exist and are valid
#'
#' @param file_info Data frame with file information
#' @param version_dir The directory to check
#' @return A data frame with file status information
#' @noRd
check_local_files <- function(file_info, version_dir) {
  # Initialize vectors for status
  exists <- logical(nrow(file_info))
  valid <- logical(nrow(file_info))

  # Check each file
  for (i in seq_len(nrow(file_info))) {
    file_path <- file.path(version_dir, file_info$filename[i])

    # Check if file exists
    exists[i] <- file.exists(file_path)

    # Check if file is valid (if it exists)
    if (exists[i]) {
      valid[i] <- verify_file_checksum(file_path, file_info$checksum[i])
    } else {
      valid[i] <- FALSE
    }
  }

  # Add status to file_info
  file_info$exists <- exists
  file_info$valid <- valid
  file_info$needs_download <- !exists | !valid

  file_info
}

#' Download missing or tampered files
#'
#' Downloads only those files that are missing or tampered.
#'
#' @param files_info Data frame with file information
#' @param version_dir The directory where files should be stored
#' @return NULL invisibly
#' @noRd
download_missing_files <- function(files_info, version_dir) {
  # Check if status columns already exist in files_info
  status_columns <- c("exists", "valid", "needs_download")
  status_columns_exist <- all(status_columns %in% names(files_info))

  status_info <- if (status_columns_exist) {
    files_info
  } else {
    check_local_files(files_info, version_dir)
  }

  # Extract files that need downloading (missing or invalid)
  files_to_download <- status_info[status_info$needs_download, ]

  # Download each file that needs it
  if (nrow(files_to_download) > 0) {
    for (i in seq_len(nrow(files_to_download))) {
      # Display appropriate message based on status
      if (!files_to_download$exists[i]) {
        message(sprintf(
          "File %s is missing, downloading...",
          files_to_download$filename[i]
        ))
      } else {
        message(sprintf(
          "File %s is tampered, re-downloading...",
          files_to_download$filename[i]
        ))
      }

      # Download the file
      download_file(
        url = files_to_download$url[i],
        file = files_to_download$filename[i],
        path = version_dir,
        overwrite = TRUE
      )
    }
  }

  # Log valid files
  valid_files <- status_info[!status_info$needs_download, ]
  if (nrow(valid_files) > 0) {
    for (i in seq_len(nrow(valid_files))) {
      message(sprintf("File %s is valid", valid_files$filename[i]))
    }
  }

  invisible(NULL)
}

#' Get Path to Zenodo Metadata Cache File
#'
#' Constructs a file path to the Zenodo metadata cache file based on
#' the specified version and base path.
#'
#' @param version The version of the data to use
#' @param path The base path where data is stored
#' @param filename The name of the metadata cache file
#'                  (default: "zenodo_metadata.rds")
#'
#' @return A character string with the full path to the metadata cache file
#'
#' @noRd
get_meta_cache_path <- function(
    version,
    path,
    filename = "zenodo_metadata.rds") {
  file.path(
    get_data_dir(version = version, path = path),
    filename
  )
}
