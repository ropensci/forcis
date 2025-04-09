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

  check_if_path_exists(path)

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
  appauthor <- get_app_author()
  appname <- get_app_name()
  appdata_subdir <- get_app_data_subdir()

  # Use custom path if provided, otherwise use standard location
  data_dir <- if (!is.null(path)) {
    file.path(path, appname, appdata_subdir)
  } else {
    # Get user data directory with correct parameter order
    user_data_dir <- rappdirs::user_data_dir(
      appname = appname,
      appauthor = appauthor
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
#' @return TRUE if successful
#' @noRd
clean_cache <- function(version = NULL, path = NULL) {
  # Get appropriate directory
  if (!is.null(version)) {
    # Get version directory
    target_dir <- get_data_dir(version = version, path = path)

    # Check if version exists
    if (!dir.exists(target_dir)) {
      warning("Version ", version, " is not cached")
      return(FALSE)
    }

    # Remove version directory
    unlink(target_dir, recursive = TRUE)
    message("Cleaned cache for version ", version)
  } else {
    # For cleaning everything
    data_dir <- get_data_dir(path = path)

    if (!dir.exists(data_dir)) {
      message("Cache directory does not exist, nothing to clean")
      return(TRUE)
    }

    # If using a custom path
    if (!is.null(path)) {
      appname_dir <- dirname(data_dir)
      print(appname_dir)

      # Remove the appname directory and everything under it
      unlink(appname_dir, recursive = TRUE)
      message("Cleaned all cached data and directory structure")
    } else {
      # For standard path, just clean everything under the data directory
      contents <- list.files(data_dir,
        full.names = TRUE, all.files = TRUE,
        include.dirs = TRUE, no.. = TRUE
      )

      if (length(contents) > 0) {
        vapply(contents, unlink, recursive = TRUE, FUN.VALUE = logical(1))
        message("Cleaned all cached versions")
      } else {
        message("Cache is already empty")
      }
    }
  }

  TRUE
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
