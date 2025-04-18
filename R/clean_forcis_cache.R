#' Clean package data cache
#'
#' Removes downloaded files from the package's data cache directory. This
#' function can clean a specific version's cache, remove individual files from
#' a version, or clean the entire cache completely.
#'
#' @param version Character string specifying a version to clean. If NULL
#'        (default), all cached versions will be removed.
#' @param path Optional character string specifying a custom path for the data
#'        directory.
#'        If NULL (default), uses the standard user data cache directory as
#'        determined by `get_data_dir()`.
#' @param filenames Optional character vector of specific filenames to remove
#'        from the version's cache directory. Only used when `version`
#'        is not NULL.
#'
#' @return Returns `invisible(TRUE)` if the operation was successful.
#'         Returns `invisible(FALSE)` if the specified version does not exist
#'         in the cache.
#'
#' @details
#' The function produces informative messages about which files or directories
#' were cleaned. If a version doesn't exist or a specified file is not found,
#' appropriate warnings are generated.
#'
#' @examples
#' \dontrun{
#' # Clean all cached data
#' clean_forcis_cache()
#'
#' # Clean only data for version "02"
#' clean_forcis_cache(version = "02")
#'
#' # Clean specific files from version "09"
#' clean_forcis_cache(
#'   version = "09",
#'   filenames = c("data.csv", "metadata.json")
#' )
#'
#' # Clean version "10" from a custom path
#' clean_forcis_cache(version = "10", path = ".")
#' }
#'
#' @noRd

clean_forcis_cache <- function(version = NULL, path = NULL, filenames = NULL) {
  # Get appropriate directory
  if (!is.null(version)) {
    # Get version directory
    target_dir <- get_data_dir(version = version, path = path)

    # Check if version exists
    if (!dir.exists(target_dir)) {
      warning("Version ", version, " is not cached")
      return(invisible(FALSE))
    }

    if (!is.null(filenames)) {
      # Process each file
      for (file in filenames) {
        file_path <- file.path(target_dir, file)
        if (file.exists(file_path)) {
          unlink(file_path)
          message("Cleaned cache file: ", file)
        } else {
          warning("File not found in cache: ", file)
        }
      }
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
      return(invisible(TRUE))
    }

    # Remove the appname directory and everything under it
    unlink(data_dir, recursive = TRUE)
    message("Cleaned all cached data: ", data_dir)
  }

  invisible(TRUE)
}
