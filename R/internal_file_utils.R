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