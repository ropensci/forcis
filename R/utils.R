#' Download a csv file
#'
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
      utils_download_file(url = url, destfile = destination, mode = "wb")

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

#' download.file wrapper
#'
#' to make mock testing easier
#' @noRd
utils_download_file <- function(url, destfile, mode, ...) {
  utils::download.file(url = url, destfile = destfile, mode = mode, ...)
}


#' Date format used in raw data
#'
#' @noRd

date_format <- function() "%d/%m/%Y"


#' Retrieve data type
#'
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


#' Add a column 'data_type' in data.frame (if required)
#'
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


#' Read a FORCIS datasheet
#'
#' @noRd

read_data_ <- function(
  check_file_fun,
  data_msg,
  path,
  version = options()$"forcis_version",
  check_for_update = options()$"forcis_check_for_update"
) {
  ## Check args ----

  check_if_character(path)
  check_version(version)

  ## Check/set version ----
  version <- set_version(version, ask = FALSE)

  ## Check local database ----
  path <- file.path(path, "forcis-db", paste0("version-", version))

  if (!dir.exists(path)) {
    stop(
      "The directory '",
      path,
      "' does not exist. Please check the ",
      "argument 'path' or use the function 'download_forcis_db()'.",
      call. = FALSE
    )
  }

  ## Check file ----
  file_name <- list.files(path, pattern = check_file_fun())

  if (!length(file_name)) {
    stop(
      sprintf(
        "The %s dataset does not exist. Please use the function ",
        data_msg
      ),
      "'download_forcis_db()'.",
      call. = FALSE
    )
  }

  ## Check for update ----

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

  file_name <- list.files(path, pattern = check_file_fun())

  data <- vroom::vroom(
    file.path(path, file_name),
    delim = ";",
    altrep = FALSE,
    show_col_types = FALSE
  )

  as.data.frame(data)
}


#' Robinson coordinate system
#'
#' @noRd

crs_robinson <- function() {
  paste0(
    "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84",
    " +units=m +no_defs"
  )
}
