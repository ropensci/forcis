#' Check for non-empty data.frame
#'
#' @param data Data.frame to check
#' @return NULL invisibly, raises an error if not a valid data.frame
#' @noRd
check_if_df <- function(data) {
  if (missing(data)) {
    stop("Argument 'data' is required", call. = FALSE)
  }

  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a data.frame", call. = FALSE)
  }

  if (!nrow(data)) {
    stop("Argument 'data' must have at least one row", call. = FALSE)
  }

  invisible(NULL)
}

#' Check if a column is present in a data.frame
#'
#' @param data Data.frame to check
#' @param field Column name to check for
#' @return NULL invisibly, raises an error if field not found
#' @noRd
check_field_in_data <- function(data, field) {
  check_if_df(data)
  check_if_character(field)

  if (!(field %in% colnames(data))) {
    stop(
      "The column '",
      deparse(substitute(field)),
      "' is missing from 'data'",
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Check for non-missing argument of type character and length 1
#'
#' @param str Value to check
#' @return NULL invisibly, raises an error if not a valid character
#' @noRd
check_if_character <- function(str) {
  if (missing(str)) {
    stop("Argument '", deparse(substitute(str)), "' is required", call. = FALSE)
  }

  if (!is.character(str)) {
    stop(
      "Argument '",
      deparse(substitute(str)),
      "' must be a character",
      call. = FALSE
    )
  }

  if (length(str) != 1) {
    stop(
      "Argument '",
      deparse(substitute(str)),
      "' must be of length 1",
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Check for required columns
#'
#' @param data Data.frame to check for required columns
#' @return NULL invisibly, raises an error if required columns are missing
#' @noRd
check_required_columns <- function(data) {
  check_if_df(data)

  if (any(!(get_required_columns() %in% colnames(data)))) {
    stop("Some required columns are absent from data", call. = FALSE)
  }

  invisible(NULL)
}

#' Validate Zenodo API response format
#'
#' @param res API response from Zenodo
#' @param required_fields Character vector of required fields
#' @return NULL invisibly, raises an error if the required fields are missing
#' @noRd
validate_zenodo_response <- function(res, required_fields) {
  for (field in required_fields) {
    if (is.null(eval(parse(text = paste0("res$", field))))) {
      stop("Invalid response format from Zenodo API", call. = FALSE)
    }
  }
  invisible(NULL)
}

#' Validate a dataset name against available datasets.
#'
#' This function checks if a given dataset name is valid by comparing it
#' against the list of available datasets obtained from
#' `forcis_datasets_info()`.
#'
#' @param name A character string representing the dataset name to validate.
#' @noRd
validate_dataset_name <- function(name) {
  # Get metadata directly from forcis_datasets_info
  metadata <- forcis_datasets_info()

  # Check if name is valid
  valid_datasets <- metadata$names()
  if (!name %in% valid_datasets) {
    stop(
      "Invalid dataset name: '", name, "'. ",
      "Available datasets are: ", paste(valid_datasets, collapse = ", "),
      call. = FALSE
    )
  }
}

#' Calculate the MD5 checksum of a file.
#'
#' This function calculates the MD5 checksum of a file located at the specified
#' path.
#'
#' @param file_path A character string specifying the path to the file.
#' @return A character string representing the MD5 checksum of the file,
#'   or \code{NA} if the file does not exist or an error occurs.
#' @noRd
calculate_md5_checksum <- function(file_path) {
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NA)
  }
  tryCatch(
    tools::md5sum(file_path),
    error = function(e) {
      warning(paste(
        "Error calculating MD5 checksum for:",
        file_path, "-",
        e$message
      ))
      NA
    }
  )
}

#' Calculate and compare the MD5 checksum of a file to a given checksum.
#'
#' This function calculates the MD5 checksum of a file located at the specified
#' path and compares it to a provided checksum string.
#'
#' @param file_path A character string specifying the path to the file.
#' @param expected_checksum A character string representing the expected MD5
#' checksum, prefixed with "md5:".
#' @param calculated_checksum An optional pre-calculated MD5 checksum.
#' If provided, the function will use this value instead of recalculating it.
#' @return A logical value: \code{TRUE} if the calculated checksum matches the
#'   expected checksum, \code{FALSE} otherwise. Returns \code{NA} if the file
#'   does not exist or an error occurs during calculation.
#'
#' @noRd
compare_md5_checksum <- function(
    file_path,
    expected_checksum,
    calculated_checksum = NULL) {
  # Use provided checksum or calculate it
  if (is.null(calculated_checksum)) {
    calculated_checksum <- calculate_md5_checksum(file_path)
  }

  if (is.na(calculated_checksum)) {
    return(NA)
  }

  # Remove "md5:" prefix and convert to lowercase
  expected_checksum_value <- gsub("^md5:", "", tolower(expected_checksum))
  calculated_checksum_value <- tolower(calculated_checksum)

  # Compare the checksums
  expected_checksum_value == calculated_checksum_value
}
