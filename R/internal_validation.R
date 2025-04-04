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