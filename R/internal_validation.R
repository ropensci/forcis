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
#' @param res API response from Zenodo (should be a list)
#' @param required_fields Character vector of required field names.
#'        Nested fields can be specified using '$' (e.g., "hits$hits").
#' @return NULL invisibly, raises an error if the required fields are missing
#' @noRd
validate_zenodo_response <- function(res, required_fields) {
  # Check if res is a list
  if (!is.list(res)) {
    stop("Input 'res' must be a list.", call. = FALSE)
  }

  for (field in required_fields) {
    # Split field string by '$' to handle potential nesting
    field_parts <- strsplit(field, "$", fixed = TRUE)[[1]]

    # Start at the top level of the response
    current_obj <- res
    field_exists <- TRUE

    # Navigate through the nested structure
    for (part in field_parts) {
      if (is.list(current_obj) && part %in% names(current_obj)) {
        current_obj <- current_obj[[part]]
        if (is.null(current_obj)) {
          field_exists <- FALSE
          break
        }
      } else {
        field_exists <- FALSE
        break
      }
    }

    if (!field_exists) {
      stop("Invalid response format from Zenodo API: Missing required field '",
        field, "'",
        call. = FALSE
      )
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
