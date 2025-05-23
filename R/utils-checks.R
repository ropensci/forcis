#' Check if a path exists
#'
#' @noRd

check_if_path_exists <- function(path) {
  check_if_character(path)

  if (!dir.exists(path)) {
    stop("The directory '", path, "' does not exist", call. = FALSE)
  }

  invisible(NULL)
}


#' Check for non-empty data.frame
#'
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
#' @noRd

check_required_columns <- function(data) {
  check_if_df(data)

  if (any(!(get_required_columns() %in% colnames(data)))) {
    stop("Some required columns are absent from data", call. = FALSE)
  }

  invisible(NULL)
}


#' Check if a taxonomy name is valid
#'
#' @noRd

check_if_valid_taxonomy <- function(taxonomy) {
  check_if_character(taxonomy)
  taxonomy <- tolower(taxonomy)

  taxonomies <- unique(species_list()[, "taxonomy"])
  taxonomies <- tolower(taxonomies)

  if (!(taxonomy %in% taxonomies)) {
    stop(
      "Bad taxonomy. Valid taxonomies names are: ",
      toString(toupper(taxonomies)),
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Check for multiple taxonomies
#'
#' @noRd

check_unique_taxonomy <- function(data) {
  check_if_df(data)

  if (get_data_type(data) != "CPR North") {
    check_required_columns(data)

    pos <- which(species_list()[, "taxon"] %in% colnames(data))

    if (length(pos) > 0) {
      taxonomy <- unique(species_list()[pos, "taxonomy"])

      if (length(taxonomy) > 1) {
        stop(
          "Multiple taxonomies are not allowed. Please use the function ",
          "'select_taxonomy()' before going any further",
          call. = FALSE
        )
      }
    }
  }

  invisible(NULL)
}
