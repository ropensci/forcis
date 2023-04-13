#' Date format used in raw data
#' 
#' @noRd

date_format <- function() "%d/%m/%Y"



#' Check if a path exists
#' 
#' If the path `path` does not exist, returns an error.
#' 
#' @inheritParams get_forcis_db
#' 
#' @noRd

check_if_path_exists <- function(path) {
  
  if (!dir.exists(path)) {
    stop("The directory '", path, "' does not exist", call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check for non-empty data.frame
#' 
#' @noRd

check_if_not_df <- function(data) {
  
  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a data.frame", call. = FALSE)
  }
  
  if (!nrow(data)) {
    stop("Argument 'data' must have at least one row", call. = FALSE)
  }
  
  if (!ncol(data)) {
    stop("Argument 'data' must have at least one column", call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check for non-missing argument of type character and length 1
#' 
#' @noRd

is_character <- function(str) {
  
  if (missing(str)) {
    stop("Argument '", deparse(substitute(str)), "' is required", 
         call. = FALSE)
  }
  
  if (!is.character(str)) {
    stop("Argument '", deparse(substitute(str)), "' must be a character", 
         call. = FALSE)
  }
  
  if (length(str) != 1) {
    stop("Argument '", deparse(substitute(str)), "' must be of length 1", 
         call. = FALSE)
  }
  
  invisible(NULL)
}

