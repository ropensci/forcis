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
