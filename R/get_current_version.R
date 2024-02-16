#' Get the version of the database currently used
#' 
#' @description
#' This function returns the version of the database currently used in the 
#' project. 
#'
#' @return A `character` of length 1.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

get_current_version <- function() {
  
  if (file.exists(".forcis")) {
    
    config_file <- readLines(".forcis")
    in_use_version <- gsub("FORCIS_VERSION=", "", config_file)
    
    if (length(in_use_version) == 0) { # Empty file
      in_use_version <- NULL
    }
    
  } else {
    
    in_use_version <- NULL
  }
  
  in_use_version
}
