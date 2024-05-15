#' Get the version of the FORCIS database currently used
#' 
#' @description
#' Returns the version of the FORCIS database currently used in the project. 
#' This function will read the content of the hidden file `.forcis` created by
#' the function `download_forcis_db()`. This file keeps track of the latest
#' version of the database used for a dedicated project. For more information, 
#' please read the vignette available at
#' \url{https://frbcesab.github.io/forcis/articles/database-versions.html}.
#'
#' @return A `character` of length 1, i.e. the label of the version in use.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Attach the package ----
#' library("forcis")
#' 
#' # Folder in which the database will be saved ----
#' path_to_save_db <- "data"
#' 
#' # Download the database ----
#' download_forcis_db(path = path_to_save_db, version = NULL)
#' 
#' # Get the version of the database ----
#' get_current_version()
#' }

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
