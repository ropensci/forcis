#' Get local database version number from an hidden file .forcis
#' 
#' @noRd

get_in_use_version <- function() {
  
  if (file.exists(".forcis")) {
    
    config_file <- readLines(".forcis")
    in_use_version <- gsub("FORCIS_VERSION=", "", config_file)
    
  } else {
    
    in_use_version <- NULL
  }
  
  in_use_version
}



#' Set/update local database version number in an hidden file .forcis
#' 
#' @noRd

set_in_use_version <- function(version) {
  
  saved_version <- get_in_use_version()
  
  if (is.null(saved_version) || saved_version != version) {
    version <- paste0("FORCIS_VERSION=", version)
    cat(version, file = ".forcis", append = FALSE, sep = "\n")
  }
  
  invisible(NULL)
}
