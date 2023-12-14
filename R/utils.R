#' Date format used in raw data
#' 
#' @noRd

date_format <- function() "%d/%m/%Y"



#' Retrieve data type
#' 
#' @noRd

get_data_type <- function(data) {
  
  check_if_not_df(data)
  
  if (!("data_type" %in% colnames(data))) {
    stop(paste0("The column 'data_type' is absent from 'data'. Did you use ", 
                "the functions 'get_*_data()' to import data?"),
         call. = FALSE)
  }
  
  
  data_type <- unique(data$"data_type")
  
  if (length(data_type) != 1) {
    stop("The column 'data_type' cannot contain different values", 
         call. = FALSE)
  }
  
  data_type
}



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



#' Check for required columns
#' 
#' @noRd

check_required_columns <- function(data) {
  
  check_if_not_df(data)
  
  if (any(!(required_columns() %in% colnames(data)))) {
    stop("Some required columns are absent from data", call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check if a taxonomy name is valid
#' 
#' @noRd

check_if_valid_taxonomy <- function(taxonomy) {
  
  is_character(taxonomy)
  taxonomy   <- tolower(taxonomy)
  
  taxonomies <- unique(species_list()[ , "taxonomy"])
  taxonomies <- tolower(taxonomies)
  
  if (!(taxonomy %in% taxonomies)) {
    stop("Bad taxonomy. Valid taxonomies names are: ",
         toString(toupper(taxonomies)), call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check for multiple taxonomies
#' 
#' @noRd

check_multiple_taxonomies <- function(data) {
  
  if (get_data_type(data) != "CPR North") {
    
    check_required_columns(data)
    
    pos <- which(species_list()[ , "taxon"] %in% colnames(data))
    
    if (length(pos) > 0) { 
      
      taxonomy <- unique(species_list()[pos, "taxonomy"])
      
      if (length(taxonomy) > 1) {
        stop("Multiple taxonomies are not allowed. Please use the function ", 
             "'select_taxonomy()' before going any further", call. = FALSE)
      }
    } 
  }
  
  invisible(NULL)
}



#' Check Zenodo version
#' 
#' @noRd

check_zen_version <- function(version) {
  
  
  if (!is.null(version)) {
    
    if (!is.character(version)) {
      stop("Argument 'version' must be character", call. = FALSE)
    }
    
    if (length(version) != 1) {
      stop("Argument 'version' must be character of length 1", call. = FALSE)
    }
  }
  
  invisible(NULL)
}



#' Set Zenodo version to latest is missing
#' 
#' @noRd

set_zen_version <- function(version) {
  
  check_zen_version(version)
  
  versions <- zen_list_versions()
  
  if (is.null(version)) {
    
    version <- versions[which.max(as.Date(versions$"publication_date")), 
                        "version"]
    
  } else {
    
    if (!(version %in% versions$"version")) {
      
      stop("The required version is not available. Please run ", 
           "`zen_list_versions()` to list available versions.", call. = FALSE)
    }
  }
  
  version
}
