#' Print information of a specific version of the FORCIS database
#' 
#' @description
#' Prints information of a specific version of the FORCIS database by querying 
#' the Zenodo API (\url{https://developers.zenodo.org}).
#'   
#' @param version a `character` of length 1. The label of the version. Use 
#'   [get_available_versions()] to list available versions. If `NULL` (default)
#'   the latest version is used.
#'
#' @return A `list` with all information about the version, including: `title`,
#' `doi`, `publication_date`, `description`, `access_right`, `creators`, 
#' `keywords`, `version`, `resource_type`, `license`, and `files`.
#'   
#' @export
#'
#' @examples
#' # Attach the package ----
#' library("forcis")
#' 
#' # Get information for the latest version of the FORCIS database ----
#' get_version_metadata()

get_version_metadata <- function(version = NULL) {
  
  ## Check arguments ----
  
  check_version(version)
  
  
  ## Retrieve information ----
  
  res <- get_metadata()
  
  meta  <- res$"hits"$"hits"$"metadata"
  files <- res$"hits"$"hits"$"files"
  
  
  ## Subset version ----
  
  if (is.null(version)) {
    
    pos <- which.max(as.Date(meta$"publication_date"))
    
  } else {
    
    pos <- which(meta$"version" == version)
    
    if (length(pos) == 0) {
      stop("The required version is not available. Please run ", 
           "'get_available_versions()' to list available versions.")
    }
  }
  
  meta  <- as.list(meta[pos, ])
  files <- files[[pos]]
  
  
  ## Clean output ----
  
  pos <- which(names(meta) == "relations")
  
  if (length(pos) > 0) {
    meta <- meta[-pos]
  }
  
  pos <- which(names(meta) == "dates")
  
  if (length(pos) > 0) {
    meta <- meta[-pos]
  }
  
  meta$"resource_type" <- meta$"resource_type"$"type"
  
  meta$"files" <- files
  
  meta
}
