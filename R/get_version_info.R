#' Print information of a specific version of a Zenodo repository
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
#' ## Get information for the latest version of the FORCIS database ----
#' 
#' get_version_info()

get_version_info <- function(version = NULL) {
  
  ## Check arguments ----
  
  check_zen_version(version)
  
  
  ## Retrieve information ----
  
  res <- jsonlite::read_json(path = paste0("https://zenodo.org/api/records/", 
                                           "?q=conceptrecid:", zenodo_id(), 
                                           "&all_versions=true"),
                             simplifyVector = TRUE)
  
  if (res$"hits"$"total" == 0) {
    stop("No information available for the Zenodo record '", zenodo_id(), "'", 
         call. = FALSE)
  }
  
  
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
