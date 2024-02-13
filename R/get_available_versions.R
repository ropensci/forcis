#' Retrieve all versions of a Zenodo repository
#'
#' @return A `data.frame` with three columns:
#'   - `publication_date`: the date of the release of the version
#'   - `version`: the label of the version
#'   - `access_right`: is the version open or restricted?
#'   
#' @export
#'
#' @examples
#' ## Versions of the FORCIS database ----
#' 
#' get_available_versions()

get_available_versions <- function() {
  

  ## Retrieve information ----
  
  res <- jsonlite::read_json(path = paste0("https://zenodo.org/api/records/", 
                                           "?q=conceptrecid:", zenodo_id(), 
                                           "&all_versions=true"),
                             simplifyVector = TRUE)
  
  if (res$"hits"$"total" == 0) {
    stop("No information available for the Zenodo record '", zenodo_id(), "'", 
         call. = FALSE)
  }
  
  meta <- res$"hits"$"hits"$"metadata"
  
  
  ## Clean output ----
  
  meta <- data.frame("publication_date" = meta$"publication_date",
                     "version"          = meta$"version",
                     "access_right"    = meta$"access_right")
  
  meta <- meta[order(as.Date(meta$"publication_date"), decreasing = TRUE), ]
  
  rownames(meta) <- NULL
  
  meta
}
