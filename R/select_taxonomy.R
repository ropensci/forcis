#' Select a taxonomy in FORCIS data
#'
#' @description
#' Selects a taxonomy in FORCIS data. FORCIS database provides three different
#' taxonomies: `"LT"` (lumped taxonomy), `"VT"` (validated taxonomy) and `"OT"`
#' (original taxonomy). See \url{https://doi.org/10.1038/s41597-023-02264-2} for
#' further information.
#' 
#' @param data a `data.frame`. One obtained by `read_*_data()` functions.
#'
#' @param taxonomy a `character` of length 1. One among `"LT"`, `"VT"`, `"OT"`.
#' 
#' @export
#'
#' @return A `data.frame`.
#' 
#' @examples
#' \dontrun{
#' # Folder in which the database is stored ----
#' path_to_db <- "data"
#' 
#' # Download and read the plankton nets data ----
#' nets <- forcis::read_plankton_nets_data(path_to_db)
#' 
#' # Select a taxonomy ----
#' nets <- forcis::select_taxonomy(nets, taxonomy = "OT")
#' }

select_taxonomy <- function(data, taxonomy) {
  
  
  ## Check args ----
  
  check_if_df(data)
  check_if_valid_taxonomy(taxonomy)
  
  
  ## Error for CPR North ----
  
  if (get_data_type(data) == "CPR North") {
    stop(paste0("This function cannot be used with CPR North data. There is ",
                "no need to filter these data."), call. = FALSE)
  }
  
  
  ## Remove species from others taxonomies ----
  
  species_to_del <- species_list()[which(species_list()[ , "taxonomy"] != 
                                           toupper(taxonomy)), "taxon"]
  
  pos <- which(colnames(data) %in% species_to_del)
  
  if (length(pos) > 0) {
    data <- data[ , -pos]
  }
  
  data
}