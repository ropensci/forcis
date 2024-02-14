#' Get species names from column names
#'
#' @description
#' Gets species names from column names. This function is just an utility to
#' easily retrieve taxon names.
#' 
#' @param data a `data.frame`. One obtained by `read_*_data()` functions.
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
#' 
#' # Retrieve taxon names ----
#' forcis::get_species_names(nets)
#' }

get_species_names <- function(data) {
  
  check_if_df(data)
  
  
  ## Special case for CPR North ----
  
  if (get_data_type(data) == "CPR North") {
    
    species_names <- unique(data$"species")
    
    
    ## Otherwise ----
    
  } else {
    
    species_names <- colnames(data)[which(colnames(data) %in% 
                                            species_list()[ , "taxon"])]
  }
  
  species_names
}
