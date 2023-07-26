#' Select columns in FORCIS data
#'
#' @description
#' Selects columns in FORCIS data. Because FORCIS data contains more than 100 
#' columns, this function can be used to lighten the `data.frame` to easily 
#' handle it and to speed up some computations.
#' 
#' @param data a `data.frame`. One obtained by `get_*_data()` functions.
#'
#' @param cols a `character` vector of column names to keep in addition to the 
#'   required ones (see [get_required_columns()]) and to the taxa columns.
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
#' nets <- forcis::get_plankton_nets_data(path_to_db)
#' 
#' # Select a taxonomy ----
#' nets <- forcis::select_taxonomy(nets, taxonomy = "OT")
#' 
#' # Select required columns (and taxa) ----
#' nets <- forcis::select_columns(nets)
#' }

select_columns <- function(data, cols = NULL) {
  
  
  # Check args ----
  
  check_if_not_df(data)
  
  if (!is.null(cols)) {
    if (!is.character(cols)) {
      stop("Argument 'cols' must be a character vector", call. = FALSE)
    }
  }
  
  
  # Check for missing columns ----
  
  check_required_columns(data)
  
  if (!is.null(cols)) {
    if (any(!(cols %in% colnames(data)))) {
      stop("Some columns to select are absent from data", call. = FALSE)
    }
  }
  
  
  # Extract species columns ----
  
  if (get_data_type(data) != "CPR North") {
    
    species_cols <- get_species_names(data) 
    
  } else {
    
    species_cols <- c("species", "count_bin_min", "count_bin_max")
  }
  
  
  # Subset columns ----
  
  if (is.null(cols)) {
    
    data <- data[ , c(required_columns(), species_cols)]
    
  } else {
    
    data <- data[ , c(required_columns(), cols, species_cols)]
  }
  
  data
}



#' Select a taxonomy in FORCIS data
#'
#' @description
#' Selects a taxonomy in FORCIS data. FORCIS database provides three different
#' taxonomies: `"LT"` (lumped taxonomy), `"VT"` (validated taxonomy) and `"OT"`
#' (original taxonomy). See \url{https://doi.org/10.1038/s41597-023-02264-2} for
#' further information.
#' 
#' @param data a `data.frame`. One obtained by `get_*_data()` functions.
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
#' nets <- forcis::get_plankton_nets_data(path_to_db)
#' 
#' # Select a taxonomy ----
#' nets <- forcis::select_taxonomy(nets, taxonomy = "OT")
#' }

select_taxonomy <- function(data, taxonomy) {
  
  
  ## Check args ----
  
  check_if_not_df(data)
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



#' Get species names from column names
#'
#' @description
#' Gets species names from column names. This function is just an utility to
#' easily retrieve taxon names.
#' 
#' @param data a `data.frame`. One obtained by `get_*_data()` functions.
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
#' nets <- forcis::get_plankton_nets_data(path_to_db)
#' 
#' # Select a taxonomy ----
#' nets <- forcis::select_taxonomy(nets, taxonomy = "OT")
#' 
#' # Retrieve taxon names ----
#' forcis::get_species_names(nets)
#' }

get_species_names <- function(data) {
  
  check_if_not_df(data)
  
  
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



#' Get required column names
#'
#' @description
#' Gets required column names (except taxa names) for the package. This 
#' function is designed to help users to add additional columns in 
#' [select_columns()] (argument `cols`) if missing from this list.
#' 
#' @export
#'
#' @return A `character`.
#' 
#' @examples
#' \dontrun{
#' # Get required column names (expect taxa names) ----
#' forcis::get_required_columns()
#' }

get_required_columns <- function() {
  
  required_columns()
}
