#' Select columns in FORCIS data
#'
#' @description
#' Selects columns in FORCIS data. Because FORCIS data contains more than 100 
#' columns, this function can be used to lighten the `data.frame` to easily 
#' handle it and to speed up some computations.
#' 
#' @param data a `data.frame`. One obtained by `read_*_data()` functions.
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
#' nets <- forcis::read_plankton_nets_data(path_to_db)
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
