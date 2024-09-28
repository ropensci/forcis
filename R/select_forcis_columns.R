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
#'   required ones (see [get_required_columns()]) and to the taxa columns. Can 
#'   be `NULL` (default).
#' 
#' @export
#'
#' @return A `data.frame`.
#' 
#' @examples
#' # Attach the package ----
#' library("forcis")
#' 
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"), 
#'                          package = "forcis")
#' 
#' net_data <- read.table(file_name, dec = ".", sep = ";")
#' 
#' # Add 'data_type' column ----
#' net_data$"data_type" <- "Net"
#' 
#' # Dimensions of the data.frame ----
#' dim(net_data)
#' 
#' # Select a taxonomy ----
#' net_data <- select_taxonomy(net_data, taxonomy = "VT")
#' 
#' # Dimensions of the data.frame ----
#' dim(net_data)
#' 
#' # Select only required columns (and taxa) ----
#' net_data <- select_forcis_columns(net_data)
#' 
#' # Dimensions of the data.frame ----
#' dim(net_data)

select_forcis_columns <- function(data, cols = NULL) {
  
  
  # Check args ----
  
  check_if_df(data)
  
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
    
    data <- data[ , c(get_required_columns(), species_cols)]
    
  } else {
    
    data <- data[ , c(get_required_columns(), cols, species_cols)]
  }
  
  data
}
