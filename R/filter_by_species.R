#' Filter FORCIS data by species 
#'
#' @description
#' Filters FORCIS data by a species list.
#' 
#' @param data a `data.frame`. One obtained by `read_*_data()` functions.
#' 
#' @param species a `character` vector listing species of interest.
#' 
#' @return A `data.frame` containing a subset of `data`.
#' 
#' @export
#'
#' @examples
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"), 
#'                          package = "forcis")
#' 
#' net_data <- read.table(file_name, dec = ".", sep = ";")
#' 
#' # Add 'data_type' column ----
#' net_data$"data_type" <- "Net"
#' 
#' # Select a taxonomy ----
#' net_data <- select_taxonomy(net_data, taxonomy = "VT")
#' 
#' # Select only required columns (and taxa) ----
#' net_data <- select_forcis_columns(net_data)
#' 
#' # Dimensions of the data.frame ----
#' dim(net_data)
#' 
#' # Get species names ----
#' get_species_names(net_data)
#' 
#' # Select records for three species ----
#' net_data_sub <- filter_by_species(data    = net_data, 
#'                                   species = c("g_inflata_VT", 
#'                                               "g_elongatus_VT", 
#'                                               "g_glutinata_VT"))
#'
#' # Dimensions of the data.frame ----
#' dim(net_data_sub)
#' 
#' # Get species names ----
#' get_species_names(net_data_sub)

filter_by_species <- function(data, species) {
  
  ## Check data object ----
  
  check_if_df(data)
  
  if (get_data_type(data) == "CPR North") {
    stop("This function is not designed to work with 'CPR North' data", 
         call. = FALSE) 
  }
  
  if (length(grep("^taxa$", colnames(data))) == 1) {
    stop("This function is not designed to work on long format data.frame", 
         call. = FALSE) 
  }
  
  if (length(get_species_names(data)) == 0) {
    stop("No species columns detected", call. = FALSE) 
  }
  
  
  ## Check species object ----
  
  if (missing(species)) {
    stop("Argument 'species' is required", call. = FALSE)
  }
  
  if (!is.character(species)) {
    stop("Argument 'species' must be a character of length >= 1", call. = FALSE)
  }


  if (all(!(species %in% get_species_names(data)))) {
    stop("The species provided are absent from 'data'", call. = FALSE)
  }
  
  
  ## Filter by species ----
  
  species_to_del <- get_species_names(data)[!(get_species_names(data) %in% 
                                                species)]
  
  if (length(species_to_del) > 0) {
    data <- data[ , -which(colnames(data) %in% species_to_del), drop = FALSE]
  }
  
  data
}
