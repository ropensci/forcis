#' Filter FORCIS data by species 
#'
#' @description
#' Filters FORCIS data by a species list.
#' 
#' @param data a `data.frame`. Must be the output of the function 
#'   `reshape_data()`, i.e. a FORCIS dataset in long format.
#' 
#' @param species a `character` vector listing species of interest.
#' 
#' @param rm_na a `logical` value. If `FALSE` (default), keeps taxa with `NA` 
#'   counts.
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
#' net_data <- select_columns(net_data)
#' 
#' # Dimensions of the data.frame ----
#' dim(net_data)
#' 
#' # Convert data into long format ----
#' net_data <- reshape_data(net_data)
#' 
#' # Dimensions of the data.frame ----
#' dim(net_data)
#' 
#' # Total number of species ----
#' length(unique(net_data$"taxa"))
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
#' # Total number of species ----
#' length(unique(net_data_sub$"taxa"))

filter_by_species <- function(data, species, rm_na = FALSE) {
  
  ## Check data object ----
  
  check_if_df(data)
  
  if (get_data_type(data) == "CPR North") {
    stop("This function is not designed to work with 'CPR North' data", 
         call. = FALSE) 
  }
  
  if (length(get_species_names(data)) > 0) {
    stop("This function requires data in long format. Please use the function ",
         "'reshape_data()'", call. = FALSE) 
  }
  
  
  ## Check species object ----
  
  if (missing(species)) {
    stop("Argument 'species' is required", call. = FALSE)
  }
  
  if (!is.character(species)) {
    stop("Argument 'species' must be a character of length >= 1", call. = FALSE)
  }


  if (all(!(species %in% unique(data$"taxa")))) {
    stop("The species provided are absent from 'data'", call. = FALSE)
  }
  
  
  ## Filter by species ----
  
  data <- data[data$"taxa" %in% species, ]
  
  
  ## Remove lines w/ NA counts (if required) ----
  
  if (rm_na) {
    count_columns <- grepl("counts", names(data))
    #data <- data[!is.na(data$"counts"), ]
    data <- data[!is.na(data[, count_columns]), ]
    
  }
  
  data
}
