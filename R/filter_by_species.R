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
#' file_name <- system.file(file.path("extdata", "FORCIS_pump_sample.csv"), 
#'                          package = "forcis")
#' 
#' pump_data <- vroom::vroom(file_name, delim = ";", show_col_types = FALSE)
#' 
#' # Add 'data_type' column ----
#' pump_data$"data_type" <- "Pump"
#' 
#' # Select a taxonomy ----
#' pump_data <- select_taxonomy(pump_data, taxonomy = "OT")
#' 
#' # Select only required columns (and taxa) ----
#' pump_data <- select_columns(pump_data)
#' 
#' # Dimensions of the data.frame ----
#' dim(pump_data)
#' 
#' # Convert data into long format ----
#' pump_data <- reshape_data(pump_data)
#' 
#' # Dimensions of the data.frame ----
#' dim(pump_data)
#' 
#' # Total number of species ----
#' length(unique(pump_data$"taxa"))
#' 
#' # Select records for three species ----
#' pump_data_sub <- filter_by_species(data    = pump_data, 
#'                                    species = c("g_inflata", "g_ruber", 
#'                                                "g_glutinata"))
#'
#' # Dimensions of the data.frame ----
#' dim(pump_data_sub)
#' 
#' # Total number of species ----
#' length(unique(pump_data_sub$"taxa"))

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
