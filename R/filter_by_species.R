#' Filter FORCIS data by species 
#'
#' @description
#' A short description...
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset, except for CPR North data.
#' 
#' @param species a `character` vector listing species of interest.
#' 
#' @param rm_na a `logical` value. If `FALSE`, keeps taxa with `NA` counts.
#' 
#' @return A `data.frame` containing a subset of `data`.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

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
