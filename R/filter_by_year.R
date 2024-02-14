#' Filter FORCIS data by year of sampling
#'
#' @description
#' This function can be used to filter FORCIS data by year of sampling.
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset.
#' 
#' @param years a `numeric` containing one or several years.
#'
#' @return A `data.frame` containing a subset of `data` for the desired years.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

filter_by_year <- function(data, years) {
  
  ## Check data object ----
  
  check_if_df(data)
  
  
  ## Check years object ----
  
  if (missing(years)) {
    stop("Argument 'years' is required", call. = FALSE)
  }
  
  if (!is.numeric(years)) {
    stop("Argument 'years' must be a numeric of length >= 1", call. = FALSE)
  }
  
  
  if (get_data_type(data) == "Sediment trap") {
    
    check_field_in_data(data, "sample_date_time_start")
    
    data <- data[!is.na(data$"sample_date_time_start"), ]
    
    start_dates <- unlist(lapply(strsplit(data$"sample_date_time_start", "\\s"),
                                 function(x) x[1]))
    
    start_dates <- as.Date(start_dates, format = date_format())
    start_years <- as.numeric(format(start_dates, "%Y"))
    
    if (all(!(as.numeric(years) %in% unique(start_years)))) {
      stop("The years provided are out of FORCIS temporal range", call. = FALSE)
    }
    
    pos <- which(start_years %in% as.numeric(years))
    
    data <- data[pos, ]
    
  } else {
    
    check_field_in_data(data, "profile_date_time")
    
    data <- data[!is.na(data$"profile_date_time"), ]
    
    start_dates <- as.Date(start_dates, format = date_format())
    start_years <- as.numeric(format(start_dates, "%Y"))
    
    if (all(!(as.numeric(years) %in% unique(start_years)))) {
      stop("The years provided are out of FORCIS temporal range", call. = FALSE)
    }
    
    pos <- which(start_years %in% as.numeric(years))
    
    data <- data[pos, ]
  }
  
  data
}
