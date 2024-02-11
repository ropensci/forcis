#' Filter FORCIS data by month of sampling
#'
#' @description
#' This function can be used to filter FORCIS data by month of sampling.
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset.
#' 
#' @param months a `numeric` containing one or several months.
#'
#' @return A `data.frame` containing a subset of `data` for the desired months.
#' 
#' @export
#' 
#' @examples
#' ## ADD EXAMPLE ----

filter_by_month <- function(data, months) {
  
  ## Check data object ----
  
  check_if_not_df(data)
  
  
  ## Check months object ----
  
  if (missing(months)) {
    stop("Argument 'months' is required", call. = FALSE)
  }
  
  if (!is.numeric(months)) {
    stop("Argument 'months' must be a numeric of length >= 1", call. = FALSE)
  }
  
  
  if (get_data_type(data) == "Sediment trap") {
    
    check_field_in_data(data, "sample_date_time_start")
    
    data <- data[!is.na(data$"sample_date_time_start"), ]
    
    start_dates <- unlist(lapply(strsplit(data$"sample_date_time_start", "\\s"),
                                 function(x) x[1]))
    
    start_dates  <- as.Date(start_dates, format = "%d/%m/%Y")
    start_months <- as.numeric(format(start_dates, "%m"))
    
    pos <- which(start_months %in% as.numeric(months))
    
    data <- data[pos, ]
    
  } else {
    
    check_field_in_data(data, "profile_date_time")
    
    data <- data[!is.na(data$"profile_date_time"), ]
    
    start_dates  <- as.Date(start_dates, format = "%d/%m/%Y")
    start_months <- as.numeric(format(start_dates, "%m"))
    
    pos <- which(start_months %in% as.numeric(months))
    
    data <- data[pos, ]
  }
  
  data
}
