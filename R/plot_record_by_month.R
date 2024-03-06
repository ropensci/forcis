#' Plot sample records by month
#'
#' @description
#' This function produces a barplot of FORCIS sample records by month.
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset.
#'
#' @return A `ggplot` object.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

plot_record_by_month <- function(data) {
  
  ## Check data object ----
  
  check_if_df(data)
  check_field_in_data(data, "sample_id")
  
  
  ## Extract year ----
  
  if (get_data_type(data) == "Sediment trap") {
    
    check_field_in_data(data, "sample_date_time_start")
    
    data$"sampling_month" <- as.numeric(sub("^\\d{2}/(\\d{2})/\\d{4}$", "\\1",
                                            data$"sample_date_time_start"))
    
  } else {
    
    check_field_in_data(data, "profile_date_time")
    
    data$"sampling_month" <- as.numeric(sub("^\\d{2}/(\\d{2})/\\d{4}$", "\\1", 
                                            data$"profile_date_time"))
  }
  
  
  ## Get distinct values ----
  
  data <- data %>% 
    select(.data$sample_id, .data$sampling_month) %>% 
    distinct()
  

  ## Plot ----
  
  ggplot(data, aes(x = .data$sampling_month)) +  
    geom_bar(width = 0.7, col = "black", stat = "count") + 
    theme_classic() +
    xlab("Month") +  
    ylab("Number of FORCIS samples")
}
