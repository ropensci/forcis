#' Plot sample records by season
#' 
#' @description
#' This function produces a barplot of FORCIS sample records by season.
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset.
#'
#' @return A `ggplot` object.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----
 
plot_record_by_season <- function(data) {
  
  ## Check data object ----
  
  check_if_df(data)
  check_field_in_data(data, "sample_id")
  check_field_in_data(data, "site_lat_start_decimal")
  
  
  ## Extract month ----
  
  if (get_data_type(data) == "Sediment trap") {
    
    check_field_in_data(data, "sample_date_time_start")
    data$"sampling_month" <- as.numeric(sub("^\\d{2}/(\\d{2})/\\d{4}$", "\\1",
                                            data$"sample_date_time_start"))
    
  } else {
    
    check_field_in_data(data, "profile_date_time")
    data$"sampling_month" <- as.numeric(sub("^\\d{2}/(\\d{2})/\\d{4}$", "\\1", 
                                            data$"profile_date_time"))
  }
  
    
  ## Identify season ----
  
  data$"season" <- ifelse(data$"site_lat_start_decimal" > 0 & 
                          data$"sampling_month" %in% c(6, 7, 8, 9),
                          "Summer",
                   ifelse(data$"site_lat_start_decimal" < 0 & 
                          data$"sampling_month" %in% c(12, 1, 2),
                          "Summer",
                   ifelse(data$"site_lat_start_decimal" < 0 & 
                          data$"sampling_month" %in% c(6, 7),
                          "Winter",
                   ifelse(data$"site_lat_start_decimal" > 0 & 
                          data$"sampling_month" %in% c(12, 1, 2, 3),
                          "Winter",
                   ifelse(data$"site_lat_start_decimal" > 0 & 
                          data$"sampling_month" %in% c(3, 4, 5),
                          "Spring",
                   ifelse(data$"site_lat_start_decimal" > 0 & 
                          data$"sampling_month" %in% c(9, 10, 11),
                          "Fall",
                   ifelse(data$"site_lat_start_decimal" < 0 & 
                          data$"sampling_month" %in% c(3, 4, 5),
                          "Fall",
                   ifelse(data$"site_lat_start_decimal" < 0 & 
                          data$"sampling_month" %in% c(9, 10, 11),
                          "Spring", 
                   "Unknown"))))))))
    
  
  ## Get distinct values ----
  
  data <- data %>% 
    select(.data$sample_id, .data$season) %>% 
    distinct()
  
  data$season <-factor(data$season, levels = c("Fall", "Winter", "Spring", "Summer", "Unknown"))
  
  
  ## Plot ----

  ggplot(data, aes(x = .data$season)) + 
    geom_bar(width = 0.7, col = "black", stat = "count") +  
    theme_classic() +
    xlab("Season") + 
    ylab("Number of FORCIS samples")   
}
