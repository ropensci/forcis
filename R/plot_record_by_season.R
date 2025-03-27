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
#' # Attach the package ----
#' library("forcis")
#' 
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"), 
#'                          package = "forcis")
#' 
#' net_data <- read.table(file_name, dec = ".", sep = ";")
#' 
#' # Plot data by year (example dataset) ----
#' plot_record_by_season(net_data)
 
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
  
  data <- data[ , c("sample_id", "season")]
  data <- data[!duplicated(data), ]
       
  data <- table(data$"season") |> 
    data.frame()
   
  colnames(data) <- c("season", "count")
       
  
  ## Ensure to have all months ----
  
  season <- data.frame("season" = c("Fall", "Winter", "Spring", 
                                    "Summer", "Unknown"))
  
  data <- merge(data, season, by = "season", all = TRUE)
  
  data$"count" <- ifelse(is.na(data$"count"), 0, data$"count")
  
  
  ## Trick for ggplot2 ----
  
  data$"season" <- factor(
    x      = data$"season", 
    levels = c("Fall", "Winter", "Spring", "Summer", "Unknown")
  )
  
  
  ## Plot ----

  ggplot(data, aes(x = .data$season, y = .data$count)) +  
    geom_bar(width = 0.7, col = "black", stat = "identity") + 
    theme_classic() +
    xlab(label = "Season") + 
    ylab(label = "Number of FORCIS samples")   
}
