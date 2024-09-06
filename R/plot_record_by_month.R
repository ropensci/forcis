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
#' # Plot data by year (example dataset) ----
#' plot_record_by_month(net_data)

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
  
  
  ## Get distinct values & count ----
  
  data <- data %>% 
    select(.data$sample_id, .data$sampling_month) %>%
    group_by(.data$sampling_month) %>%
    summarise(count = n_distinct(.data$sample_id))
  
  
  ## Ensure to have all months ----
  
  sampling_month <- data.frame("sampling_month" = 1:12)
  
  data <- merge(data, sampling_month, by = "sampling_month", all = TRUE)
  
  data$"count" <- replace_na(data$"count", 0)
  
  
  ## Trick for ggplot2 ----
  
  data$"sampling_month" <- factor(data$"sampling_month", levels = 1:12)
  
  
  ## Plot ----
  
  ggplot(data, aes(x = .data$sampling_month, y = .data$count)) +  
    geom_bar(width = 0.7, col = "black", stat = "identity") + 
    theme_classic() +
    xlab("Month") +  
    ylab("Number of FORCIS samples")
}
