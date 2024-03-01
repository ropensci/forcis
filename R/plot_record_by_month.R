#' Plot sample record by month
#'
#' @param data forcis data
#'
#' @return barplot with sample counts by month
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import rlang

plot_record_by_month <- function(data) {
  if (get_data_type(data)=="Sediment trap"){
    data$sampling_month=as.numeric(sub("^\\d{2}/(\\d{2})/\\d{4}$", "\\1",
                                       data$sample_date_time_start))
    # Plotting
    ggplot(data %>% 
             select(.data$sample_id, .data$sampling_month) %>% 
             distinct(), aes(x = .data$sampling_month)) +  
      geom_bar(width = 0.7, col = 'black', stat = "count") + 
      theme_classic() +
      xlab('Month') +  
      ylab('Number of Samples')  
    
  } else {
    data$sampling_month=as.numeric(sub("^\\d{2}/(\\d{2})/\\d{4}$", "\\1",
                                       data$profile_date_time))
    
    # Plotting
    ggplot(data %>% 
             select(.data$sample_id, .data$sampling_month) %>% 
             distinct(), aes(x = .data$sampling_month)) +  
      geom_bar(width = 0.7, col = 'black', stat = "count") +  
      theme_classic() +
      xlab('Month') +  
      ylab('Number of Samples')  
  }
}
