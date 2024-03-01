#' Plot sample record by year
#'
#' @param data forcis data
#'
#' @return barplot with sample counts by year
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import rlang

plot_record_by_year <- function(data) {
  if (get_data_type(data)=="Sediment trap"){
    data$sampling_year = as.numeric(sub("^\\d{2}/\\d{2}/(\\d{4})$", "\\1",
                                        data$sample_date_time_start))
    # Plotting
    ggplot(data %>% 
             select(.data$sample_id, .data$sampling_year) %>% 
             distinct(), aes(x = .data$sampling_year)) +  
      geom_bar(width = 0.7, col = 'black', stat = "count") + 
      theme_classic() +
      xlab('Year') +  
      ylab('Number of Samples')  
    
  } else {
    data$sampling_year = as.numeric(sub("^\\d{2}/\\d{2}/(\\d{4})$", "\\1", 
                                        data$profile_date_time))
    # Plotting
    ggplot(data %>%
             select(.data$sample_id, .data$sampling_year) %>%
             distinct(), aes(x = .data$sampling_year)) +
      geom_bar(width = 0.7, col = 'black', stat = "count") +
      theme_classic() +
      xlab('Year') +
      ylab('Number of Samples')  
  }
}
