#' Plot sample record by season
#'
#' @param data forcis data
#'
#' @return barplot with sample counts by season
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import rlang
#' 
plot_record_by_season <- function(data) {
  if (get_data_type(data)=="Sediment trap"){
    data$sampling_month=as.numeric(sub("^\\d{2}/(\\d{2})/\\d{4}$", "\\1",
                                       data$sample_date_time_start))
    
    data$season = ifelse(data$site_lat_start_decimal > 0 & data$sampling_month %in% c(6, 7, 8, 9),
                         'Summer',
                         ifelse(data$site_lat_start_decimal < 0 & data$sampling_month %in% c(12, 1, 2),
                                'Summer',
                                ifelse(data$site_lat_start_decimal < 0 & data$sampling_month %in% c(6, 7),
                                       'Winter',
                                       ifelse(data$site_lat_start_decimal > 0 & data$sampling_month %in% c(12, 1, 2, 3),
                                              'Winter',
                                              ifelse(data$site_lat_start_decimal > 0 & data$sampling_month %in% c(3, 4, 5),
                                                     'Spring',
                                                     ifelse(data$site_lat_start_decimal > 0 & data$sampling_month %in% c(9, 10, 11),
                                                            'Fall',
                                                            ifelse(data$site_lat_start_decimal < 0 & data$sampling_month %in% c(3, 4, 5),
                                                                   'Fall',
                                                                   ifelse(data$site_lat_start_decimal < 0 & data$sampling_month %in% c(9, 10, 11),
                                                                          'Spring', 'Unknown'))))))))
    
    # Plotting
    ggplot(data %>% 
             select(.data$sample_id, .data$season) %>% 
             distinct(), aes(x = .data$season)) + 
      geom_bar(width = 0.7, col = 'black', stat = "count") +  
      theme_classic() +
      xlab('Season') + 
      ylab('Number of Samples')   
    
  } else {
    data$sampling_month=as.numeric(sub("^\\d{2}/(\\d{2})/\\d{4}$", "\\1", 
                                       data$profile_date_time))
    data$season = ifelse(data$site_lat_start_decimal > 0 & data$sampling_month %in% c(6, 7, 8, 9), 
                         'Summer',
                         ifelse(data$site_lat_start_decimal < 0 & data$sampling_month %in% c(12, 1, 2),
                                'Summer',
                                ifelse(data$site_lat_start_decimal < 0 & data$sampling_month %in% c(6, 7),
                                       'Winter',
                                       ifelse(data$site_lat_start_decimal > 0 & data$sampling_month %in% c(12, 1, 2, 3),
                                              'Winter',
                                              ifelse(data$site_lat_start_decimal > 0 & data$sampling_month %in% c(3, 4, 5),
                                                     'Spring',
                                                     ifelse(data$site_lat_start_decimal > 0 & data$sampling_month %in% c(9, 10, 11),
                                                            'Fall',
                                                            ifelse(data$site_lat_start_decimal < 0 & data$sampling_month %in% c(3, 4, 5),
                                                                   'Fall',
                                                                   ifelse(data$site_lat_start_decimal < 0 & data$sampling_month %in% c(9, 10, 11),
                                                                          'Spring', 'Unknown'))))))))
    
    # Plotting
    ggplot(data %>% 
             select(.data$sample_id,.data$season) %>% 
             distinct(), aes(x = .data$season)) + 
      geom_bar(width = 0.7, col = 'black', stat = "count") +  
      theme_classic() +
      xlab('Season') + 
      ylab('Number of Samples')  
  }
}
