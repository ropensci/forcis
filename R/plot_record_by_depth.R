#' Plot sample record by depth of collection
#'
#' @param data forcis data
#'
#' @return barplot with sample counts by depth interval 
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import rlang
#' @importFrom stats reorder

plot_record_by_depth <- function(data) {
  if (get_data_type(data) %in% c("CPR North","CPR South","Sediment trap","Pump")) {
    stop("This function is designed to work only with Net data", call. = FALSE) 
  }
  plot_dat <- data %>% 
    select(.data$sample_id, .data$sample_min_depth, .data$sample_max_depth) %>%
    unite('sampling_interval', c(.data$sample_min_depth, .data$sample_max_depth),sep = '-', remove = TRUE) %>%
    distinct() %>% 
    group_by(.data$sampling_interval) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(upper_lim=as.numeric(gsub('-.*','',.data$sampling_interval)))
  
  ggplot(plot_dat, aes(reorder(.data$sampling_interval, -.data$upper_lim), .data$n))+
    geom_col(width=0.7, col = 'black')+
    xlab('Sampling Interval') + 
    ylab('Number of Samples') +
    coord_flip()+
    theme_classic() 
  
}