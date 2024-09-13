#' Plot sample records by depth of collection
#'
#' @description
#' This function produces a barplot of FORCIS sample records by depth.
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
#' plot_record_by_depth(net_data)

plot_record_by_depth <- function(data) {
  
  ## Check data object ----
  
  check_if_df(data)
  
  if (get_data_type(data) != "Net") {
    stop("This function is designed to work only with Net data", call. = FALSE) 
  }
  
  check_field_in_data(data, "sample_id")
  check_field_in_data(data, "sample_min_depth")
  check_field_in_data(data, "sample_max_depth")
  
  
  ## Prepare data ----
  
  data <- data %>% 
    select(.data$sample_id, .data$sample_min_depth, .data$sample_max_depth) %>%
    mutate(sampling_interval = ifelse(
      is.na(.data$sample_min_depth), 'Unknown', 
      ifelse(.data$sample_min_depth <=5, 'from Surface',
             ifelse(.data$sample_min_depth <= 100, 'from first 100m',
                    ifelse(.data$sample_min_depth > 100 & .data$sample_min_depth <= 300, 'from 100m-300m',
                           ifelse(.data$sample_min_depth > 300 & .data$sample_min_depth <= 500, 'from 300m-500m',
                                  ifelse(.data$sample_min_depth >= 500, 'from below 500m', NA))))))) %>% 
    select(-c(.data$sample_min_depth, .data$sample_max_depth)) %>%
    distinct() %>% 
    group_by(.data$sampling_interval) %>% 
    count() %>% 
    ungroup() 
  
  data$"sampling_interval" <- factor(data$"sampling_interval",
                                       levels = c('Unknown',
                                                  'from below 500m',
                                                  'from 300m-500m',
                                                  'from 100m-300m',
                                                  'from first 100m',
                                                  'from Surface'))

    
  ggplot(data, 
         aes(.data$sampling_interval, .data$n)) +
    geom_col(width = 0.7, col = "black") +
    xlab("Sampling interval") + 
    ylab("Number of FORCIS samples") +
    coord_flip() +
    theme_classic() 
}
