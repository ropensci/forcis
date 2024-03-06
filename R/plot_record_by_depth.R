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
#' ## ADD EXAMPLE ----

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
    unite('sampling_interval', c(.data$sample_min_depth, 
                                 .data$sample_max_depth), sep = '-', 
          remove = TRUE) %>%
    distinct() %>% 
    group_by(.data$sampling_interval) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(upper_lim = as.numeric(gsub("-.*", "", .data$sampling_interval)))
  
  ggplot(data, 
         aes(stats::reorder(.data$sampling_interval, -.data$upper_lim), 
             .data$n)) +
    geom_col(width = 0.7, col = "black") +
    xlab("Sampling interval") + 
    ylab("Number of FORCIS samples") +
    coord_flip() +
    theme_classic() 
}
