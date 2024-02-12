#' @rdname computations
#' @export

compute_concentration_bins_CPR_n <- function(data) {
  
  ## Check data ----
  
  check_if_not_df(data)
  
  if (get_data_type(data)!="CPR North") {
    stop(paste0("This function is designed to work only with CPR North data"), 
         call. = FALSE) 
  }
  
  tot_dat <- data %>% 
    mutate(min_conc_binned = .data$count_bin_min / .data$sample_volume_filtered,
           max_conc_binned = .data$count_bin_max / .data$sample_volume_filtered) %>% 
    select(-c(.data$count_bin_min,
              .data$count_bin_max))
  
  tot_dat
}
