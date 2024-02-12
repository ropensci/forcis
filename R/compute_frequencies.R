#' @rdname computations
#' @export

compute_frequencies <- function(data, aggregate = TRUE) {
  
  ## Check data ----
  
  check_if_not_df(data)
  
  if (get_data_type(data) %in% c("CPR North", "Sediment trap")) {
    stop(paste0("This function is not designed to work with 'CPR North' or ", 
                "'Sediment trap' data"), call. = FALSE) 
  }
  
  check_multiple_taxonomies(data)
  
  
  taxa_cols <- get_species_names(data)
  
  ready_dat <- data %>%
    filter(.data$subsample_count_type == "Relative") %>%    
    pivot_longer(all_of(taxa_cols), 
                 names_to  = 'taxa', 
                 values_to = 'counts') %>% 
    mutate(to_drop = ifelse(is.na(.data$counts), 'drop', 'keep')) %>% 
    filter(.data$to_drop == 'keep') %>% 
    select(-c(.data$to_drop,
              .data$subsample_count_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind,
              .data$sampling_device_type,
              .data$sample_volume_filtered)) 
  
  samples_to_convert <- data$sample_id[
    which(data$subsample_all_shells_present_were_counted == 1)]
  
  list_samples <- unique(samples_to_convert)
  
  conc_to_frequency <- data %>%
    filter(.data$subsample_count_type == "Absolute") %>% 
    pivot_longer(all_of(taxa_cols), 
                 names_to  = 'taxa', 
                 values_to = 'counts') %>% 
    mutate(to_drop = ifelse(is.na(.data$counts), 'drop', 'keep')) %>% 
    filter(.data$to_drop == 'keep') %>% 
    select(-c(.data$to_drop,.data$subsample_count_type)) %>% 
    filter(.data$sample_volume_filtered > 0) %>% 
    mutate(to_drop = ifelse(.data$sample_id %in% list_samples, 'keep', 
                            'drop')) %>% 
    filter(.data$to_drop == 'keep') %>% 
    select(-.data$to_drop) %>% 
    mutate(counts = floor(.data$sample_volume_filtered * .data$counts)) %>% 
    group_by(.data$subsample_id) %>% 
    mutate(tot_subsample = sum(.data$counts)) %>% 
    ungroup() %>% 
    group_by(.data$sample_id) %>% 
    mutate(tot_sample = sum(.data$counts)) %>% 
    ungroup() %>% 
    select(-c(.data$sample_volume_filtered,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind,
              .data$sampling_device_type))
  
  abs_to_frequency <- data %>% 
    filter(.data$subsample_count_type == "Raw") %>% 
    pivot_longer(all_of(taxa_cols), 
                 names_to  = 'taxa', 
                 values_to = 'counts') %>% 
    mutate(to_drop = ifelse(is.na(.data$counts), 'drop', 'keep')) %>% 
    filter(.data$to_drop == 'keep') %>% 
    select(-c(.data$to_drop, .data$subsample_count_type)) %>% 
    mutate(to_drop = ifelse(.data$sample_id %in% list_samples, 'keep', 
                            'drop')) %>% 
    filter(.data$to_drop == 'keep') %>% 
    select(- c(.data$to_drop,
               .data$subsample_all_shells_present_were_counted,
               .data$total_of_forams_counted_ind,
               .data$sampling_device_type,
               .data$sample_volume_filtered)) %>% 
    group_by(.data$subsample_id) %>% 
    mutate(tot_subsample = sum(.data$counts)) %>% 
    ungroup() %>% 
    group_by(.data$sample_id) %>% 
    mutate(tot_sample = sum(.data$counts)) %>% 
    ungroup()
  
  merged_frequency <- rbind(conc_to_frequency, abs_to_frequency)
  
  excluded_samples_volume <- data %>% 
    filter(.data$subsample_count_type == "Absolute") %>% 
    pivot_longer(all_of(taxa_cols), 
                 names_to  = 'taxa', 
                 values_to = 'counts') %>% 
    filter(is.na(.data$sample_volume_filtered))
  
  samples_not_possible__to_convert <- data$sample_id[
    which(data$subsample_all_shells_present_were_counted == 0)]
  
  message("Counts from ", 
          length(unique(excluded_samples_volume$"sample_id")),
          " samples could not be converted because of missing volume data")
  
  message("Counts from ", 
          length(unique(samples_not_possible__to_convert)),
          " samples could not be converted because of missing data on total ",
          "assemblage")
  
  partial_data <- merged_frequency %>% 
    mutate(counts = (.data$counts / .data$tot_subsample) * 100) %>% 
    select(-c(.data$tot_subsample, .data$tot_sample))
  
  tot_dat <- rbind(partial_data, ready_dat)
  
  
  if (!aggregate) {
    
    return(tot_dat)
    
  } else {
    
    partial_data <- merged_frequency %>% 
      mutate(counts = (.data$counts / .data$tot_sample) * 100) %>% 
      select(-c(.data$tot_subsample, .data$tot_sample))
    
    aggregated_dat <- rbind(partial_data, ready_dat) %>% 
      select(-c(.data$subsample_id,
                .data$subsample_size_fraction_min,
                .data$subsample_size_fraction_max)) %>% 
      distinct()
    
    return(aggregated_dat)
  }
}
