#' @rdname computations
#' @export

compute_abundances <- function(data, aggregate = TRUE) {
  
  ## Check data ----
  
  check_if_df(data)
  
  if (get_data_type(data) %in% c("CPR North", "Sediment trap")) {
    stop(paste0("This function is not designed to work with 'CPR North' or ", 
                "'Sediment trap' data"), call. = FALSE) 
  }
  
  check_unique_taxonomy(data)
  
  
  taxa_cols <- get_species_names(data)
  
  ready_dat <- data %>%
    filter(.data$subsample_count_type == "Raw") %>%  
    pivot_longer(all_of(taxa_cols), 
                 names_to  = 'taxa', 
                 values_to = 'counts') %>% 
    mutate(to_drop = ifelse(is.na(.data$counts), 'drop', 'keep')) %>% 
    filter(.data$to_drop == 'keep')%>%
    select(-c(.data$to_drop,
              .data$subsample_count_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind,
              .data$sampling_device_type)) %>% 
    rename('counts_raw_ab' = 'counts')
  
  conc_data_to_convert <- data %>%
    filter(.data$sample_volume_filtered > 0) %>% 
    filter(.data$subsample_count_type == 'Absolute') %>%   
    pivot_longer(all_of(taxa_cols), 
                 names_to  = 'taxa', 
                 values_to = 'counts') %>%
    mutate(to_drop = ifelse(is.na(.data$counts), 'drop', 'keep')) %>% 
    filter(.data$to_drop == 'keep') %>% 
    select(-c(.data$to_drop,
              .data$sampling_device_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind)) %>% 
    mutate(new_counts = floor(.data$counts * .data$sample_volume_filtered)) %>% 
    select(-c(.data$counts, .data$subsample_count_type)) %>% 
    rename('counts_raw_ab' = 'new_counts') %>% 
    distinct()
  
  rel_data_to_convert <- data %>%
    filter(.data$sample_volume_filtered > 0) %>% 
    filter(.data$subsample_count_type == 'Relative') %>%    
    pivot_longer(all_of(taxa_cols), 
                 names_to  = 'taxa', 
                 values_to = 'counts') %>%
    mutate(to_drop = ifelse(is.na(.data$counts), 'drop', 'keep')) %>% 
    filter(.data$to_drop == 'keep') %>% 
    select(-.data$to_drop) %>% 
    filter(.data$subsample_all_shells_present_were_counted == 1) %>% 
    filter(!is.na(.data$total_of_forams_counted_ind)) %>% 
    mutate(new_counts = floor((.data$counts * 
                                 .data$total_of_forams_counted_ind) / 100)) %>% 
    select(-c(.data$counts,
              .data$subsample_count_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind,
              .data$sampling_device_type)) %>% 
    rename('counts_raw_ab' = 'new_counts') %>% 
    distinct()
  
  excluded_samples_volume <- data %>%
    filter(.data$subsample_count_type != "Raw") %>%
    filter(is.na(.data$sample_volume_filtered))
  
  excluded_samples_missing_counts <- data %>%
    filter(.data$sample_volume_filtered > 0) %>% 
    filter(.data$subsample_count_type == 'Relative') %>%    
    pivot_longer(all_of(taxa_cols), 
                 names_to  = 'taxa', 
                 values_to = 'counts') %>%
    mutate(to_drop = ifelse(is.na(.data$counts), 'drop', 'keep')) %>% 
    filter(.data$to_drop == 'keep') %>% 
    select(-.data$to_drop) %>%  
    filter(is.na(.data$total_of_forams_counted_ind))
  
  message("Counts from ", 
          length(unique(excluded_samples_volume$"sample_id")),
          " samples could not be converted because of missing volume data")
  
  message("Relative counts from ", 
          length(unique(excluded_samples_missing_counts$"sample_id")),
          " samples could not be converted because of missing data on total ",
          "assemblage")
  
  tot_dat <- rbind(ready_dat, conc_data_to_convert, rel_data_to_convert)
  
  if (aggregate) {
    
    tot_dat <- tot_dat %>% 
      group_by(.data$sample_id, .data$taxa) %>% 
      mutate(new_counts = sum(.data$counts_raw_ab, na.rm = TRUE)) %>% 
      ungroup() %>% 
      select(-c(.data$counts_raw_ab, .data$subsample_id,
                .data$subsample_size_fraction_min,
                .data$subsample_size_fraction_max)) %>%
      distinct() %>% 
      rename('counts_raw_ab' = 'new_counts')
  }
  
  tot_dat
}
