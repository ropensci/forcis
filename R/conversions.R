#' Compute count conversions
#'
#' @description
#' __ADD DESCRIPTION__
#'
#' @param data a `data.frame`. One obtained by `get_*_data()` functions.
#' 
#' @param aggregate a `logical` of length 1. If `FALSE` counts will be derived
#'   for each subsample. If `TRUE` (default) subsample counts will be 
#'   aggregated by `sample_id`.
#'
#' @return A `data.frame`.
#' 
#' @details
#' 
#' - `compute_concentrations()` __ADD A FEW WORDS__
#' - `compute_frequencies()` __ADD A FEW WORDS__
#' - `compute_abundances()` __ADD A FEW WORDS__
#'
#' @name computations
NULL



#' @rdname computations
#' @export

compute_concentrations <- function(data, aggregate = TRUE) {
  
  ## Check data ----
  
  check_if_not_df(data)
  
  if (get_data_type(data) %in% c("CPR North", "Sediment trap")) {
    stop(paste0("This function is not designed to work with 'CPR North' or ", 
                "'Sediment trap' data"), call. = FALSE) 
  }
  
  check_multiple_taxonomies(data)
  
  
  taxa_cols <- extract_species_names(data) 
  
  ready_dat <- data %>%
    filter(.data$subsample_count_type == "Absolute") %>%  
    pivot_longer(all_of(taxa_cols), 
                 names_to  = 'taxa', 
                 values_to = 'counts') %>%
    mutate(to_drop = ifelse(is.na(.data$counts), 'drop', 'keep')) %>% 
    filter(.data$to_drop == 'keep') %>%
    select(-c(.data$to_drop,
              .data$subsample_count_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind,
              .data$sampling_device_type))
  
  abs_data_to_convert <- data %>%
    filter(.data$sample_volume_filtered > 0) %>% 
    filter(.data$subsample_count_type == 'Raw') %>%   
    pivot_longer(all_of(taxa_cols), 
                 names_to  = 'taxa', 
                 values_to = 'counts') %>%
    mutate(to_drop = ifelse(is.na(.data$counts), 'drop', 'keep')) %>% 
    filter(.data$to_drop == 'keep') %>% 
    select(-.data$to_drop) %>% 
    mutate(new_counts = .data$counts / .data$sample_volume_filtered) %>% 
    select(-c(.data$counts,
              .data$subsample_count_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind,
              .data$sampling_device_type)) %>% 
    rename('counts' = 'new_counts') %>% 
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
    mutate(abs_counts = floor((.data$counts * 
                                 .data$total_of_forams_counted_ind) / 100)) %>%
    mutate(new_counts = .data$abs_counts / .data$sample_volume_filtered) %>%
    select(-c(.data$counts,
              .data$abs_counts,
              .data$subsample_count_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind,
              .data$sampling_device_type)) %>%
    rename('counts' = 'new_counts') %>%
    distinct()
  
  excluded_samples_volume <- data %>%
    filter(.data$subsample_count_type != "Absolute") %>%
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
  
  tot_dat <- rbind(ready_dat,
                   abs_data_to_convert,
                   rel_data_to_convert)
  
  if (aggregate) {
    
    tot_dat <- tot_dat %>%
      filter(!is.na(.data$sample_volume_filtered)) %>%
      mutate(abs_sub_tot = floor(.data$sample_volume_filtered * 
                                   .data$counts)) %>%
      group_by(.data$sample_id, .data$taxa) %>%
      mutate(new_counts = sum(.data$abs_sub_tot, na.rm = TRUE)) %>%
      ungroup(.data) %>%
      select(-c(.data$counts, .data$abs_sub_tot, .data$subsample_id)) %>%
      distinct() %>%
      mutate(conc_counts = .data$new_counts / .data$sample_volume_filtered) %>%
      select(-.data$new_counts) %>%
      distinct() %>%
      rename('counts' = 'conc_counts')
  }
  
  tot_dat
}



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
  
  
  taxa_cols <- extract_species_names(data)
  
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
    ungroup(.data) %>% 
    group_by(.data$sample_id) %>% 
    mutate(tot_sample = sum(.data$counts)) %>% 
    ungroup(.data) %>% 
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
    ungroup(.data) %>% 
    group_by(.data$sample_id) %>% 
    mutate(tot_sample = sum(.data$counts)) %>% 
    ungroup(.data)
  
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
    
    aggregated_dat <- rbind(partial_data, ready_dat)
    
    return(aggregated_dat)
  }
}



#' @rdname computations
#' @export

compute_abundances <- function(data, aggregate = TRUE) {
  
  ## Check data ----
  
  check_if_not_df(data)
  
  if (get_data_type(data) %in% c("CPR North", "Sediment trap")) {
    stop(paste0("This function is not designed to work with 'CPR North' or ", 
                "'Sediment trap' data"), call. = FALSE) 
  }
  
  check_multiple_taxonomies(data)
  
  
  taxa_cols <- extract_species_names(data)
  
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
              .data$sampling_device_type))
  
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
    rename('counts' = 'new_counts') %>% 
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
    rename('counts' = 'new_counts') %>% 
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
      mutate(new_counts = sum(.data$counts, na.rm = TRUE)) %>% 
      ungroup(.data) %>% 
      select(-c(.data$counts, .data$subsample_id)) %>%
      distinct() %>% 
      rename('counts' = 'new_counts')
  }
  
  tot_dat
}

#' @rdname computations
#' @export

compute_concentration_bins_CPR_n<- function(data) {
  
  ## Check data ----
  
  check_if_not_df(data)
  
  if (get_data_type(data)!="CPR North") {
    stop(paste0("This function is designed to work only with CPR North data"), call. = FALSE) 
  }
  
  tot_dat <-data %>% 
    mutate(min_conc_binned=.data$count_bin_min/.data$sample_volume_filtered,
           max_conc_binned=.data$count_bin_max/.data$sample_volume_filtered) %>% 
    select(-c(.data$count_bin_min,
              .data$count_bin_max))
  
  tot_dat
  
}

