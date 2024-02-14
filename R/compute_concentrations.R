#' Compute count conversions
#'
#' @description
#' __ADD DESCRIPTION__
#'
#' @param data a `data.frame`. One obtained by `read_*_data()` functions.
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
  
  check_if_df(data)
  
  if (get_data_type(data) %in% c("CPR North", "Sediment trap")) {
    stop(paste0("This function is not designed to work with 'CPR North' or ", 
                "'Sediment trap' data"), call. = FALSE) 
  }
  
  check_multiple_taxonomies(data)
  
  
  taxa_cols <- get_species_names(data) 
  
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
      ungroup() %>%
      select(-c(.data$counts, .data$abs_sub_tot, .data$subsample_id,
                .data$subsample_size_fraction_min,
                .data$subsample_size_fraction_max)) %>%
      distinct() %>%
      mutate(conc_counts = .data$new_counts / .data$sample_volume_filtered) %>%
      select(-.data$new_counts) %>%
      distinct() %>%
      rename('counts' = 'conc_counts')
  }
  
  tot_dat
}
