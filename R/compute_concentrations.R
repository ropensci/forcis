#' Compute count conversions
#'
#' @description
#' Functions to convert species counts between different formats: raw abundance, 
#' relative abundance, and number concentration, using counts metadata.
#'
#' @param data a `data.frame`. One obtained by `read_*_data()` functions.
#' 
#' @param aggregate a `logical` of length 1. If `FALSE` counts will be derived
#'   for each subsample. If `TRUE` (default) subsample counts will be 
#'   aggregated by `sample_id`.
#'
#' @return A `data.frame` in long format with two additional columns: `taxa`, 
#' the taxon name and `counts_*`, the number concentration (`counts_n_conc`) or
#' the relative abundance (`counts_rel_ab`) or the raw abundance 
#' (`counts_raw_ab`).
#' 
#' @details
#' 
#' - `compute_concentrations()` converts all counts to number concentrations 
#' (n specimens/m³).
#' - `compute_frequencies()` converts all counts to relative abundances 
#' (% specimens per sampling unit).
#' - `compute_abundances()` converts all counts to raw abundances 
#' (n specimens/sampling unit).
#'
#' @name computations
#' 
#' @examples
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"), 
#'                          package = "forcis")
#' 
#' net_data <- read.table(file_name, dec = ".", sep = ";")
#' 
#' # Add 'data_type' column ----
#' net_data$"data_type" <- "Net"
#' 
#' # Select a taxonomy ----
#' net_data <- select_taxonomy(net_data, taxonomy = "VT")
#' 
#' # Dimensions of the data.frame ----
#' dim(net_data)
#' 
#' # Compute concentration ----
#' net_data_conc <- compute_concentrations(net_data)
#' 
#' # Dimensions of the data.frame ----
#' dim(net_data_conc)
NULL



#' @rdname computations
#' @export

compute_concentrations <- function(data, aggregate = TRUE) {
  
  ## Check data ----
  
  check_if_df(data)
  
  if (get_data_type(data) == "Sediment trap") {
    stop(paste0("This function is not designed to work with ", 
                "'Sediment trap' data"), call. = FALSE) 
  }
  
  if (get_data_type(data) == "CPR North") {
      
    return(
      data %>% 
        mutate(min_conc_binned = .data$count_bin_min / 
                 .data$sample_volume_filtered,
               max_conc_binned = .data$count_bin_max / 
                 .data$sample_volume_filtered) %>% 
        select(-c(.data$count_bin_min,
                  .data$count_bin_max)))
  }
  
  check_unique_taxonomy(data)
  
  
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
              .data$sampling_device_type))%>% 
    rename('counts_n_conc' = 'counts') 
  
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
    rename('counts_n_conc' = 'new_counts') %>% 
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
    rename('counts_n_conc' = 'new_counts') %>%
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
                                   .data$counts_n_conc)) %>%
      group_by(.data$sample_id, .data$taxa) %>%
      mutate(new_counts = sum(.data$abs_sub_tot, na.rm = TRUE)) %>%
      ungroup() %>%
      select(-c(.data$counts_n_conc, .data$abs_sub_tot, .data$subsample_id,
                .data$subsample_size_fraction_min,
                .data$subsample_size_fraction_max)) %>%
      distinct() %>%
      mutate(conc_counts = .data$new_counts / .data$sample_volume_filtered) %>%
      select(-.data$new_counts) %>%
      distinct() %>%
      rename('counts_n_conc' = 'conc_counts')
  }
  
  tot_dat
}
