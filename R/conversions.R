#' Compute concentrations from net data
#'
#' @param df forcis net dataset
#' @param aggregate logical, if FALSE counts will be derived for each subsample. 
#' If TRUE subsample counts will be aggregated by sample_id.
#'
#' @return dataframe
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import rlang
#' @import messages
compute_concentrations<-function(df, aggregate=TRUE){
  
  
  
  
  ready_dat<-df %>%
    select(.data$cruise_id,.data$profile_id,.data$new_station_id,.data$profile_id,
           .data$sample_id,.data$subsample_id,.data$sample_max_depth,
           .data$site_lat_start_decimal,
           .data$site_lon_start_decimal,
           .data$subsample_count_type,.data$sample_volume_filtered,79:276) %>% 
    filter(.data$subsample_count_type=="Absolute") %>%  
    pivot_longer(11:208, names_to = 'taxa', values_to = 'counts') %>% 
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep')%>%
    select(-c(.data$to_drop,.data$subsample_count_type))
  
  abs_data_to_convert<-df %>%
    filter(.data$sample_volume_filtered>0) %>% 
    select(.data$cruise_id,.data$profile_id,
           .data$new_station_id,.data$profile_id,.data$sample_id,
           .data$subsample_id,
           .data$sample_max_depth,.data$site_lat_start_decimal,.data$site_lon_start_decimal,
           .data$subsample_count_type,.data$sample_volume_filtered,79:276) %>% 
    filter(.data$subsample_count_type=='Raw') %>%   
    pivot_longer(11:208, names_to = 'taxa', values_to ='counts') %>%
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep') %>% 
    select(-.data$to_drop) %>% 
    # group_by(sample_id,taxa) %>% 
    # mutate(counts_tot=sum(counts)) %>%
    # ungroup() %>% 
    mutate(new_counts=.data$counts/.data$sample_volume_filtered) %>% 
    select(-c(.data$counts,.data$subsample_count_type)) %>% 
    rename('counts'='new_counts') %>% 
    distinct(.data)
  
  # 
  rel_data_to_convert<-df %>%
    filter(.data$sample_volume_filtered>0) %>%
    select(.data$cruise_id,.data$profile_id,.data$new_station_id,
           .data$profile_id,.data$sample_id,.data$subsample_id,
           .data$sample_max_depth,.data$site_lat_start_decimal,
           .data$site_lon_start_decimal,
           .data$subsample_count_type,.data$sample_volume_filtered,
           .data$subsample_all_shells_present_were_counted,
           .data$total_of_forams_counted_ind,79:276) %>%
    filter(.data$subsample_count_type=='Relative') %>%
    pivot_longer(13:210, names_to = 'taxa', values_to = 'counts') %>%
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>%
    filter(.data$to_drop=='keep') %>%
    select(-.data$to_drop) %>%
    filter(.data$subsample_all_shells_present_were_counted==1) %>%
    filter(!is.na(.data$total_of_forams_counted_ind)) %>%
    mutate(abs_counts=floor((.data$counts*.data$total_of_forams_counted_ind)/ 100)) %>%
    # group_by(sample_id,taxa) %>%
    # mutate(counts_tot=sum(counts)) %>%
    # ungroup() %>%
    mutate(new_counts=.data$abs_counts/.data$sample_volume_filtered) %>%
    select(-c(.data$counts,.data$abs_counts,.data$subsample_count_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind)) %>%
    rename('counts'='new_counts') %>%
    distinct(.data)
  
  excluded_samples_volume<-df %>%
    filter(.data$subsample_count_type!="Absolute") %>%
    select(.data$cruise_id,.data$profile_id,.data$new_station_id,
           .data$profile_id,.data$sample_id,.data$subsample_id,
           .data$sample_max_depth,.data$site_lat_start_decimal,
           .data$site_lon_start_decimal,
           .data$subsample_count_type,.data$sample_volume_filtered,79:276) %>% 
    filter(is.na(.data$sample_volume_filtered)==TRUE)
  
  excluded_samples_missing_counts<-df %>%
    filter(.data$sample_volume_filtered>0) %>% 
    select(.data$cruise_id,.data$profile_id,.data$new_station_id,
           .data$profile_id,.data$sample_id,.data$subsample_id,
           .data$sample_max_depth,.data$site_lat_start_decimal,
           .data$site_lon_start_decimal,
           .data$subsample_count_type,.data$sample_volume_filtered,
           .data$subsample_all_shells_present_were_counted,
           .data$total_of_forams_counted_ind,79:276) %>% 
    filter(.data$subsample_count_type=='Relative') %>%    
    pivot_longer(13:210, names_to = 'taxa', values_to = 'counts') %>%
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep') %>% 
    select(-.data$to_drop) %>%  
    filter(is.na(.data$total_of_forams_counted_ind))
  
  
  
  msg_info("Counts from",msg_value(length(unique(excluded_samples_volume$sample_id))),
           "samples could not be converted because of missing volume data")
  
  msg_info("Relative counts from",msg_value(length(unique(excluded_samples_missing_counts$sample_id))),
           "samples could not be converted because of missing data on total assemblage")


  tot_dat<-rbind(ready_dat,abs_data_to_convert,rel_data_to_convert)
  

  if (!aggregate) {
    return(tot_dat)}
  if (aggregate) {

    aggregated_dat<-tot_dat %>%
      filter(!is.na(.data$sample_volume_filtered)==TRUE) %>%
      mutate(abs_sub_tot=floor(.data$sample_volume_filtered*.data$counts)) %>%
      group_by(.data$sample_id,.data$taxa) %>%
      mutate(new_counts=sum(.data$abs_sub_tot, na.rm = TRUE)) %>%
      ungroup(.data) %>%
      select(-c(.data$counts,.data$abs_sub_tot,.data$subsample_id)) %>%
      distinct(.data) %>%
      mutate(conc_counts=.data$new_counts/.data$sample_volume_filtered) %>%
      select(-.data$new_counts) %>%
      distinct(.data) %>%
      rename('counts'='conc_counts')

    return(aggregated_dat)
  }

}

compute_frequencies<-function(df, aggregate=TRUE){
  
  ready_dat<-df %>%
    select(.data$cruise_id,.data$profile_id,.data$new_station_id,
           .data$profile_id,.data$sample_id,.data$subsample_id,
           .data$sample_max_depth,.data$site_lat_start_decimal,
           .data$site_lon_start_decimal,.data$subsample_count_type,79:276) %>% 
    filter(.data$subsample_count_type=="Relative") %>%    
    pivot_longer(10:207, names_to = 'taxa', values_to = 'counts') %>%  
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep') %>% 
    select(-c(.data$to_drop,.data$subsample_count_type)) 
  
  samples_to_convert<-df %>%
    select(.data$cruise_id,.data$profile_id,.data$new_station_id,
           .data$profile_id,.data$sample_id,.data$subsample_id,
           .data$subsample_count_type,79:276) %>%  
    filter(.data$subsample_count_type!="Relative") %>% 
    pivot_longer(7:204, names_to = 'taxa', values_to = 'counts') %>% 
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep') %>%
    filter(!.data$taxa=='unidentified_specimens') %>%
    filter(!.data$taxa=='other') %>%
    filter(!.data$taxa=='reworked_planktic_foraminifera') %>%
    filter(!.data$taxa=='benthics') %>%
    group_by(.data$sample_id) %>% 
    summarise(n_species_counted=n_distinct(.data$taxa)) %>% 
    ungroup(.data) %>% 
    filter(.data$n_species_counted>1)
  
  list_samples<-unique(samples_to_convert$sample_id)
  
  
  conc_to_frequency<-df %>%
    select(.data$cruise_id,.data$profile_id,.data$new_station_id,
           .data$profile_id,.data$sample_id,.data$subsample_id,
           .data$sample_max_depth,.data$site_lat_start_decimal,
           .data$site_lon_start_decimal,.data$subsample_count_type,
           .data$sample_volume_filtered,79:276) %>% 
    filter(.data$subsample_count_type=="Absolute") %>% 
    pivot_longer(11:208, names_to = 'taxa', values_to = 'counts') %>% 
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep') %>% 
    select(-c(.data$to_drop,.data$subsample_count_type)) %>% 
    filter(.data$sample_volume_filtered>0) %>% 
    mutate(to_drop=ifelse(.data$sample_id %in% list_samples, 'keep','drop')) %>% 
    filter(.data$to_drop=='keep') %>% 
    select(- .data$to_drop) %>% 
    mutate(counts=floor(.data$sample_volume_filtered*.data$counts)) %>% 
    group_by(.data$subsample_id) %>% 
    mutate(tot_subsample=sum(.data$counts)) %>% 
    ungroup(.data) %>% 
    group_by(.data$sample_id) %>% 
    mutate(tot_sample=sum(.data$counts)) %>% 
    ungroup(.data) %>% 
    select(-.data$sample_volume_filtered)
  
  
  abs_to_frequency<-df %>% 
    select(.data$cruise_id,.data$profile_id,.data$new_station_id,
           .data$profile_id,.data$sample_id,.data$subsample_id,
           .data$sample_max_depth,.data$site_lat_start_decimal,
           .data$site_lon_start_decimal,.data$subsample_count_type,79:276) %>%  
    filter(.data$subsample_count_type=="Raw") %>% 
    pivot_longer(10:207, names_to = 'taxa', values_to = 'counts') %>% 
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep') %>% 
    select(-c(.data$to_drop,.data$subsample_count_type)) %>% 
    mutate(to_drop=ifelse(.data$sample_id %in% list_samples, 'keep','drop')) %>% 
    filter(.data$to_drop=='keep') %>% 
    select(- .data$to_drop) %>% 
    group_by(.data$subsample_id) %>% 
    mutate(tot_subsample=sum(.data$counts)) %>% 
    ungroup(.data) %>% 
    group_by(.data$sample_id) %>% 
    mutate(tot_sample=sum(.data$counts)) %>% 
    ungroup(.data)
  
  
  merged_frequency<-rbind(conc_to_frequency,abs_to_frequency)
  
  excluded_samples_volume<-df %>% 
    select(.data$cruise_id,.data$profile_id,.data$new_station_id,
           .data$profile_id,.data$sample_id,.data$subsample_id,
           .data$subsample_count_type,.data$sample_volume_filtered,79:276) %>%  
    filter(.data$subsample_count_type=="Absolute") %>% 
    pivot_longer(8:205, names_to = 'taxa', values_to = 'counts') %>%  
    filter(is.na(.data$sample_volume_filtered)==TRUE)
  
  
  
  excluded_samples_missing_counts<-df %>%
    select(.data$cruise_id,.data$profile_id,.data$new_station_id,
           .data$profile_id,.data$sample_id,.data$subsample_id,
           .data$subsample_count_type,.data$sample_volume_filtered,79:276) %>%
    filter(.data$subsample_count_type!="Relative") %>% 
    filter(.data$sample_volume_filtered>0) %>% 
    pivot_longer(8:205, names_to = 'taxa', values_to = 'counts') %>% 
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep') %>%
    filter(!.data$taxa=='unidentified_specimens') %>%
    filter(!.data$taxa=='other') %>%
    filter(!.data$taxa=='reworked_planktic_foraminifera') %>%
    filter(!.data$taxa=='benthics') %>%
    group_by(.data$sample_id) %>% 
    summarise(n_species_counted=n_distinct(.data$taxa)) %>% 
    ungroup(.data) %>% 
    filter(.data$n_species_counted<=1)
  
  
  
  msg_info("Counts from",msg_value(length(unique(excluded_samples_volume$sample_id))),
           "samples could not be converted because of missing volume data")
  
  msg_info("Counts from",msg_value(length(unique(excluded_samples_missing_counts$sample_id))),
           "samples could not be converted because less then 2 species were identified")
  
  partial_data<-merged_frequency %>% 
    mutate(counts=(.data$counts/.data$tot_subsample)*100) %>% 
    select(-c(.data$tot_subsample, .data$tot_sample))
  
  
  tot_dat<-rbind(partial_data, ready_dat)
  
  
  if (aggregate==FALSE) {
    return(tot_dat)}
  if (aggregate==TRUE) {
    
    partial_data<-merged_frequency %>% 
      mutate(counts=(.data$counts/.data$tot_sample)*100) %>% 
      select(-c(.data$tot_subsample,.data$tot_sample))
    
    aggregated_dat<-rbind(partial_data, ready_dat)
    
    
    
    return(aggregated_dat)
  }
  
}