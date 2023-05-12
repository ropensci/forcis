#' Compute count conversions from net and pump data
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
  
  #insert check device (net and pump only)
  #insert check taxonomy
  
  taxa_cols <- extract_species_names(df) 
  
  
  ready_dat<-df %>%
    select(select(required_columns(),all_of(taxa_cols)),79:276) %>% 
    filter(.data$subsample_count_type=="Absolute") %>%  
    pivot_longer(13:ncol(.data), names_to = 'taxa', values_to = 'counts') %>%
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep')%>%
    select(-c(.data$to_drop,.data$subsample_count_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind,
              .data$sampling_device_type))
  
  abs_data_to_convert<-df %>%
    filter(.data$sample_volume_filtered>0) %>% 
    select(required_columns(),all_of(taxa_cols)) %>% 
    filter(.data$subsample_count_type=='Raw') %>%   
    pivot_longer(13:ncol(.data), names_to = 'taxa', values_to = 'counts') %>%
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep') %>% 
    select(-.data$to_drop) %>% 
    mutate(new_counts=.data$counts/.data$sample_volume_filtered) %>% 
    select(-c(.data$counts,.data$subsample_count_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind,
              .data$sampling_device_type)) %>% 
    rename('counts'='new_counts') %>% 
    distinct(.data)
  
  rel_data_to_convert<-df %>%
    filter(.data$sample_volume_filtered>0) %>%
    select(required_columns(),all_of(taxa_cols)) %>% 
    filter(.data$subsample_count_type=='Relative') %>%
    pivot_longer(13:ncol(.data), names_to = 'taxa', values_to = 'counts') %>%
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>%
    filter(.data$to_drop=='keep') %>%
    select(-.data$to_drop) %>%
    filter(.data$subsample_all_shells_present_were_counted==1) %>%
    filter(!is.na(.data$total_of_forams_counted_ind)) %>%
    mutate(abs_counts=floor((.data$counts*.data$total_of_forams_counted_ind)/ 100)) %>%
    mutate(new_counts=.data$abs_counts/.data$sample_volume_filtered) %>%
    select(-c(.data$counts,.data$abs_counts,.data$subsample_count_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind,
              .data$sampling_device_type)) %>%
    rename('counts'='new_counts') %>%
    distinct(.data)
  
  excluded_samples_volume<-df %>%
    filter(.data$subsample_count_type!="Absolute") %>%
    select(required_columns(),all_of(taxa_cols)) %>% 
    filter(is.na(.data$sample_volume_filtered)==TRUE)
  
  excluded_samples_missing_counts<-df %>%
    filter(.data$sample_volume_filtered>0) %>% 
    select(required_columns(),all_of(taxa_cols)) %>% 
    filter(.data$subsample_count_type=='Relative') %>%    
    pivot_longer(13:ncol(.data), names_to = 'taxa', values_to = 'counts') %>%
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
  
  #insert check device (net and pump only)
  #insert check taxonomy
  
  taxa_cols <- extract_species_names(df)
  
  ready_dat<-df %>%
    select(required_columns(),all_of(taxa_cols)) %>% 
    filter(.data$subsample_count_type=="Relative") %>%    
    pivot_longer(13:ncol(.data), names_to = 'taxa', values_to = 'counts') %>% 
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep') %>% 
    select(-c(.data$to_drop,.data$subsample_count_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind,
              .data$sampling_device_type,
              .data$sample_volume_filtered)) 
  
  samples_to_convert<-df$sample_id[which(df$subsample_all_shells_present_were_counted==1)]
  
  list_samples<-unique(samples_to_convert)
  
  
  conc_to_frequency<-df %>%
    select(required_columns(),all_of(taxa_cols)) %>% 
    filter(.data$subsample_count_type=="Absolute") %>% 
    pivot_longer(13:ncol(.data), names_to = 'taxa', values_to = 'counts') %>% 
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
    select(-c(.data$sample_volume_filtered,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind,
              .data$sampling_device_type))
  
  abs_to_frequency<-df %>% 
    select(required_columns(),all_of(taxa_cols)) %>% 
    filter(.data$subsample_count_type=="Raw") %>% 
    pivot_longer(13:ncol(.data), names_to = 'taxa', values_to = 'counts') %>% 
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep') %>% 
    select(-c(.data$to_drop,.data$subsample_count_type)) %>% 
    mutate(to_drop=ifelse(.data$sample_id %in% list_samples, 'keep','drop')) %>% 
    filter(.data$to_drop=='keep') %>% 
    select(- c(.data$to_drop,
               .data$subsample_all_shells_present_were_counted,
               .data$total_of_forams_counted_ind,
               .data$sampling_device_type,
               .data$sample_volume_filtered)) %>% 
    group_by(.data$subsample_id) %>% 
    mutate(tot_subsample=sum(.data$counts)) %>% 
    ungroup(.data) %>% 
    group_by(.data$sample_id) %>% 
    mutate(tot_sample=sum(.data$counts)) %>% 
    ungroup(.data)
  
  #
  merged_frequency<-rbind(conc_to_frequency,abs_to_frequency)
  
  excluded_samples_volume<-df %>% 
    select(required_columns(),all_of(taxa_cols)) %>% 
    filter(.data$subsample_count_type=="Absolute") %>% 
    pivot_longer(13:ncol(.data), names_to = 'taxa', values_to = 'counts') %>% 
    filter(is.na(.data$sample_volume_filtered)==TRUE)
  
  
  samples_not_possible__to_convert<-df$sample_id[which(df$subsample_all_shells_present_were_counted==0)]
  
  
  
  msg_info("Counts from",msg_value(length(unique(excluded_samples_volume$sample_id))),
           "samples could not be converted because of missing volume data")
  
  msg_info("Counts from",msg_value(length(unique(samples_not_possible__to_convert))),
           "samples could not be converted because of missing data on total assemblage")
  
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


compute_abundances<-function(df, aggregate=TRUE){
  
  #insert check device (net and pump only)
  #insert check taxonomy
  
  taxa_cols <- extract_species_names(df)
  
  ready_dat<-df %>%
    select(required_columns(),all_of(taxa_cols)) %>%  
    filter(.data$subsample_count_type=="Raw") %>%  
    pivot_longer(13:ncol(.data), names_to = 'taxa', values_to = 'counts') %>% 
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep')%>%
    select(-c(.data$to_drop,.data$subsample_count_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind,
              .data$sampling_device_type))
  
  
  conc_data_to_convert<-df %>%
    filter(.data$sample_volume_filtered>0) %>% 
    select(required_columns(),all_of(taxa_cols)) %>%
    filter(.data$subsample_count_type=='Absolute') %>%   
    pivot_longer(13:ncol(.data), names_to = 'taxa', values_to = 'counts') %>%
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep') %>% 
    select(-c(.data$to_drop,
              .data$sampling_device_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind)) %>% 
    mutate(new_counts=floor(.data$counts*.data$sample_volume_filtered)) %>% 
    select(-c(.data$counts,.data$subsample_count_type)) %>% 
    rename('counts'='new_counts') %>% 
    distinct()
  
  
  rel_data_to_convert<-df %>%
    filter(.data$sample_volume_filtered>0) %>% 
    select(required_columns(),all_of(taxa_cols)) %>%
    filter(.data$subsample_count_type=='Relative') %>%    
    pivot_longer(13:ncol(.data), names_to = 'taxa', values_to = 'counts') %>%
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep') %>% 
    select(-.data$to_drop) %>% 
    filter(.data$subsample_all_shells_present_were_counted==1) %>% 
    filter(!is.na(.data$total_of_forams_counted_ind)) %>% 
    mutate(new_counts=floor((.data$counts*.data$total_of_forams_counted_ind)/ 100)) %>% 
    select(-c(.data$counts,
              .data$subsample_count_type,
              .data$subsample_all_shells_present_were_counted,
              .data$total_of_forams_counted_ind,
              .data$sampling_device_type)) %>% 
    rename('counts'='new_counts') %>% 
    distinct()
  
  excluded_samples_volume<-df %>%
    filter(.data$subsample_count_type!="Raw") %>%
    select(required_columns(),all_of(taxa_cols)) %>%   
    filter(is.na(.data$sample_volume_filtered)==TRUE)
  
  #
  excluded_samples_missing_counts<-df %>%
    filter(.data$sample_volume_filtered>0) %>% 
    select(required_columns(),all_of(taxa_cols)) %>% 
    filter(.data$subsample_count_type=='Relative') %>%    
    pivot_longer(13:ncol(.data), names_to = 'taxa', values_to = 'counts') %>% 
    mutate(to_drop=ifelse(is.na(.data$counts),'drop','keep')) %>% 
    filter(.data$to_drop=='keep') %>% 
    select(-.data$to_drop) %>%  
    filter(is.na(.data$total_of_forams_counted_ind))
  
  msg_info("Counts from",msg_value(length(unique(excluded_samples_volume$sample_id))),
           "samples could not be converted because of missing volume data")
  
  msg_info("Relative counts from",msg_value(length(unique(excluded_samples_missing_counts$sample_id))),
           "samples could not be converted because of missing data on total assemblage")
  
  
  tot_dat<-rbind(ready_dat,conc_data_to_convert,rel_data_to_convert)
  
  
  if (aggregate==FALSE) {
    return(tot_dat)}
  if (aggregate==TRUE) {
    
    aggregated_dat<-tot_dat %>% 
      group_by(.data$sample_id,.data$taxa) %>% 
      mutate(new_counts=sum(.data$counts, na.rm = TRUE)) %>% 
      ungroup() %>% 
      select(-c(.data$counts,.data$subsample_id)) %>%
      distinct() %>% 
      rename('counts'='new_counts')
    
    
    
    return(aggregated_dat)
  }
  
}





