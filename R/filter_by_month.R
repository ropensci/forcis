#' Filter FORCIS data by month of sampling
#'
#' @description
#' This function can be used to filter FORCIS data by month of sampling.
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset.
#' 
#' @param months a `numeric` containing one or several months.
#'
#' @return A `data.frame` containing a subset of `data` for the desired months.
#' 
#' @export
#' 
#' @examples
#' ## ADD EXAMPLE ----

filter_by_month <- function(data, months) {
  
  month_vector <- as.numeric(months)
  
  if (get_data_type(data) == "Sediment trap") {
    
    filtered_dat <- data %>%
      filter(!is.na(.data$sample_date_time_start)) %>%
      mutate(new_sample_date_start = gsub(' .*','', 
                                          .data$sample_date_time_start)) %>% 
      mutate(new_sample_date_start = dmy(.data$new_sample_date_start)) %>%
      mutate(month=month(.data$new_sample_date_start)) %>% 
      filter(.data$month %in% month_vector) %>%
      select(-c(.data$month,.data$new_sample_date_start))
    
  } else {
    
    filtered_dat <- data %>% 
      filter(!is.na(.data$profile_date_time)) %>% 
      mutate(new_profile_date_time = dmy(.data$profile_date_time)) %>% 
      mutate(month = month(.data$new_profile_date_time)) %>% 
      filter(.data$month %in% month_vector)%>% 
      select(-c(.data$month, .data$new_profile_date_time))
  }
  
  filtered_dat
}
