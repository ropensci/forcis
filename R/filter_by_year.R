#' Filter FORCIS data by year of sampling
#'
#' @description
#' This function can be used to filter FORCIS data by year of sampling.
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset.
#' 
#' @param years a `numeric` containing one or several years.
#'
#' @return A `data.frame` containing a subset of `data` for the desired years.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

filter_by_year <- function(data, years) {
  
  year_vector <- as.numeric(years)
  
  if (get_data_type(data) == "Sediment trap") {
    
    filtered_dat <- data %>%
      filter(!is.na(.data$sample_date_time_start)) %>%
      mutate(new_sample_date_start = gsub(' .*','', 
                                          .data$sample_date_time_start)) %>% 
      mutate(new_sample_date_start = dmy(.data$new_sample_date_start)) %>%
      mutate(year = year(.data$new_sample_date_start)) %>% 
      filter(.data$year %in% year_vector) %>%
      select(-c(.data$year, .data$new_sample_date_start))
    
  } else {
    
    filtered_dat <- data %>% 
      filter(!is.na(.data$profile_date_time)) %>% 
      mutate(new_profile_date_time = dmy(.data$profile_date_time)) %>% 
      mutate(year = year(.data$new_profile_date_time)) %>% 
      filter(.data$year %in% year_vector) %>% 
      select(-c(.data$year, .data$new_profile_date_time))
  }
  
  filtered_dat
}
