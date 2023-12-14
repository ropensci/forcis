
#' Reshape and simplify forcis data
#'
#' @param data forcis data
#'
#' @return A `data.frame`
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

reshape_forcis <- function(data){
  
  if (get_data_type(data) %in% c("CPR North")) {
    stop(paste0("This function is not designed to work with 'CPR North' data"), call. = FALSE) 
  }
  
  taxa_cols <- get_species_names(data) 
  metadat_cols <- get_required_columns()
  dat_reshaped <- dat_reshaped %>% 
    select(all_of(taxa_cols),metadat_cols) %>% 
    pivot_longer(all_of(taxa_cols), 
                 names_to  = 'taxa', 
                 values_to = 'counts')
  return(dat_reshaped)
}



#' Filter forcis data by year of sampling
#'
#' @param data forcis data in long format
#' @param years numeric vector of selected years
#'
#' @return A `data.frame`.
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

filter_by_year <- function(data,years){
  year_vector <- as.numeric(years)
  filtered_dat <- data %>% 
    filter(! is.na(.data$profile_date_time)) %>% 
    mutate(new_profile_date_time = dmy(.data$profile_date_time)) %>% 
             mutate(year=year(.data$new_profile_date_time)) %>% 
             filter(.data$year %in% year_vector) %>% 
             select(-c(.data$year,.data$new_profile_date_time))
           
           return(filtered_dat)
           
}


#' Filter forcis data by month of sampling
#'
#' @param data forcis data in long format
#' @param months  numeric vector of selected months
#'
#' @return A `data.frame`.
#' @export
#' 
#' @examples
#' ## ADD EXAMPLE ----

filter_by_month <- function(data,months){
  
  month_vector <- as.numeric(months)
  
  filtered_dat <- data %>% 
    filter(! is.na(.data$profile_date_time)) %>% 
    mutate(new_profile_date_time =dmy(.data$profile_date_time)) %>% 
    mutate(month=month(.data$new_profile_date_time)) %>% 
    filter(.data$month %in% month_vector)%>% 
    select(-c(.data$month,.data$new_profile_date_time))
  
  return(filtered_dat)
  
}


#' Filter forcis data by coordinate square 
#'
#' @param data forcis data in long format
#' @param coord_square a numeric vector containing in this order minimum latitute,
#' minimum longitude, maximum latitude, maximum longitude
#'
#' @return A `data.frame`.
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

filter_by_coordinates <- function(data, coord_square){
  
  min_lat <- coord_square[1] 
  min_long <- coord_square[2] 
  max_lat <- coord_square[3]
  max_long <- coord_square[3]
  
  filtered_dat <- data %>% 
    filter(! is.na(.data$site_lat_start_decimal)) %>% 
    filter(! is.na (.data$site_lon_start_decimal)) %>%   
    filter(.data$site_lat_start_decimal>= min_lat &
             .data$site_lat_start_decimal <=max_lat &
             .data$site_lon_start_decimal>= min_long &
             .data$site_lon_start_decimal <=max_long)
  
  return(filtered_dat)
}

#' Filter forcis data by species 
#'
#' @param data forcis data in long format, except for CPR North data
#' @param species a character vector listing species of interest
#'
#' @return A `data.frame`
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

filter_by_species <- function (data,species ){
  my_species <- as.character(species)
  
  filtered_dat <- data %>% 
    filter(.data$taxa %in% my_species)
  # filter(! is.na(counts))
  
  return(filtered_dat)
}
