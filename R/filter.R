#' Reshape and simplify FORCIS data
#'
#' @description
#' A short description...
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset, except for CPR North data.
#'
#' @return A `data.frame` reshaped in a long format.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

reshape_forcis <- function(data) {
  
  if (get_data_type(data) == "CPR North") {
    stop("This function is not designed to work with 'CPR North' data", 
         call. = FALSE) 
  }
  
  taxa_cols    <- get_species_names(data) 
  metadat_cols <- get_required_columns()
  
  dat_reshaped <- data %>% 
    select(all_of(taxa_cols), metadat_cols) %>% 
    pivot_longer(all_of(taxa_cols), 
                 names_to  = "taxa", 
                 values_to = "counts")
  
  dat_reshaped
}



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


#' Filter FORCIS data by a spatial bounding box 
#'
#' @description
#' This function can be used to filter FORCIS data by a spatial bounding box.
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset.
#' 
#' @param coord_square a vector of four `numeric` values defining a square 
#'   bounding box. Values must follow this order: minimum latitude, minimum 
#'   longitude, maximum latitude, and maximum longitude.
#'
#' @return A `data.frame` containing a subset of `data`.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

filter_by_bbox <- function(data, coord_square) {
  
  min_lat  <- coord_square[1] 
  min_long <- coord_square[2] 
  max_lat  <- coord_square[3]
  max_long <- coord_square[3]
  
  data %>% 
    filter(!is.na(.data$site_lat_start_decimal)) %>% 
    filter(!is.na(.data$site_lon_start_decimal)) %>%   
    filter(.data$site_lat_start_decimal >= min_lat &
           .data$site_lat_start_decimal <= max_lat &
           .data$site_lon_start_decimal >= min_long &
           .data$site_lon_start_decimal <= max_long)
}



#' Filter FORCIS data by species 
#'
#' @description
#' A short description...
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset, except for CPR North data.
#' 
#' @param species a `character` vector listing species of interest.
#' 
#' @param rm_na a `logical` value. If `FALSE`, keeps taxa with `NA` counts.
#' 
#' @return A `data.frame` containing a subset of `data`.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

filter_by_species <- function (data, species, rm_na = FALSE) {
  
  my_species <- as.character(species)
  taxa_cols  <- get_species_names(data) 
  
  if (length(taxa_cols) > 0) {
    stop("This function requires data in long format. Please use the function ",
         "'reshape_forcis()'", call. = FALSE) 
  }
  
  if (get_data_type(data) == "CPR North") {
    stop("This function is not designed to work with 'CPR North' data", 
         call. = FALSE) 
  }
  
  filtered_dat <- data %>% 
    filter(.data$taxa %in% my_species)
  
  if (rm_na) {
    
    filtered_dat <- filtered_dat %>% 
      filter(!is.na(.data$counts))
  }
  
  filtered_dat
}
