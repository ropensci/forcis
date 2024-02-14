#' Convert a data frame into an sf object
#'
#' @description
#' This function can be used to convert a data frame into an sf object.
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset or the output of a 
#'   `filter_*()` function.
#'
#' @return An `sf POINTS` object.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

data_to_sf <- function(data) {
  
  ## Check data object ----
  
  check_if_df(data)
  
  check_field_in_data(data, "site_lon_start_decimal")
  check_field_in_data(data, "site_lat_start_decimal")
  
  
  ## Convert data into sf object -----
  
  data <- data[!is.na(data$"site_lon_start_decimal"), ]
  data <- data[!is.na(data$"site_lat_start_decimal"), ]
  
  data_sf <- sf::st_as_sf(data, 
                          coords = c("site_lon_start_decimal", 
                                     "site_lat_start_decimal"),
                          crs    = sf::st_crs(4326))
  
  
  ## Project spatial objects into Robinson system ----
  
  data_sf <- sf::st_transform(data_sf, sf::st_crs(crs_robinson()))
  
  data_sf
}
