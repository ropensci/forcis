#' Plot a distribution map of FORCIS data
#'
#' @description
#' This function can be used to map FORCIS data.
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset or the output of a 
#'   `filter_*()` function.
#' 
#' @param col a `character` of length 1. The color of data on the map.
#' 
#' @param ... other graphical parameters passed on to `geom_sf()`.
#'
#' @return A `ggplot` object.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

map_distribution <- function(data, col = "red", ...) {
  
  ## Check data object ----
  
  check_if_not_df(data)
  
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
  
  
  ## Map ----
  
  ggplot() +
    
    ### Basemap ----
  
    geom_sf(data = ne_oceans, fill = "#cdeafc", col = "#cdeafc", 
            linewidth = 0.10) +
    geom_sf(data = ne_graticules, col = "#bae2fb", 
            linewidth = 0.10) +
    geom_sf(data = ne_countries, fill = "#a6a6a6", col = "#b1b1b1", 
            linewidth = 0.10) +
    geom_sf(data = ne_bbox, fill = NA, col = "#a6a6a6", 
            linewidth = 0.75) +
    
    
    ### Data ---
    
    geom_sf(data = data_sf, col = col, ...) +
    
    
    ### Theme ----
  
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
}
