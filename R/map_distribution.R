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
  
  data_sf <- data_to_sf(data)
  
  
  ## Map ----
  
  ggplot() +
    
    geom_basemap() +
    
    
    ### Data ---
    
    geom_sf(data = data_sf, col = col, ...) +
    
    
    ### Theme ----
  
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
}
