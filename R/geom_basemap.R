#' Add a World basemap to a ggplot object
#'
#' @description
#' This function creates a World base map that can be added to a ggplot object.
#' Spatial layers come from the Natural Earth project 
#' (<https://www.naturalearthdata.com/>).
#'
#' @return A `ggplot` object.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

geom_basemap <- function() {
  
    list(
      geom_sf(data = ne_oceans, fill = "#cdeafc", col = "#cdeafc", 
              linewidth = 0.10) +
      geom_sf(data = ne_graticules, col = "#bae2fb", 
              linewidth = 0.10) +
      geom_sf(data = ne_countries, fill = "#a6a6a6", col = "#b1b1b1", 
              linewidth = 0.10) +
      geom_sf(data = ne_bbox, fill = NA, col = "#a6a6a6", 
              linewidth = 0.75) +
      theme_void()
    )
}
