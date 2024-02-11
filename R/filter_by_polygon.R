#' Filter FORCIS data by a spatial polygon
#'
#' @description
#' This function can be used to filter FORCIS data by a spatial polygon.
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset.
#' 
#' @param polygon an `sf POLYGON` object.
#'
#' @return A `data.frame` containing a subset of `data`.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

filter_by_polygon <- function(data, polygon) {
  
  ## Check ocean names ----
  
  if (missing(polygon)) {
    stop("Argument 'polygon' is required", call. = FALSE)
  }
  
  if (!inherits(polygon, "sf")) {
    stop("Argument 'polygon' must be an 'sf' object", call. = FALSE)
  }
  
  if (any(!(unique(sf::st_geometry_type(polygon)) %in% 
        c("POLYGON", "MULTIPOLYGON")))) {
    stop("Argument 'polygon' must be a 'POLYGON' or a 'MULTIPOLYGON'", 
         call. = FALSE)
  }
  
  if (is.na(sf::st_crs(polygon))) {
    stop("The object 'polygon' must have a CRS", call. = FALSE)
  }
  
  
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
                          crs = sf::st_crs(4326))

  
  ## Project spatial objects into Robinson system ----
  
  polygon <- sf::st_transform(polygon, sf::st_crs(crs_robinson()))
  data_sf <- sf::st_transform(data_sf, sf::st_crs(crs_robinson()))
  
  
  ## Spatial filter ----
  
  inter <- suppressWarnings(sf::st_intersects(data_sf, polygon, sparse = FALSE))
  
  data[which(apply(inter, 1, any)), ]
}
