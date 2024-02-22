#' Filter FORCIS data by a spatial bounding box 
#'
#' @description
#' This function can be used to filter FORCIS data by a spatial bounding box.
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset.
#' 
#' @param bbox an object of class `bbox` (package `sf`) or a vector of four 
#'   `numeric` values defining a square bounding box. Values must follow this 
#'   order: minimum longitude (`xmin`), minimum latitude (`ymin`), maximum 
#'   longitude (`xmax`), and maximum latitude (`ymax`). 
#'   **Important:** if a vector of numeric values is provided, coordinates must
#'   be defined in the system WGS 84 (`epsg=4326`).
#'
#' @return A `data.frame` containing a subset of `data`.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

filter_by_bbox <- function(data, bbox) {
  
  data <- data[!is.na(data$"site_lon_start_decimal"), ]
  data <- data[!is.na(data$"site_lat_start_decimal"), ]
  
  data_sf <- data_to_sf(data)
  
  
  ## Convert clip region into bbox object ----
  
  if (inherits(bbox, "numeric")) {
    bbox <- sf::st_bbox(c(xmin = bbox[1], ymin = bbox[2], 
                          xmax = bbox[3], ymax = bbox[4]),
                        crs = sf::st_crs(4326))  
  }
  
  
  ## Check bbox object ----
  
  if (!inherits(bbox, "bbox")) {
    stop("The object 'bbox' must a numeric or a bbox object", call. = FALSE)
  }
  
  if (is.na(sf::st_crs(bbox))) {
    stop("The object 'bbox' must have a CRS", call. = FALSE)
  }
  
  
  ## Convert bbox into spatial polygon ----
  
  bbox <- sf::st_as_sf(sf::st_as_sfc(bbox))
  
  
  ## Project spatial objects into Robinson system ----
  
  bbox    <- sf::st_transform(bbox, sf::st_crs(crs_robinson()))
  
  
  ## Spatial filter ----
  
  inter <- suppressWarnings(sf::st_intersects(data_sf, bbox, sparse = FALSE))
  
  data[which(apply(inter, 1, any)), ]
}
