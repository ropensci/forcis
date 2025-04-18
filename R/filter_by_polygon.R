#' Filter FORCIS data by a spatial polygon
#'
#' @description
#' Filters FORCIS data by a spatial polygon.
#'
#' @param data a `tibble` or a `data.frame`. One obtained by `read_*_data()`
#'   functions.
#'
#' @param polygon an `sf POLYGON` object.
#'
#' @return A `tibble` containing a subset of `data` for the desired spatial
#'   polygon.
#'
#' @export
#'
#' @examples
#' # Attach the package ----
#' library("forcis")
#'
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"),
#'   package = "forcis"
#' )
#'
#' net_data <- read.table(file_name, dec = ".", sep = ";")
#'
#' # Add 'data_type' column ----
#' net_data$"data_type" <- "Net"
#'
#' # Dimensions of the data.frame ----
#' dim(net_data)
#'
#' # Import Indian Ocean spatial polygons ----
#' file_name <- system.file(
#'   file.path(
#'     "extdata",
#'     "IHO_Indian_ocean_polygon.gpkg"
#'   ),
#'   package = "forcis"
#' )
#'
#' indian_ocean <- sf::st_read(file_name)
#'
#' # Filter by polygon ----
#' net_data_sub <- filter_by_polygon(net_data, polygon = indian_ocean)
#'
#' # Dimensions of the data.frame ----
#' dim(net_data_sub)

filter_by_polygon <- function(data, polygon) {
  ## Check ocean names ----

  if (missing(polygon)) {
    stop("Argument 'polygon' is required", call. = FALSE)
  }

  if (!inherits(polygon, "sf")) {
    stop("Argument 'polygon' must be an 'sf' object", call. = FALSE)
  }

  if (
    any(
      !(unique(sf::st_geometry_type(polygon)) %in% c("POLYGON", "MULTIPOLYGON"))
    )
  ) {
    stop(
      "Argument 'polygon' must be a 'POLYGON' or a 'MULTIPOLYGON'",
      call. = FALSE
    )
  }

  if (is.na(sf::st_crs(polygon))) {
    stop("The object 'polygon' must have a CRS", call. = FALSE)
  }

  data <- data[!is.na(data$"site_lon_start_decimal"), ]
  data <- data[!is.na(data$"site_lat_start_decimal"), ]

  data_sf <- data_to_sf(data)

  ## Project spatial objects into Robinson system ----

  polygon <- sf::st_transform(polygon, sf::st_crs(crs_robinson()))

  ## Spatial filter ----

  inter <- suppressWarnings(sf::st_intersects(data_sf, polygon, sparse = FALSE))

  data <- data[which(apply(inter, 1, any)), ]

  tibble::as_tibble(data)
}
