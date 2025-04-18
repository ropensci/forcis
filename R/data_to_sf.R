#' Convert a data frame into an sf object
#'
#' @description
#' This function can be used to convert a `data.frame` into an `sf` object.
#' Note that coordinates (columns `site_lon_start_decimal` and
#' `site_lat_start_decimal`) are projected in the Robinson coordinate system.
#'
#' @param data a `tibble` or a `data.frame`, i.e. a FORCIS dataset or the
#'   output of a `filter_*()` function.
#'
#' @return An `sf POINTS` object.
#'
#' @export
#'
#' @examples
#' # Attach packages ----
#' library("forcis")
#' library("ggplot2")
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
#' # Filter by years ----
#' net_data_sub <- filter_by_year(net_data, years = 1992)
#'
#' # Convert to an sf object ----
#' net_data_sub_sf <- data_to_sf(net_data_sub)
#'
#' # World basemap ----
#' ggplot() +
#'   geom_basemap() +
#'   geom_sf(data = net_data_sub_sf)

data_to_sf <- function(data) {
  ## Check data object ----

  check_if_df(data)

  check_field_in_data(data, "site_lon_start_decimal")
  check_field_in_data(data, "site_lat_start_decimal")

  ## Convert data into sf object -----

  data <- data[!is.na(data$"site_lon_start_decimal"), ]
  data <- data[!is.na(data$"site_lat_start_decimal"), ]

  data_sf <- sf::st_as_sf(
    data,
    coords = c("site_lon_start_decimal", "site_lat_start_decimal"),
    crs = sf::st_crs(4326)
  )

  ## Project spatial objects into Robinson system ----

  data_sf <- sf::st_transform(data_sf, sf::st_crs(crs_robinson()))

  data_sf
}
