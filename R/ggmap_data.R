#' Map the spatial distribution of FORCIS data
#'
#' @description
#' Maps the spatial distribution of FORCIS data.
#'
#' @param data a `data.frame`. One obtained by `read_*_data()` functions.
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
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"),
#'                          package = "forcis")
#'
#' net_data <- read.table(file_name, dec = ".", sep = ";")
#'
#' # Add 'data_type' column ----
#' net_data$"data_type" <- "Net"
#'
#' # Map data (default) ----
#' ggmap_data(net_data)
#'
#' # Map data ----
#' ggmap_data(net_data, col = "black", fill = "red", shape = 21, size = 2)

ggmap_data <- function(data, col = "red", ...) {
  data_sf <- data_to_sf(data)

  ggplot() +
    geom_basemap() +
    geom_sf(data = data_sf, col = col, ...) +
    theme(plot.title = element_text(hjust = 0.5))
}
