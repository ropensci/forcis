#' Filter FORCIS data by ocean
#'
#' @description
#' Filters FORCIS data by one or several oceans.
#' 
#' @param data a `data.frame`. One obtained by `read_*_data()` functions.
#' 
#' @param ocean a `character` vector of one or several ocean names. Use the
#'   function `get_ocean_names()` to find the correct spelling.
#'
#' @return A `data.frame` containing a subset of `data` for the desired oceans.
#' 
#' @export
#'
#' @examples
#' # Attach the package ----
#' library("forcis")
#' 
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"), 
#'                          package = "forcis")
#' 
#' net_data <- read.table(file_name, dec = ".", sep = ";")
#' 
#' # Add 'data_type' column ----
#' net_data$"data_type" <- "Net"
#' 
#' # Dimensions of the data.frame ----
#' dim(net_data)
#' 
#' # Get ocean names ----
#' get_ocean_names()
#' 
#' # Filter by oceans ----
#' net_data_sub <- filter_by_ocean(net_data, ocean = "Indian Ocean")
#' 
#' # Dimensions of the data.frame ----
#' dim(net_data_sub)

filter_by_ocean <- function(data, ocean) {
  
  ## Check ocean names ----
  
  if (missing(ocean)) {
    stop("Argument 'ocean' is required", call. = FALSE)
  }
  
  if (!is.character(ocean)) {
    stop("Argument 'ocean' must be a character of length >= 1", call. = FALSE)
  }
  
  if (any(!(ocean %in% get_ocean_names()))) {
    stop("Some ocean names are mispelled. Please use 'get_ocean_names()' ",
         "to find the correct spelling", call. = FALSE)
  }

  
  ## Select ocean polygons in IHO layer ----  
  
  iho <- iho_boundaries[which(iho_boundaries$"NAME" %in% ocean), ]
  
  
  data <- data[!is.na(data$"site_lon_start_decimal"), ]
  data <- data[!is.na(data$"site_lat_start_decimal"), ]
  
  data_sf <- data_to_sf(data)

  
  ## Spatial filter ----
  
  inter <- suppressWarnings(sf::st_intersects(data_sf, iho, sparse = FALSE))
  
  data[which(apply(inter, 1, any)), ]
}
