#' Filter FORCIS data by ocean
#'
#' @description
#' This function can be used to filter FORCIS data by one or several oceans.
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset.
#' 
#' @param ocean a `character` vector of one or several ocean names. Use the
#'   function `get_ocean_names()` to find the correct spelling.
#'
#' @return A `data.frame` containing a subset of `data`.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

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
  
  
  data_sf <- data_to_sf(data)

  
  ## Spatial filter ----
  
  inter <- suppressWarnings(sf::st_intersects(data_sf, iho, sparse = FALSE))
  
  data[which(apply(inter, 1, any)), ]
}
