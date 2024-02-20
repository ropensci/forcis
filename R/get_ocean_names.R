#' Get World ocean names 
#'
#' @description
#' This function returns the name of World oceans according to the IHO Sea Areas
#' dataset version 3 (Flanders Marine Institute, 2018).
#'
#' @return A `character` vector with World ocean names.
#' 
#' @export
#' 
#' @references 
#' Flanders Marine Institute (2018). IHO Sea Areas, version 3. 
#' Available online at: \url{https://www.marineregions.org/}.
#' DOI: \url{https://doi.org/10.14284/323}.
#' 
#' @examples
#' \dontrun{
#' get_ocean_names()
#' }

get_ocean_names <- function() {
  
  iho_boundaries$"NAME"
}
