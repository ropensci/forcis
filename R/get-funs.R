#' Download the FORCIS database as csv files
#'
#' @description 
#' __ADD DESCRIPTION__
#'
#' @param path a character. The folder in which the FORCIS database will be 
#'   save
#'   
#' @param version a character. The version number of the database. 
#'
#' @return No return value. The five `csv` files will be saved in the `path` 
#'   folder.
#' 
#' @examples
#' \dontrun{
#' path_to_save_csv <- "data"
#' forcis::get_forcis_db(path_to_save_csv)
#' list.files(path_to_save_csv)
#' }

get_forcis_db <- function(path = ".", version = forcis_db_version()) {
  
  ## Create outputs directory is required ----
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  # Download a ZIP or separate csv files ???
  
  # utils::download.file(url = "url_on_zenodo",
  #                      destfile = file.path(path, "forcis_database.csv"))
  
  invisible(NULL)
}



#' Title
#'
#' @param data ...
#'
#' @return ...
#' 
#' @export


get_nets_table <- function(data) {
  
  # ...
  
  x <- 1 + 2
  x
}