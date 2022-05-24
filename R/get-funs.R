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



#' Import plankton nets data
#' 
#' @description 
#' This function reads the csv file `planktonnet_May2022.csv` stored in the 
#' folder `path`. If this file does not exist it will be downloaded from
#' \url{Repository}.
#'
#' @inheritParams get_forcis_db
#'
#' @return A `data.frame` with x rows and y columns. See **Data Paper URL** for
#'   further information.
#' 
#' @export


get_plankton_nets_data <- function(path, version = forcis_db_version()) {
  
  ## Check if path exists ----
  
  if (!dir.exists(path)) {
    stop("The directory '", path, "' does not exist")
  }
  
  
  ## Check if csv exists ----
  
  file_name <- list.files(path, pattern = plankton_net_filename())
  
  if (length(file_name) == 0) {
    
    message("Downloading Plankton nets data...")
    # utils::download.file()
  }
  
  read.csv(file.path(path, plankton_net_filename()))
}
