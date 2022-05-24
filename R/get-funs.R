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
  
  # download_csv(path, plankton_net_filename())
  # download_csv(path, pump_filename())
  # download_csv(path, cpr_north_filename())
  # download_csv(path, cpr_south_filename())
  # download_csv(path, sediment_trap_filename())
  
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
  
  check_if_path_exists(path)
  
  file_name <- list.files(path, pattern = plankton_net_filename())
  
  if (!length(file_name)) {
    download_csv(path, plankton_net_filename())
  }
  
  utils::read.csv2(file.path(path, plankton_net_filename()))
}
