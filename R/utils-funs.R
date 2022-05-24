#' FORCIS database version
#' 
#' @noRd

forcis_db_version <- function() "May2022"



#' FORCIS database URL
#' 
#' @noRd

forcis_db_url <- function() "repository_url"



#' Plankton nets file name
#' 
#' @noRd

plankton_net_filename <- function() paste0("planktonnet_", 
                                           forcis_db_version(), ".csv")



#' Pumps file name
#' 
#' @noRd

pump_filename <- function() paste0("pump_", 
                                   forcis_db_version(), ".csv")



#' CPR North file name
#' 
#' @noRd

cpr_north_filename <- function() paste0("cprnorth_", 
                                        forcis_db_version(), ".csv")



#' CPR South file name
#' 
#' @noRd

cpr_south_filename <- function() paste0("cprsouth_", 
                                        forcis_db_version(), ".csv")



#' CPR South file name
#' 
#' @noRd

sediment_trap_filename <- function() paste0("sedimenttraps_", 
                                            forcis_db_version(), ".csv")



#' Check if a path exists
#' 
#' If the path `path` does not exist, returns an error.
#' 
#' @inheritParams get_forcis_db
#' 
#' @noRd

check_if_path_exists <- function(path) {
  
  if (!dir.exists(path)) {
    stop("The directory '", path, "' does not exist", call. = FALSE)
  }
  
  invisible(NULL)
}



#' Download a csv file
#' 
#' @param file a character. The name of the csv to download.
#' 
#' @inheritParams get_forcis_db
#' 
#' @noRd

download_csv <- function(path, file) {
  
  check_if_path_exists(path)
  
  utils::download.file(url      = paste0(forcis_db_url(), ""), 
                       destfile = file.path(path, file))
  
  invisible(NULL)
}
