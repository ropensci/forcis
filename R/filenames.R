#' Plankton nets file name
#' 
#' @noRd

plankton_net_filename <- function() paste0("subset-planktonnet_", 
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