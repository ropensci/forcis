#' Plankton nets file name
#' 
#' @noRd

plankton_net_filename <- function() paste0("FORCIS_net_", 
                                           forcis_db_version(), ".csv")



#' Pumps file name
#' 
#' @noRd

pump_filename <- function() paste0("FORCIS_pump_", 
                                   forcis_db_version(), ".csv")



#' CPR North file name
#' 
#' @noRd

cpr_north_filename <- function() paste0("FORCIS_cpr_north_", 
                                        forcis_db_version(), ".csv")



#' CPR South file name
#' 
#' @noRd

cpr_south_filename <- function() paste0("FORCIS_cpr_south_", 
                                        forcis_db_version(), ".csv")



#' Sediment Traps file name
#' 
#' @noRd

sediment_trap_filename <- function() paste0("FORCIS_trap_", 
                                            forcis_db_version(), ".csv")