#' Read FORCIS data
#' 
#' @description 
#' These functions read one specific `csv` file of the FORCIS database 
#' (see below) stored in the folder `path`. The function [download_forcis_db()]
#' must be used first to store locally the database.
#'
#' @param path a `character` of length 1. The folder in which the FORCIS 
#'   database has been saved.
#'   
#' @inheritParams download_forcis_db
#' 
#' @details
#' 
#' - `read_plankton_nets_data()` reads the FORCIS plankton nets data
#' - `read_pump_data()` reads the FORCIS pump data
#' - `read_cpr_north_data()` reads the FORCIS CPR North data
#' - `read_cpr_south_data()` reads the FORCIS CPR South data
#' - `read_sediment_trap_data()` reads the FORCIS sediment traps data
#'
#' @return A `data.frame`. See \url{https://zenodo.org/record/7390792} for a
#'   preview of the datasets.
#' 
#' @seealso [download_forcis_db()] to download the complete FORCIS database.
#'
#' @name read_data
NULL



#' @rdname read_data
#' @export

read_plankton_nets_data <- function(
    path = ".", 
    version = options()$"forcis_version", 
    check_for_update = options()$"check_for_update") {
  
  ## Check args ----
  
  check_if_character(path)
  check_version(version)
  
  
  ## Check/set version ----
  
  if (is.null(check_for_update)) {
    check_for_update <- TRUE
  }
  
  version <- set_version(version, ask = check_for_update)
  
  
  ## Check local database ----
  
  path <- file.path(path, "forcis-db", paste0("version-", version))
  
  if (!dir.exists(path)) {
    stop("The directory '", path, "' does not exist. Please check the ",
         "argument 'path' or use the function 'download_forcis_db()'.",
         call. = FALSE)
  }
  
  
  ## Check file ----
  
  file_name <- list.files(path, pattern = plankton_net_filename())
  
  if (!length(file_name)) {
    stop("The Plankton net dataset does not exist. Please use the function ", 
         "'download_forcis_db()'.", call. = FALSE)
  }
  
  
  ## Read data ----
  
  file_name <- list.files(path, pattern = plankton_net_filename())
  
  data <- vroom::vroom(file.path(path, file_name), delim = ";")
  
  
  ## Check for data_type column ----
  
  pos <- which("data_type" %in% colnames(data))
  
  if (length(pos) > 0) {
    
    data$"data_type" <- "Net"
    
  } else {
    
    data <- data.frame("data_type" = "Net", data)  
  }
  
  
  taxa_columns <- get_species_names(data)
  
  dplyr::mutate(data, dplyr::across(dplyr::all_of(taxa_columns), as.numeric))
}
