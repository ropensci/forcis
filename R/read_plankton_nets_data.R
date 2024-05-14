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

  version <- set_version(version, ask = FALSE)
  
  
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
  
  
  ## Check for update ----
  
  if (is.null(check_for_update)) {
    check_for_update <- TRUE
  }
  
  if (check_for_update) {
    if (version != get_latest_version()) {
      message("A newer version of the FORCIS database is available. Use ", 
              "'download_forcis_db(version = NULL)' to download it.")
    }  
  }
  
  
  ## Read data ----
  
  file_name <- list.files(path, pattern = plankton_net_filename())
  
  data <- vroom::vroom(file.path(path, file_name), delim = ";",
                       altrep = FALSE, show_col_types = FALSE)
  
  data <- as.data.frame(data)
  
  data <- add_data_type(data, "Net")
  
  
  ## Check and convert columns ----
  
  taxa_columns <- get_species_names(data)
  
  for (i in 1:length(taxa_columns)) {
    
    data[ , taxa_columns[i]] <- as.numeric(data[ , taxa_columns[i]])
  }
  
  data
}
