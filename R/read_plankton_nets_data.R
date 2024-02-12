#' Import FORCIS data
#' 
#' @description 
#' These functions import one specific `csv` file of the FORCIS database 
#' (see below) stored in the folder `path`. If the `csv` file does not exist 
#' it will be downloaded from Zenodo (\url{https://zenodo.org/record/7936568}).
#'
#' @param path a `character` of length 1. The folder in which the FORCIS 
#'   database has been (or will be) saved.
#'   
#' @inheritParams download_forcis_db
#' 
#' @details
#' 
#' - `read_plankton_nets_data()` imports the FORCIS plankton nets data
#' - `read_pump_data()` imports the FORCIS pump data
#' - `read_cpr_north_data()` imports the FORCIS CPR North data
#' - `read_cpr_south_data()` imports the FORCIS CPR South data
#' - `read_sediment_trap_data()` imports the FORCIS sediment traps data
#'
#' @return A `data.frame`. See \url{https://zenodo.org/record/7390792} for a
#'   preview of the dataset.
#' 
#' @seealso [download_forcis_db()] to download the complete FORCIS database.
#'
#' @name read_data
NULL



#' @rdname read_data
#' @export

read_plankton_nets_data <- function(
    path = ".", version = options()$"forcis_version", 
    check_for_update = options()$"check_for_update", 
    overwrite = FALSE, timeout = 60) {
  
  ## Check args ----
  
  is_character(path)
  check_zen_version(version)
  
  
  ## Check/set version ----
  
  if (is.null(check_for_update)) {
    check_for_update <- TRUE
  }
  
  version <- set_zen_version(version, ask = check_for_update)
  
  
  ## Build outputs directory ----
  
  path <- file.path(path, "forcis-db", paste0("version-", version))
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  
  ## Download file (if required) ----
  
  file_name <- list.files(path, pattern = plankton_net_filename())
  
  if (!length(file_name)) {
    
    forcis_meta  <- version_info(version = version)
    forcis_files <- forcis_meta$"files"
    
    pos <- grep(plankton_net_filename(), forcis_files$"key")
    
    if (length(pos) == 1) {
      
      download_file(url       = forcis_files[pos, "links"]$"self",
                    path      = path, 
                    file      = forcis_files[pos, "key"], 
                    overwrite = overwrite, 
                    timeout   = timeout)
      
    } else {
      
      stop("Unable to download the 'Plankton nets' dataset", call. = FALSE)
    }
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
