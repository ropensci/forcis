#' @rdname read_data
#' @export

read_pump_data <- function(
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
  
  file_name <- list.files(path, pattern = pump_filename())
  
  if (!length(file_name)) {
    stop("The Pump dataset does not exist. Please use the function ", 
         "'download_forcis_db()'.", call. = FALSE)
  }
  
  
  ## Read data ----
  
  file_name <- list.files(path, pattern = pump_filename())
  
  data <- vroom::vroom(file.path(path, file_name), delim = ";")
  
  
  ## Check for data_type column ----
  
  pos <- which("data_type" %in% colnames(data))
  
  if (length(pos) > 0) {
    
    data$"data_type" <- "Pump"
    
  } else {
    
    data <- data.frame("data_type" = "Pump", data)
  }
  
  
  taxa_columns <- get_species_names(data)
  
  dplyr::mutate(data, dplyr::across(dplyr::all_of(taxa_columns), as.numeric))
}
