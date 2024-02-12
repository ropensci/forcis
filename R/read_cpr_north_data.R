#' @rdname read_data
#' @export

read_cpr_north_data <- function(path = ".", 
                                version = options()$"forcis_version", 
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
  
  file_name <- list.files(path, pattern = cpr_north_filename())
  
  if (!length(file_name)) {
    
    forcis_meta  <- version_info(version = version)
    forcis_files <- forcis_meta$"files"
    
    pos <- grep(cpr_north_filename(), forcis_files$"key")
    
    if (length(pos) == 1) {
      
      download_file(url       = forcis_files[pos, "links"]$"self",
                    path      = path, 
                    file      = forcis_files[pos, "key"], 
                    overwrite = overwrite, 
                    timeout   = timeout)
      
    } else {
      
      stop("Unable to download the 'CPR North' dataset", call. = FALSE)
    }
  }
  
  
  ## Read data ----
  
  file_name <- list.files(path, pattern = cpr_north_filename())
  
  data <- vroom::vroom(file.path(path, file_name), delim = ";")
  
  
  ## Check for data_type column ----
  
  pos <- which("data_type" %in% colnames(data))
  
  if (length(pos) > 0) {
    
    data$"data_type" <- "CPR North"
    
  } else {
    
    data <- data.frame("data_type" = "CPR North", data)
  }
  
  
  data |> 
    dplyr::mutate(dplyr::across(.data$count_bin_min:.data$count_bin_max, 
                                as.numeric))
}
