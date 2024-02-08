#' Download the entire FORCIS database
#'
#' @description 
#' Downloads the entire FORCIS database as a collection of five `csv` files from
#' Zenodo (\url{https://zenodo.org/record/7936568}). 
#'
#' @param path a `character` of length 1. The folder in which the FORCIS 
#'   database will be saved. Note that a subdirectory will be created in the
#'   version number.
#'   
#' @param version a `character` of length 1. The version number of the 
#'   FORCIS database. Default is the latest version.
#' 
#' @param check_for_update a `logical`. If `TRUE` (default) it will check if a
#'   newer version of the FORCIS database is available on Zenodo and ask user
#'   to download it.
#'
#' @param overwrite a `logical`. If `TRUE` it will override the downloaded 
#'   files of the FORCIS database. Default is `FALSE`.
#'
#' @param timeout a `integer`. The timeout for downloading files from the 
#'   FORCIS database. Default is `60`.
#'
#' @return No return value. The FORCIS files will be saved in the `path` folder.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Folder in which the database will be saved ----
#' path_to_save_db <- "data"
#' 
#' # Download the database ----
#' forcis::get_forcis_db(path = path_to_save_db)
#' 
#' # Check the content of the folder ----
#' list.files(path_to_save_db, recursive = TRUE)
#' }

get_forcis_db <- function(path = ".", version = NULL, check_for_update = TRUE,
                          overwrite = FALSE, timeout = 60) {
  
  ## Check args ----
  
  is_character(path)
  
  if (!is.null(version)) {
    
    if (!is.character(version)) {
      stop("Argument 'version' must be character", call. = FALSE)
    }
    
    if (length(version) != 1) {
      stop("Argument 'version' must be character of length 1", call. = FALSE)
    }
  }
  
  
  ## Check/set version ----
  
  version <- set_zen_version(version, ask = check_for_update)
  
  
  ## Create outputs directory if required ----
  
  path <- file.path(path, "forcis-db", paste0("version-", version))
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  
  ## Download files from Zenodo ----
  
  forcis_meta  <- zen_get_version_info(version = version)
  forcis_files <- forcis_meta$"files"
  
  for (i in 1:nrow(forcis_files)) {
    
    download_file(url       = forcis_files[i, "links"]$"self",
                  path      = path, 
                  file      = forcis_files[i, "key"], 
                  overwrite = overwrite, 
                  timeout   = timeout)
  }
  
  invisible(NULL)
}



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
#' @inheritParams get_forcis_db
#' 
#' @details
#' 
#' - `get_plankton_nets_data()` imports the FORCIS plankton nets data
#' - `get_pump_data()` imports the FORCIS pump data
#' - `get_cpr_north_data()` imports the FORCIS CPR North data
#' - `get_cpr_south_data()` imports the FORCIS CPR South data
#' - `get_sediment_trap_data()` imports the FORCIS sediment traps data
#'
#' @return A `data.frame`. See \url{https://zenodo.org/record/7390792} for a
#'   preview of the dataset.
#' 
#' @seealso [get_forcis_db()] to download the complete FORCIS database.
#'
#' @name get_data
NULL



#' @rdname get_data
#' @export

get_plankton_nets_data <- function(path = ".", version = NULL, 
                                   check_for_update = TRUE, overwrite = FALSE, 
                                   timeout = 60) {
  
  ## Check args ----
  
  is_character(path)
  check_zen_version(version)
  
  
  ## Check/set version ----
  
  version <- set_zen_version(version, ask = check_for_update)
  
  
  ## Build outputs directory ----
  
  path <- file.path(path, "forcis-db", paste0("version-", version))
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  
  ## Download file (if required) ----
  
  file_name <- list.files(path, pattern = plankton_net_filename())
  
  if (!length(file_name)) {
    
    forcis_meta  <- zen_get_version_info(version = version)
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
  
  data |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(taxa_columns), as.numeric))
}



#' @rdname get_data
#' @export

get_pump_data <- function(path = ".", version = NULL, 
                          check_for_update = TRUE, overwrite = FALSE, 
                          timeout = 60) {
  
  ## Check args ----
  
  is_character(path)
  check_zen_version(version)
  
  
  ## Check/set version ----
  
  version <- set_zen_version(version, ask = check_for_update)
  
  
  ## Build outputs directory ----
  
  path <- file.path(path, "forcis-db", paste0("version-", version))
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  
  ## Download file (if required) ----
  
  file_name <- list.files(path, pattern = pump_filename())
  
  if (!length(file_name)) {
    
    forcis_meta  <- zen_get_version_info(version = version)
    forcis_files <- forcis_meta$"files"
    
    pos <- grep(pump_filename(), forcis_files$"key")
    
    if (length(pos) == 1) {
      
      download_file(url       = forcis_files[pos, "links"]$"self",
                    path      = path, 
                    file      = forcis_files[pos, "key"], 
                    overwrite = overwrite, 
                    timeout   = timeout)
      
    } else {
      
      stop("Unable to download the 'Pumps' dataset", call. = FALSE)
    }
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
  
  data |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(taxa_columns), as.numeric))
}



#' @rdname get_data
#' @export

get_cpr_north_data <- function(path = ".", version = NULL, 
                               check_for_update = TRUE, overwrite = FALSE, 
                               timeout = 60) {
  
  ## Check args ----
  
  is_character(path)
  check_zen_version(version)
  
  
  ## Check/set version ----
  
  version <- set_zen_version(version, ask = check_for_update)
  
  
  ## Build outputs directory ----
  
  path <- file.path(path, "forcis-db", paste0("version-", version))
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  
  ## Download file (if required) ----
  
  file_name <- list.files(path, pattern = cpr_north_filename())
  
  if (!length(file_name)) {
    
    forcis_meta  <- zen_get_version_info(version = version)
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



#' @rdname get_data
#' @export

get_cpr_south_data <- function(path = ".", version = NULL, 
                               check_for_update = TRUE, overwrite = FALSE, 
                               timeout = 60) {
  
  ## Check args ----
  
  is_character(path)
  check_zen_version(version)
  
  
  ## Check/set version ----
  
  version <- set_zen_version(version, ask = check_for_update)
  
  
  ## Build outputs directory ----
  
  path <- file.path(path, "forcis-db", paste0("version-", version))
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  
  ## Download file (if required) ----
  
  file_name <- list.files(path, pattern = cpr_south_filename())
  
  if (!length(file_name)) {
    
    forcis_meta  <- zen_get_version_info(version = version)
    forcis_files <- forcis_meta$"files"
    
    pos <- grep(cpr_south_filename(), forcis_files$"key")
    
    if (length(pos) == 1) {
      
      download_file(url       = forcis_files[pos, "links"]$"self",
                    path      = path, 
                    file      = forcis_files[pos, "key"], 
                    overwrite = overwrite, 
                    timeout   = timeout)
      
    } else {
      
      stop("Unable to download the 'CPR South' dataset", call. = FALSE)
    }
  }
  
  
  ## Read data ----
  
  file_name <- list.files(path, pattern = cpr_south_filename())
  
  data <- vroom::vroom(file.path(path, file_name), delim = ";")
  
  
  ## Check for data_type column ----
  
  pos <- which("data_type" %in% colnames(data))
  
  if (length(pos) > 0) {
    
    data$"data_type" <- "CPR South"
    
  } else {
    
    data <- data.frame("data_type" = "CPR South", data)
  }
  
  
  taxa_columns <- get_species_names(data)
  
  data |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(taxa_columns), as.numeric))
}



#' @rdname get_data
#' @export

get_sediment_trap_data <- function(path = ".", version = NULL, 
                                   check_for_update = TRUE, overwrite = FALSE, 
                                   timeout = 60) {
  
  ## Check args ----
  
  is_character(path)
  check_zen_version(version)
  
  
  ## Check/set version ----
  
  version <- set_zen_version(version, ask = check_for_update)
  
  
  ## Build outputs directory ----
  
  path <- file.path(path, "forcis-db", paste0("version-", version))
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  
  ## Download file (if required) ----
  
  file_name <- list.files(path, pattern = sediment_trap_filename())
  
  if (!length(file_name)) {
    
    forcis_meta  <- zen_get_version_info(version = version)
    forcis_files <- forcis_meta$"files"
    
    pos <- grep(sediment_trap_filename(), forcis_files$"key")
    
    if (length(pos) == 1) {
      
      download_file(url       = forcis_files[pos, "links"]$"self",
                    path      = path, 
                    file      = forcis_files[pos, "key"], 
                    overwrite = overwrite, 
                    timeout   = timeout)
      
    } else {
      
      stop("Unable to download the 'Sediment traps' dataset", call. = FALSE)
    }
  }
  
  
  ## Read data ----
  
  file_name <- list.files(path, pattern = sediment_trap_filename())
  
  data <- vroom::vroom(file.path(path, file_name), delim = ";")
  
  
  ## Check for data_type column ----
  
  pos <- which("data_type" %in% colnames(data))
  
  if (length(pos) > 0) {
    
    data$"data_type" <- "Sediment trap"
    
  } else {
    
    data <- data.frame("data_type" = "Sediment trap", data)
  }

  
  taxa_columns <- get_species_names(data)
  
  data |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(taxa_columns), as.numeric))
}



#' Download and read IHO spatial layer
#' 
#' Used in the `spatial_filter_*()` functions
#' 
#' @noRd

get_iho_data <- function(path = ".", version = NULL, 
                         check_for_update = TRUE, overwrite = FALSE, 
                         timeout = 60) {
  
  ## Check args ----
  
  is_character(path)
  check_zen_version(version)
  
  
  ## Check/set version ----
  
  version <- set_zen_version(version, ask = check_for_update)
  
  
  ## Build outputs directory ----
  
  path <- file.path(path, "forcis-db", paste0("version-", version))
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  
  ## Download file (if required) ----
  
  file_name <- list.files(path, pattern = iho_oceans_filename())
  
  if (!length(file_name)) {
    
    forcis_meta  <- zen_get_version_info(version = version)
    forcis_files <- forcis_meta$"files"
    
    pos <- grep(iho_oceans_filename(), forcis_files$"key")
    
    if (length(pos) == 1) {
      
      download_file(url       = forcis_files[pos, "links"]$"self",
                    path      = path, 
                    file      = forcis_files[pos, "key"], 
                    overwrite = overwrite, 
                    timeout   = timeout)
      
    } else {
      
      stop("Unable to download the 'IHO' dataset", call. = FALSE)
    }
  }
  
  
  ## Read data ----
  
  file_name <- list.files(path, pattern = iho_oceans_filename())
  
  data <- readRDS(file.path(path, file_name))
  sf::st_as_sf(data)
}



#' Download a csv file
#' 
#' @param file a `character` of length 1. The name of the csv to download.
#' 
#' @noRd

download_file <- function(url, path, file, overwrite = FALSE, timeout = 60) {
  
  url  <- utils::URLencode(url)
  file <- gsub("\\s", "_", file)
  
  check_if_path_exists(path)
  
  
  ## Check if the file already exists ----
  
  destination  <- file.path(path, file)
  
  if (!overwrite && file.exists(destination)) {
    message("The file '", file, "' already exists. If you want to download ",
            "again this file please use the argument 'overwrite'.")
    return(invisible(NULL))
  }
  
  
  ## Download the file if 'overwrite' is TRUE or it doesn't exist ----
  
  # change timeout for large file and slow connection
  user_opts <- options()
  on.exit(options(user_opts))
  options(timeout = max(timeout, getOption("timeout")))
  
  tryCatch({
    utils::download.file(url = url, destfile = destination, 
                         mode = "wb")
  
    message("The file '", file, "' has been successfully downloaded")
    
  }, error = function(e) {
    
    message("Download error: ", e$message)
    
    if (file.exists(destination)) {
      file.remove(destination)
      
      message("Temporary file deleted")
    }
  })
  
  invisible(NULL)
}
