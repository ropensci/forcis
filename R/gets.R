#' Download the entire FORCIS database
#'
#' @description 
#' Downloads the entire FORCIS database as a collection of five `csv` files from
#' Zenodo (\url{https://zenodo.org/record/7936568}). 
#'
#' @param path a `character` of length 1. The folder in which the FORCIS 
#'   database will be saved.
#'   
#' @param version a `character` of length 1. The version number of the 
#'   FORCIS database. Default is the latest version.
#'
#' @param overwrite a `logical`. If `TRUE` it will override the downloaded 
#'   files of the FORCIS database. Default is `FALSE`.
#'
#' @param timeout a `integer`. The timeout for downloading files from the 
#'   FORCIS database. Default is `60`.
#'
#' @return No return value. The five `csv` files will be saved in the `path`
#' folder.
#' 
#' @details
#' If user is interested in only one type of data (e.g. plankton nets), the 
#' function `get_plankton_nets_data()` (and others) is a better choice as it 
#' will download only one `csv` file.
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
#' list.files(path_to_save_db)
#' }

get_forcis_db <- function(path = ".", version = forcis_db_version(), 
                          overwrite = FALSE, timeout = 60) {
  
  ## Check args ----
  
  is_character(path)
  is_character(version)
  
  
  ## Create outputs directory if required ----
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  
  ## Download files from Zenodo ----
  
  download_csv(path, plankton_net_filename(), overwrite, timeout)
  download_csv(path, pump_filename(), overwrite, timeout)
  download_csv(path, cpr_north_filename(), overwrite, timeout)
  download_csv(path, cpr_south_filename(), overwrite, timeout)
  download_csv(path, sediment_trap_filename(), overwrite, timeout)
  
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

get_plankton_nets_data <- function(path, version = forcis_db_version(), 
                                   timeout = 60) {
  
  ## Check args ----
  
  is_character(path)
  is_character(version)
  
  check_if_path_exists(path)
  
  
  ## Download csv (if required) ----
  
  file_name <- list.files(path, pattern = plankton_net_filename())
  
  if (!length(file_name)) {
    download_csv(path, plankton_net_filename(), timeout)
  }
  
  
  ## Read data ----
  
  data <- vroom::vroom(file.path(path, plankton_net_filename()), delim = ";")
  data <- data.frame("data_type" = "Net", data)
  
  taxa_columns <- get_species_names(data)
  
  data |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(taxa_columns), as.numeric))
}



#' @rdname get_data
#' @export

get_pump_data <- function(path, version = forcis_db_version(), timeout = 60) {
  
  ## Check args ----
  
  is_character(path)
  is_character(version)
  
  check_if_path_exists(path)
  
  
  ## Download csv (if required) ----
  
  file_name <- list.files(path, pattern = pump_filename())
  
  if (!length(file_name)) {
    download_csv(path, pump_filename(), timeout)
  }
  
  
  ## Read data ----
  
  data <- vroom::vroom(file.path(path, pump_filename()), delim = ";")
  data <- data.frame("data_type" = "Pump", data)
  
  taxa_columns <- get_species_names(data)
  
  data |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(taxa_columns), as.numeric))
}



#' @rdname get_data
#' @export

get_cpr_north_data <- function(path, version = forcis_db_version(), 
                               timeout = 60) {
  
  ## Check args ----
  
  is_character(path)
  is_character(version)
  
  check_if_path_exists(path)
  
  
  ## Download csv (if required) ----
  
  file_name <- list.files(path, pattern = cpr_north_filename())
  
  if (!length(file_name)) {
    download_csv(path, cpr_north_filename(), timeout)
  }
  
  
  ## Read data ----
  
  data <- vroom::vroom(file.path(path, cpr_north_filename()), delim = ";")
  data <- data.frame("data_type" = "CPR North", data)
  
  data |> 
    dplyr::mutate(dplyr::across(.data$count_bin_min:.data$count_bin_max, 
                                as.numeric))
}



#' @rdname get_data
#' @export

get_cpr_south_data <- function(path, version = forcis_db_version(),
                               timeout = 60) {
  
  ## Check args ----
  
  is_character(path)
  is_character(version)
  
  check_if_path_exists(path)
  
  
  ## Download csv (if required) ----
  
  file_name <- list.files(path, pattern = cpr_south_filename())
  
  if (!length(file_name)) {
    download_csv(path, cpr_south_filename(), timeout)
  }
  
  
  ## Read data ----
  
  data <- vroom::vroom(file.path(path, cpr_south_filename()), delim = ";")
  data <- data.frame("data_type" = "CPR South", data)
  
  taxa_columns <- get_species_names(data)
  
  data |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(taxa_columns), as.numeric))
}



#' @rdname get_data
#' @export

get_sediment_trap_data <- function(path, version = forcis_db_version(), 
                                   timeout = 60) {
  
  ## Check args ----
  
  is_character(path)
  is_character(version)
  
  check_if_path_exists(path)
  
  
  ## Download csv (if required) ----
  
  file_name <- list.files(path, pattern = sediment_trap_filename())
  
  if (!length(file_name)) {
    download_csv(path, sediment_trap_filename(), timeout)
  }
  
  
  ## Read data ----
  
  data <- vroom::vroom(file.path(path, sediment_trap_filename()), delim = ";")
  data <- data.frame("data_type" = "Sediment trap", data)
  
  taxa_columns <- get_species_names(data)
  
  data |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(taxa_columns), as.numeric))
}



#' Download a csv file
#' 
#' @param file a `character` of length 1. The name of the csv to download.
#' 
#' @inheritParams get_forcis_db
#' 
#' @noRd

download_csv <- function(path, file, overwrite = FALSE, timeout = 60) {
  
  check_if_path_exists(path)
  
  ## Check if the file is already exists ----
  
  destination  <- file.path(path, file)
  download_url <- paste0(forcis_db_url(), file, "?download=1")
  
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
    utils::download.file(url = download_url, destfile = destination, 
                         mode = "wb")
  
    message("The file '", file, "' has been successfully downloaded")
    
  }, error = function(e) {
    
    message("Download error: ", e$message)
    
    if (file.exists(destination)) {
      file.remove(destination)
      
      message("Temporary file deleted.")
    }
  })
  
  invisible(NULL)
}
