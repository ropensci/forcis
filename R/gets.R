#' Download the FORCIS database as a collection of five csv files
#'
#' @description 
#' __ADD DESCRIPTION__
#'
#' @param path a `character` of length 1. The folder in which the FORCIS 
#'   database will be saved.
#'   
#' @param version a `character` of length 1. The version number of the 
#'   FORCIS database. Default is the latest version.
#'
#' @return No return value. The five `csv` files will be saved in the `path` 
#'   folder.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' path_to_save_csv <- "data"
#' forcis::get_forcis_db(path_to_save_csv)
#' list.files(path_to_save_csv)
#' }

get_forcis_db <- function(path = ".", version = forcis_db_version()) {
  
  ## Check args ----
  
  is_character(path)
  is_character(version)
  
  
  ## Create outputs directory if required ----
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  
  ## Download files from Zenodo ----
  
  download_csv(path, plankton_net_filename())
  download_csv(path, pump_filename())
  download_csv(path, cpr_north_filename())
  download_csv(path, cpr_south_filename())
  download_csv(path, sediment_trap_filename())
  
  invisible(NULL)
}



#' Import FORCIS data
#' 
#' @description 
#' These functions import one specific csv file of the FORCIS database 
#' (see below) stored in the folder `path`. If the csv file does not exist 
#' it will be downloaded from \url{https://zenodo.org/record/7390792}.
#'
#' @param path a `character` of length 1. The folder in which the FORCIS 
#'   database is (or will be) saved or read.
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

get_plankton_nets_data <- function(path, version = forcis_db_version()) {
  
  ## Check args ----
  
  is_character(path)
  is_character(version)
  
  check_if_path_exists(path)
  
  
  ## Download csv (if required) ----
  
  file_name <- list.files(path, pattern = plankton_net_filename())
  
  if (!length(file_name)) {
    download_csv(path, plankton_net_filename())
  }
  
  
  ## Read data ----
  
  data <- read.csv2(file.path(path, plankton_net_filename()), dec = ".")
  
  taxa_columns <- extract_species_names(data)
  
  data |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(taxa_columns), as.numeric))
}



#' @rdname get_data
#' @export

get_pump_data <- function(path, version = forcis_db_version()) {
  
  ## Check args ----
  
  is_character(path)
  is_character(version)
  
  check_if_path_exists(path)
  
  
  ## Download csv (if required) ----
  
  file_name <- list.files(path, pattern = pump_filename())
  
  if (!length(file_name)) {
    download_csv(path, pump_filename())
  }
  
  
  ## Read data ----
  
  data <- read.csv2(file.path(path, pump_filename()), dec = ".")
  
  taxa_columns <- extract_species_names(data)
  
  data |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(taxa_columns), as.numeric))
}



#' @rdname get_data
#' @export

get_cpr_north_data <- function(path, version = forcis_db_version()) {
  
  ## Check args ----
  
  is_character(path)
  is_character(version)
  
  check_if_path_exists(path)
  
  
  ## Download csv (if required) ----
  
  file_name <- list.files(path, pattern = cpr_north_filename())
  
  if (!length(file_name)) {
    download_csv(path, cpr_north_filename())
  }
  
  
  ## Read data ----
  
  data <- read.csv2(file.path(path, cpr_north_filename()), dec = ".")
  
  data |> 
    dplyr::mutate(dplyr::across(.data$count_bin_min:.data$count_bin_max, as.numeric))
}



#' @rdname get_data
#' @export

get_cpr_south_data <- function(path, version = forcis_db_version()) {
  
  ## Check args ----
  
  is_character(path)
  is_character(version)
  
  check_if_path_exists(path)
  
  
  ## Download csv (if required) ----
  
  file_name <- list.files(path, pattern = cpr_south_filename())
  
  if (!length(file_name)) {
    download_csv(path, cpr_south_filename())
  }
  
  
  ## Read data ----
  
  data <- read.csv2(file.path(path, cpr_south_filename()), dec = ".")
  
  taxa_columns <- extract_species_names(data)
  
  data |> 
    dplyr::mutate(dplyr::across(dplyr::all_of(taxa_columns), as.numeric))
}



#' @rdname get_data
#' @export

get_sediment_trap_data <- function(path, version = forcis_db_version()) {
  
  ## Check args ----
  
  is_character(path)
  is_character(version)
  
  check_if_path_exists(path)
  
  
  ## Download csv (if required) ----
  
  file_name <- list.files(path, pattern = sediment_trap_filename())
  
  if (!length(file_name)) {
    download_csv(path, sediment_trap_filename())
  }
  
  
  ## Read data ----
  
  data <- read.csv2(file.path(path, sediment_trap_filename()), dec = ".")
  
  taxa_columns <- extract_species_names(data)
  
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

download_csv <- function(path, file) {
  
  check_if_path_exists(path)
  
  utils::download.file(url      = paste0(forcis_db_url(), file, "?download=1"), 
                       destfile = file.path(path, file), mode = "wb")
  
  message("The file '", file, "' has been successfully downloaded")
  
  invisible(NULL)
}
