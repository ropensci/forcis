#' Select a taxonomy in FORCIS data
#'
#' @description
#' Selects a taxonomy in FORCIS data. FORCIS database provides three different
#' taxonomies: `"LT"` (lumped taxonomy), `"VT"` (validated taxonomy) and `"OT"`
#' (original taxonomy). See \url{https://doi.org/10.1038/s41597-023-02264-2} for
#' further information.
#' 
#' @param data a `data.frame`. One obtained by `read_*_data()` functions.
#'
#' @param taxonomy a `character` of length 1. One among `"LT"`, `"VT"`, `"OT"`.
#' 
#' @export
#'
#' @return A `data.frame`.
#' 
#' @examples
#' # Attach the package ----
#' library("forcis")
#' 
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_pump_sample.csv"), 
#'                          package = "forcis")
#' 
#' pump_data <- vroom::vroom(file_name, delim = ";", show_col_types = FALSE)
#' 
#' # Add 'data_type' column ----
#' pump_data$"data_type" <- "Pump"
#' 
#' # Dimensions of the data.frame ----
#' dim(pump_data)
#' 
#' # Select a taxonomy ----
#' pump_data <- select_taxonomy(pump_data, taxonomy = "OT")
#' 
#' # Dimensions of the data.frame ----
#' dim(pump_data)

select_taxonomy <- function(data, taxonomy) {
  
  
  ## Check args ----
  
  check_if_df(data)
  check_if_valid_taxonomy(taxonomy)
  
  
  ## Error for CPR North ----
  
  if (get_data_type(data) == "CPR North") {
    stop(paste0("This function cannot be used with CPR North data. There is ",
                "no need to filter these data."), call. = FALSE)
  }
  
  
  ## Remove species from others taxonomies ----
  
  species_to_del <- species_list()[which(species_list()[ , "taxonomy"] != 
                                           toupper(taxonomy)), "taxon"]
  
  pos <- which(colnames(data) %in% species_to_del)
  
  if (length(pos) > 0) {
    data <- data[ , -pos, drop = FALSE]
  }
  
  if (length(get_species_names(data)) == 0) {
    stop("No species match the desired taxonomy", call. = FALSE)
  }
  
  data
}