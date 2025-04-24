#' Get species names from column names
#'
#' @description
#' Gets species names from column names. This function is just an utility to
#' easily retrieve taxon names.
#'
#' @param data a `tibble` or a `data.frame`. One obtained by `read_*_data()`
#'   functions.
#'
#' @export
#'
#' @return A `character` vector of species names.
#'
#' @examples
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"),
#'                          package = "forcis")
#'
#' net_data <- read.csv(file_name)
#'
#' # Select a taxonomy ----
#' net_data <- select_taxonomy(net_data, taxonomy = "VT")
#'
#' # Retrieve taxon names ----
#' get_species_names(net_data)

get_species_names <- function(data) {
  check_if_df(data)

  ## Special case for CPR North ----

  if (get_data_type(data) == "CPR North") {
    species_names <- unique(data$"species")

    ## Otherwise ----
  } else {
    species_names <- colnames(data)[which(
      colnames(data) %in%
        species_list()[, "taxon"]
    )]
  }

  species_names
}
