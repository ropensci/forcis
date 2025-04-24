#' Reshape and simplify FORCIS data
#'
#' @description
#' Reshapes FORCIS data by pivoting species columns into two columns: `taxa`
#' (taxon names) and `counts` (taxon abundances). It converts wider `data.frame`
#' to a long format.
#'
#' @param data a `tibble` or a `data.frame`, i.e. a FORCIS dataset, except for
#'   CPR North data.
#'
#' @return A `tibble` reshaped in a long format.
#'
#' @export
#'
#' @examples
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"),
#'                          package = "forcis")
#'
#' net_data <- read.csv(file_name)
#'
#' # Dimensions of the data.frame ----
#' dim(net_data)
#'
#' # Reshape data ----
#' net_data <- convert_to_long_format(net_data)
#'
#' # Dimensions of the data.frame ----
#' dim(net_data)
#'
#' # Column names ----
#' colnames(net_data)

convert_to_long_format <- function(data) {
  ## Check data object ----

  check_if_df(data)

  if (get_data_type(data) == "CPR North") {
    stop(
      "This function is not designed to work with 'CPR North' data",
      call. = FALSE
    )
  }

  taxa_cols <- get_species_names(data)
  metadata_cols <- get_required_columns()

  data <- data[, c(taxa_cols, metadata_cols)]

  tidyr::pivot_longer(
    data,
    tidyr::all_of(taxa_cols),
    names_to = "taxa",
    values_to = "counts"
  )
}
