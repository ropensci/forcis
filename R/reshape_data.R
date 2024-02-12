#' Reshape and simplify FORCIS data
#'
#' @description
#' A short description...
#' 
#' @param data a `data.frame`, i.e. a FORCIS dataset, except for CPR North data.
#'
#' @return A `data.frame` reshaped in a long format.
#' 
#' @export
#'
#' @examples
#' ## ADD EXAMPLE ----

reshape_data <- function(data) {
  
  ## Check data object ----
  
  check_if_not_df(data)
  
  if (get_data_type(data) == "CPR North") {
    stop("This function is not designed to work with 'CPR North' data", 
         call. = FALSE) 
  }
  
  taxa_cols     <- get_species_names(data) 
  metadata_cols <- get_required_columns()
  
  data <- data[ , c(taxa_cols, metadata_cols)]

  tidyr::pivot_longer(data,
                      tidyr::all_of(taxa_cols), 
                      names_to  = "taxa", 
                      values_to = "counts")
}
