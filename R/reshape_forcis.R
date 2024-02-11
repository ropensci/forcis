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

reshape_forcis <- function(data) {
  
  if (get_data_type(data) == "CPR North") {
    stop("This function is not designed to work with 'CPR North' data", 
         call. = FALSE) 
  }
  
  taxa_cols    <- get_species_names(data) 
  metadat_cols <- get_required_columns()
  
  dat_reshaped <- data %>% 
    select(all_of(taxa_cols), metadat_cols) %>% 
    pivot_longer(all_of(taxa_cols), 
                 names_to  = "taxa", 
                 values_to = "counts")
  
  dat_reshaped
}
