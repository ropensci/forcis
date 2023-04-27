#' Select columns in FORCIS data
#'
#' @description
#' __ADD DESCRIPTION__
#' 
#' @param data a `data.frame`. One obtained by `get_*_data()` functions.
#'
#' @param cols a `character` vector of columns names to keep in addition to the 
#'   required ones.
#' 
#' @export
#'
#' @return A `data.frame`.
#' 
#' @examples
#' ## __ADD EXAMPLE__

select_columns <- function(data, cols = NULL) {
  
  
  # Check args ----
  
  check_if_not_df(data)
  
  if (!is.null(cols)) {
    if (is.character(cols)) {
      stop("Argument 'cols' must be a character", call. = FALSE)
    }
  }
  
  
  # Check for missing columns ----
  
  check_required_columns(data)
  
  if (!is.null(cols)) {
    if (any(!(cols %in% colnames(data)))) {
      stop("Some columns to select are absent from data", call. = FALSE)
    }
  }
  
  
  # Subset columns ----
  
  if (is.null(cols)) {
    
    data <- data[ , required_columns()]
    
  } else {
    
    data <- data[ , c(required_columns(), cols)]
  }
  
  data
}
