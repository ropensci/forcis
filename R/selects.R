#' Select columns in a data.frame
#'
#' @param data a data.frame. One obtained by `get_*_data()` functions.
#' @param cols a character vector of columns names to keep in addition to the 
#'   required ones.
#' 
#' @export
#'
#' @return A `data.frame`

select_columns <- function(data, cols = NULL) {
  
  check_if_not_df(data)
  
  if (is.null(cols)) {
    
  } else {
    
  }
  
  data
}