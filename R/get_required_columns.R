#' Get required column names
#'
#' @description
#' Gets required column names (except taxa names) for the package. This 
#' function is designed to help users to add additional columns in 
#' [select_columns()] (argument `cols`) if missing from this list.
#' 
#' @export
#'
#' @return A `character`.
#' 
#' @examples
#' \dontrun{
#' # Get required column names (expect taxa names) ----
#' forcis::get_required_columns()
#' }

get_required_columns <- function() {
  
  required_columns()
}
