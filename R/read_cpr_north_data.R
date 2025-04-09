#' @rdname read_data
#' @export

read_cpr_north_data <- function(
    path = ".",
    version = options()$"forcis_version",
    check_for_update = options()$"forcis_check_for_update") {
  load_forcis(
    name = "cpr_north",
    version = version,
    check_for_update = check_for_update
  )
}
