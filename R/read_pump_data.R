#' @rdname read_data
#' @export

# TODO: to be removed & replaced by load_forcis("pump")
read_pump_data <- function(
    path = ".",
    version = options()$"forcis_version",
    check_for_update = options()$"forcis_check_for_update") {
  load_forcis(
    name = "pump",
    version = version,
    check_for_update = check_for_update
  )
}
