#' @rdname read_data
#' @export

read_pump_data <- function(
    path = ".",
    version = options()$"forcis_version",
    check_for_update = options()$"forcis_check_for_update") {
  read_dataset_by_name(
    name = "pump",
    version = version,
    check_for_update = check_for_update
  )
}
