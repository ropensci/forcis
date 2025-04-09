#' @rdname read_data
#' @export

# TODO: to be removed & replaced by load_forcis("sediment_trap")
read_sediment_trap_data <- function(
    path = ".",
    version = options()$"forcis_version",
    check_for_update = options()$"forcis_check_for_update") {
  load_forcis(
    name = "sediment_trap",
    version = version,
    check_for_update = check_for_update
  )
  # TODO: investigate warning:
  #   Warning message:
  # One or more parsing issues, call `problems()` on your data frame for details, e.g.:
  #   dat <- vroom(...)
  #   problems(dat)
}
