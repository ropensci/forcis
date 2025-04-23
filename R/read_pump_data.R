#' @rdname read_data
#' @export

read_pump_data <- function(
  path = ".",
  version = options()$"forcis_version",
  check_for_update = options()$"forcis_check_for_update"
) {
  data <- read_data_(
    check_file_fun = pump_filename,
    data_msg = "Pump",
    path = path,
    version = version,
    check_for_update = check_for_update
  )

  data <- add_data_type(data, "Pump")

  ## Check and convert columns ----

  taxa_columns <- get_species_names(data)

  for (i in seq_len(length(taxa_columns))) {
    data[, taxa_columns[i]] <- as.numeric(data[, taxa_columns[i]])
  }

  tibble::as_tibble(data)
}
