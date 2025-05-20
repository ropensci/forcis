#' @rdname read_data
#' @export

read_cpr_south_data <- function(
  path,
  version = options()$"forcis_version",
  check_for_update = options()$"forcis_check_for_update"
) {
  data <- read_data_(
    check_file_fun = cpr_south_filename,
    data_msg = "South CPR",
    path = path,
    version = version,
    check_for_update = check_for_update
  )

  data <- add_data_type(data, "CPR South")

  ## Check and convert columns ----

  taxa_columns <- get_species_names(data)

  for (i in seq_len(length(taxa_columns))) {
    data[, taxa_columns[i]] <- as.numeric(data[, taxa_columns[i]])
  }

  tibble::as_tibble(data)
}
