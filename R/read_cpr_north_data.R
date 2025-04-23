#' @rdname read_data
#' @export

read_cpr_north_data <- function(
  path = ".",
  version = options()$"forcis_version",
  check_for_update = options()$"forcis_check_for_update"
) {
  data <- read_data_(
    check_file_fun = cpr_north_filename,
    data_msg = "North CPR",
    path = path,
    version = version,
    check_for_update = check_for_update
  )

  data <- add_data_type(data, "CPR North")

  ## Check and convert columns ----

  taxa_columns <- c("count_bin_min", "count_bin_max")

  for (i in seq_len(length(taxa_columns))) {
    check_field_in_data(data, taxa_columns[i])

    data[, taxa_columns[i]] <- as.numeric(data[, taxa_columns[i]])
  }

  tibble::as_tibble(data)
}
