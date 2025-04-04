#' Plot sample records by year
#'
#' @description
#' This function produces a barplot of FORCIS sample records by year.
#'
#' @param data a `tibble` or a `data.frame`, i.e. a FORCIS dataset.
#'
#' @return A `ggplot` object.
#'
#' @export
#'
#' @examples
#' # Attach the package ----
#' library("forcis")
#'
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"),
#'   package = "forcis"
#' )
#'
#' net_data <- read.table(file_name, dec = ".", sep = ";")
#'
#' # Plot data by year (example dataset) ----
#' plot_record_by_year(net_data)
plot_record_by_year <- function(data) {
  ## Check data object ----

  check_if_df(data)
  check_field_in_data(data, "sample_id")

  ## Extract year ----

  if (get_data_type(data) == "Sediment trap") {
    check_field_in_data(data, "sample_date_time_start")

    data$"sampling_year" <- as.numeric(sub(
      "^\\d{2}/\\d{2}/(\\d{4})$",
      "\\1",
      data$"sample_date_time_start"
    ))
  } else {
    check_field_in_data(data, "profile_date_time")

    data$"sampling_year" <- as.numeric(sub(
      "^\\d{2}/\\d{2}/(\\d{4})$",
      "\\1",
      data$"profile_date_time"
    ))
  }

  ## Get distinct values ----

  data <- data[, c("sample_id", "sampling_year")]
  data <- data[!duplicated(data), ]

  ## Plot ----

  ggplot(data, aes(x = .data$sampling_year)) +
    geom_bar(width = 0.7, col = "black", stat = "count") +
    theme_classic() +
    xlab(label = "Year") +
    ylab(label = "Number of FORCIS samples")
}
