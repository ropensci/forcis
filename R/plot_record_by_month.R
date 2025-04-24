#' Plot sample records by month
#'
#' @description
#' This function produces a barplot of FORCIS sample records by month.
#'
#' @param data a `tibble` or a `data.frame`, i.e. a FORCIS dataset.
#'
#' @return A `ggplot` object.
#'
#' @export
#'
#' @examples
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"),
#'                          package = "forcis")
#'
#' net_data <- read.table(file_name, dec = ".", sep = ";")
#'
#' # Plot data by year (example dataset) ----
#' plot_record_by_month(net_data)

plot_record_by_month <- function(data) {
  ## Check data object ----

  check_if_df(data)
  check_field_in_data(data, "sample_id")

  ## Extract year ----

  if (get_data_type(data) == "Sediment trap") {
    check_field_in_data(data, "sample_date_time_start")

    data$"sampling_month" <- as.numeric(sub(
      "^\\d{2}/(\\d{2})/\\d{4}$",
      "\\1",
      data$"sample_date_time_start"
    ))
  } else {
    check_field_in_data(data, "profile_date_time")

    data$"sampling_month" <- as.numeric(sub(
      "^\\d{2}/(\\d{2})/\\d{4}$",
      "\\1",
      data$"profile_date_time"
    ))
  }

  ## Get distinct values & count ----

  data <- data[, c("sample_id", "sampling_month")]
  data <- data[!duplicated(data), ]

  data <- table(data$"sampling_month") |>
    data.frame()

  colnames(data) <- c("sampling_month", "count")

  data$"sampling_month" <- as.numeric(as.character(data$"sampling_month"))

  ## Ensure to have all months ----

  sampling_month <- data.frame("sampling_month" = 1:12)

  data <- merge(data, sampling_month, by = "sampling_month", all = TRUE)

  data$"count" <- ifelse(is.na(data$"count"), 0, data$"count")

  ## Trick for ggplot2 ----

  data$"sampling_month" <- factor(data$"sampling_month", levels = 1:12)

  ## Plot ----

  ggplot(data, aes(x = .data$sampling_month, y = .data$count)) +
    geom_bar(width = 0.7, col = "black", stat = "identity") +
    theme_classic() +
    xlab(label = "Month") +
    ylab(label = "Number of FORCIS samples")
}
