#' Plot sample records by depth of collection
#'
#' @description
#' This function produces a barplot of FORCIS sample records by depth.
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
#' plot_record_by_depth(net_data)

plot_record_by_depth <- function(data) {
  ## Check data object ----

  check_if_df(data)

  if (get_data_type(data) != "Net") {
    stop("This function is designed to work only with Net data", call. = FALSE)
  }

  check_field_in_data(data, "sample_id")
  check_field_in_data(data, "sample_min_depth")
  check_field_in_data(data, "sample_max_depth")

  ## Prepare data ----

  data <- data[, c("sample_id", "sample_min_depth", "sample_max_depth")]

  data[["sampling_interval"]] <- NA

  pos <- which(
    is.na(data[["sample_min_depth"]])
  )
  if (length(pos) > 0) {
    data[pos, "sampling_interval"] <- "Unknown"
  }

  pos <- which(
    data[["sample_min_depth"]] <= 5
  )
  if (length(pos) > 0) {
    data[pos, "sampling_interval"] <- "from Surface"
  }

  pos <- which(
    data[["sample_min_depth"]] > 5 &
      data[["sample_min_depth"]] <= 100
  )
  if (length(pos) > 0) {
    data[pos, "sampling_interval"] <- "from first 100m"
  }

  pos <- which(
    data[["sample_min_depth"]] > 100 &
      data[["sample_min_depth"]] <= 300
  )
  if (length(pos) > 0) {
    data[pos, "sampling_interval"] <- "from 100m-300m"
  }

  pos <- which(
    data[["sample_min_depth"]] > 500 &
      data[["sample_min_depth"]] <= 500
  )
  if (length(pos) > 0) {
    data[pos, "sampling_interval"] <- "from 300m-500m"
  }

  pos <- which(
    data[["sample_min_depth"]] > 500
  )
  if (length(pos) > 0) {
    data[pos, "sampling_interval"] <- "from below 500m"
  }

  data <- table(data$"sampling_interval") |>
    data.frame()

  colnames(data) <- c("sampling_interval", "n")

  data$"sampling_interval" <- factor(
    x = data$"sampling_interval",
    levels = c(
      "Unknown",
      "from below 500m",
      "from 300m-500m",
      "from 100m-300m",
      "from first 100m",
      "from Surface"
    )
  )

  ggplot(data = data, mapping = aes(.data$sampling_interval, .data$n)) +
    geom_col(width = 0.7, col = "black") +
    xlab(label = "Sampling interval") +
    ylab(label = "Number of FORCIS samples") +
    coord_flip() +
    theme_classic()
}
