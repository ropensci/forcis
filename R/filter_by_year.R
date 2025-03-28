#' Filter FORCIS data by year of sampling
#'
#' @description
#' Filters FORCIS data by year of sampling.
#'
#' @param data a `tibble` or a `data.frame`. One obtained by `read_*_data()`
#'   functions.
#'
#' @param years a `numeric` containing one or several years.
#'
#' @return A `tibble` containing a subset of `data` for the desired years.
#'
#' @export
#'
#' @examples
#' # Attach the package ----
#' library("forcis")
#'
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"),
#'                          package = "forcis")
#'
#' net_data <- read.table(file_name, dec = ".", sep = ";")
#'
#' # Add 'data_type' column ----
#' net_data$"data_type" <- "Net"
#'
#' # Dimensions of the data.frame ----
#' dim(net_data)
#'
#' # Filter by years ----
#' net_data_sub <- filter_by_year(net_data, years = 1992)
#'
#' # Dimensions of the data.frame ----
#' dim(net_data_sub)

filter_by_year <- function(data, years) {
  ## Check data object ----

  check_if_df(data)

  ## Check years object ----

  if (missing(years)) {
    stop("Argument 'years' is required", call. = FALSE)
  }

  if (!is.numeric(years)) {
    stop("Argument 'years' must be a numeric of length >= 1", call. = FALSE)
  }

  if (get_data_type(data) == "Sediment trap") {
    check_field_in_data(data, "sample_date_time_start")

    data <- data[!is.na(data$"sample_date_time_start"), ]

    if (nrow(data) == 0) {
      stop("The column 'sample_date_time_start' contain only NA", call. = FALSE)
    }

    start_dates <- unlist(lapply(
      strsplit(data$"sample_date_time_start", "\\s"),
      function(x) x[1]
    ))

    start_dates <- as.Date(start_dates, format = date_format())
    start_years <- as.numeric(format(start_dates, "%Y"))

    if (all(!(as.numeric(years) %in% unique(start_years)))) {
      stop("The years provided are out of FORCIS temporal range", call. = FALSE)
    }

    pos <- which(start_years %in% as.numeric(years))

    data <- data[pos, ]
  } else {
    check_field_in_data(data, "profile_date_time")

    data <- data[!is.na(data$"profile_date_time"), ]

    if (nrow(data) == 0) {
      stop("The column 'profile_date_time' contain only NA", call. = FALSE)
    }

    start_dates <- as.character(data$"profile_date_time")
    start_dates <- as.Date(start_dates, format = date_format())
    start_years <- as.numeric(format(start_dates, "%Y"))

    if (all(!(as.numeric(years) %in% unique(start_years)))) {
      stop("The years provided are out of FORCIS temporal range", call. = FALSE)
    }

    pos <- which(start_years %in% as.numeric(years))

    data <- data[pos, ]
  }

  tibble::as_tibble(data)
}
