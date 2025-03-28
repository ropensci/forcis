#' Compute count conversions
#'
#' @description
#' Functions to convert species counts between different formats: raw abundance,
#' relative abundance, and number concentration, using counts metadata.
#'
#' @param data a `data.frame`. One obtained by `read_*_data()` functions.
#'
#' @param aggregate a `logical` of length 1. If `FALSE` counts will be derived
#'   for each subsample. If `TRUE` (default) subsample counts will be
#'   aggregated by `sample_id`.
#'
#' @return A `data.frame` in long format with two additional columns: `taxa`,
#' the taxon name and `counts_*`, the number concentration (`counts_n_conc`) or
#' the relative abundance (`counts_rel_ab`) or the raw abundance
#' (`counts_raw_ab`).
#'
#' @details
#'
#' - `compute_concentrations()` converts all counts to number concentrations
#' (n specimens/m³).
#' - `compute_frequencies()` converts all counts to relative abundances
#' (% specimens per sampling unit).
#' - `compute_abundances()` converts all counts to raw abundances
#' (n specimens/sampling unit).
#'
#' @name computations
#'
#' @examples
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"),
#'                          package = "forcis")
#'
#' net_data <- read.table(file_name, dec = ".", sep = ";")
#'
#' # Select a taxonomy ----
#' net_data <- select_taxonomy(net_data, taxonomy = "VT")
#'
#' # Dimensions of the data.frame ----
#' dim(net_data)
#'
#' # Compute concentration ----
#' net_data_conc <- compute_concentrations(net_data)
#'
#' # Dimensions of the data.frame ----
#' dim(net_data_conc)
NULL


#' @rdname computations
#' @export

compute_concentrations <- function(data, aggregate = TRUE) {
  ## Check data ----

  check_if_df(data)

  if (get_data_type(data) == "Sediment trap") {
    stop(
      paste0(
        "This function is not designed to work with ",
        "'Sediment trap' data"
      ),
      call. = FALSE
    )
  }

  if (get_data_type(data) == "CPR North") {
    data[["min_conc_binned"]] <-
      data$"count_bin_min" / data$"sample_volume_filtered"

    data[["max_conc_binned"]] <-
      data$"count_bin_max" / data$"sample_volume_filtered"

    cols_to_remove <- c("count_bin_min", "count_bin_max")

    data <- data[, !(colnames(data) %in% cols_to_remove)]

    return(data)
  }

  check_unique_taxonomy(data)

  taxa_cols <- get_species_names(data)

  ## Absolute data ----

  abs_data <- data[data$"subsample_count_type" == "Absolute", ]

  abs_data <- tidyr::pivot_longer(
    data = abs_data,
    cols = tidyr::all_of(taxa_cols),
    names_to = 'taxa',
    values_to = 'counts'
  )

  abs_data <- abs_data[!is.na(abs_data$"counts"), ]

  cols_to_remove <- c(
    "subsample_count_type",
    "sampling_device_type",
    "subsample_all_shells_present_were_counted",
    "total_of_forams_counted_ind"
  )

  abs_data <- abs_data[, !(colnames(abs_data) %in% cols_to_remove)]

  colnames(abs_data)[grep("^counts$", colnames(abs_data))] <-
    "counts_n_conc"

  ## Raw data ----

  raw_data <- data[data$"subsample_count_type" == "Raw", ]
  raw_data <- raw_data[raw_data$"sample_volume_filtered" > 0, ]

  raw_data <- tidyr::pivot_longer(
    data = raw_data,
    cols = tidyr::all_of(taxa_cols),
    names_to = "taxa",
    values_to = "counts"
  )

  raw_data <- raw_data[!is.na(raw_data$"counts"), ]

  raw_data[["new_counts"]] <-
    raw_data$"counts" / raw_data$"sample_volume_filtered"

  cols_to_remove <- c(
    "counts",
    "subsample_count_type",
    "sampling_device_type",
    "subsample_all_shells_present_were_counted",
    "total_of_forams_counted_ind"
  )

  raw_data <- raw_data[, !(colnames(raw_data) %in% cols_to_remove)]

  colnames(raw_data)[grep("^new_counts$", colnames(raw_data))] <-
    "counts_n_conc"

  raw_data <- raw_data[!duplicated(raw_data), ]

  ## Relative data ----

  rel_data <- data[data$"sample_volume_filtered" > 0, ]
  rel_data <- rel_data[rel_data$"subsample_count_type" == "Relative", ]

  rel_data <- tidyr::pivot_longer(
    data = rel_data,
    cols = tidyr::all_of(taxa_cols),
    names_to = 'taxa',
    values_to = 'counts'
  )

  rel_data <- rel_data[!is.na(rel_data$"counts"), ]

  rel_data <- rel_data[
    rel_data$"subsample_all_shells_present_were_counted" == 1,
  ]

  rel_data <- rel_data[!is.na(rel_data$"total_of_forams_counted_ind"), ]

  rel_data[["abs_counts"]] <- floor(
    (rel_data$"counts" * rel_data$"total_of_forams_counted_ind") / 100
  )

  rel_data[["new_counts"]] <-
    rel_data$"abs_counts" / rel_data$"sample_volume_filtered"

  cols_to_remove <- c(
    "counts",
    "abs_counts",
    "subsample_count_type",
    "subsample_all_shells_present_were_counted",
    "total_of_forams_counted_ind",
    "sampling_device_type"
  )

  rel_data <- rel_data[, !(colnames(rel_data) %in% cols_to_remove)]

  colnames(rel_data)[grep("^new_counts$", colnames(rel_data))] <-
    "counts_n_conc"

  rel_data <- rel_data[!duplicated(rel_data), ]

  ## Compute metrics for messages ----

  missing_volume <- data[data$"subsample_count_type" != "Absolute", ]
  missing_volume <- missing_volume[
    is.na(missing_volume$"sample_volume_filtered"),
  ]

  missing_volume <- length(unique(missing_volume$"sample_id"))

  missing_counts <- data[data$"sample_volume_filtered" > 0, ]
  missing_counts <- missing_counts[
    missing_counts$"subsample_count_type" == "Relative",
  ]

  missing_counts <- tidyr::pivot_longer(
    data = missing_counts,
    cols = tidyr::all_of(taxa_cols),
    names_to = "taxa",
    values_to = "counts"
  )

  missing_counts <- missing_counts[!is.na(missing_counts$"counts"), ]
  missing_counts <- missing_counts[
    is.na(missing_counts$"total_of_forams_counted_ind"),
  ]

  missing_counts <- length(unique(missing_counts$"sample_id"))

  message(
    "Counts from ",
    missing_volume,
    " samples could not be converted ",
    "because of missing volume data"
  )

  message(
    "Relative counts from ",
    missing_counts,
    " samples could not be ",
    "converted because of missing data on total assemblage"
  )

  tot_data <- rbind(raw_data, abs_data, rel_data)

  if (aggregate) {
    tot_data <- tot_data[!is.na(tot_data$"sample_volume_filtered"), ]

    tot_data[["abs_sub_tot"]] <- floor(
      tot_data$"sample_volume_filtered" * tot_data$"counts_n_conc"
    )

    cols_to_remove <- c(
      "counts_n_conc",
      "abs_sub_tot",
      "subsample_id",
      "subsample_size_fraction_min",
      "subsample_size_fraction_max",
      "taxa"
    )

    sample_data <- tot_data[, !(colnames(tot_data) %in% cols_to_remove)]
    sample_data <- sample_data[!duplicated(sample_data), ]

    y <- stats::aggregate(
      abs_sub_tot ~ sample_id + taxa,
      data = tot_data,
      function(x) sum(x, na.rm = TRUE)
    )

    tot_data <- merge(sample_data, y, by = "sample_id")

    tot_data[["abs_sub_tot"]] <-
      tot_data$"abs_sub_tot" / tot_data$"sample_volume_filtered"

    colnames(tot_data)[grep("^abs_sub_tot$", colnames(tot_data))] <-
      "counts_n_conc"

    tot_data <- tot_data[c(colnames(sample_data), "taxa", "counts_n_conc")]
  }

  tibble::as_tibble(tot_data)
}
