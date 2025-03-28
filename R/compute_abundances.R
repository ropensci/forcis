#' @rdname computations
#' @export

compute_abundances <- function(data, aggregate = TRUE) {
  ## Check data ----

  check_if_df(data)

  if (get_data_type(data) %in% c("CPR North", "Sediment trap")) {
    stop(
      paste0(
        "This function is not designed to work with 'CPR North' or ",
        "'Sediment trap' data"
      ),
      call. = FALSE
    )
  }

  check_unique_taxonomy(data)

  taxa_cols <- get_species_names(data)

  ## Raw data ----

  raw_data <- data[data$"subsample_count_type" == "Raw", ]

  raw_data <- tidyr::pivot_longer(
    data = raw_data,
    cols = tidyr::all_of(taxa_cols),
    names_to = "taxa",
    values_to = "counts"
  )

  raw_data <- raw_data[!is.na(raw_data$"counts"), ]

  cols_to_remove <- c(
    "subsample_count_type",
    "subsample_all_shells_present_were_counted",
    "total_of_forams_counted_ind",
    "sampling_device_type"
  )

  raw_data <- raw_data[, !(colnames(raw_data) %in% cols_to_remove)]

  colnames(raw_data)[grep("^counts$", colnames(raw_data))] <-
    "counts_raw_ab"

  ## Absolute data ----

  abs_data <- data[data$"sample_volume_filtered" > 0, ]
  abs_data <- abs_data[abs_data$"subsample_count_type" == "Absolute", ]

  abs_data <- tidyr::pivot_longer(
    data = abs_data,
    cols = tidyr::all_of(taxa_cols),
    names_to = 'taxa',
    values_to = 'counts'
  )

  abs_data <- abs_data[!is.na(abs_data$"counts"), ]

  abs_data[["new_counts"]] <- floor(
    abs_data$"counts" * abs_data$"sample_volume_filtered"
  )

  cols_to_remove <- c(
    "counts",
    "subsample_count_type",
    "sampling_device_type",
    "subsample_all_shells_present_were_counted",
    "total_of_forams_counted_ind"
  )

  abs_data <- abs_data[, !(colnames(abs_data) %in% cols_to_remove)]

  colnames(abs_data)[grep("^new_counts$", colnames(abs_data))] <-
    "counts_raw_ab"

  abs_data <- abs_data[!duplicated(abs_data), ]

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

  rel_data[["new_counts"]] <- floor(
    (rel_data$"counts" * rel_data$"total_of_forams_counted_ind") / 100
  )

  cols_to_remove <- c(
    "counts",
    "subsample_count_type",
    "subsample_all_shells_present_were_counted",
    "total_of_forams_counted_ind",
    "sampling_device_type"
  )

  rel_data <- rel_data[, !(colnames(rel_data) %in% cols_to_remove)]

  colnames(rel_data)[grep("^new_counts$", colnames(rel_data))] <-
    "counts_raw_ab"

  rel_data <- rel_data[!duplicated(rel_data), ]

  ## Compute metrics for messages ----

  missing_volume <- data[data$"subsample_count_type" != "Raw", ]
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
    " samples could not be converted because of missing volume data"
  )

  message(
    "Relative counts from ",
    missing_counts,
    " samples could not be converted because of missing data on ",
    "total assemblage"
  )

  tot_data <- rbind(raw_data, abs_data, rel_data)

  if (aggregate) {
    cols_to_remove <- c(
      "counts_raw_ab",
      "subsample_id",
      "subsample_size_fraction_min",
      "subsample_size_fraction_max",
      "taxa"
    )

    sample_data <- tot_data[, !(colnames(tot_data) %in% cols_to_remove)]
    sample_data <- sample_data[!duplicated(sample_data), ]

    y <- stats::aggregate(
      counts_raw_ab ~ sample_id + taxa,
      data = tot_data,
      function(x) sum(x, na.rm = TRUE)
    )

    tot_data <- merge(sample_data, y, by = "sample_id")

    tot_data <- tot_data[c(colnames(sample_data), "taxa", "counts_raw_ab")]
  }

  tibble::as_tibble(tot_data)
}
