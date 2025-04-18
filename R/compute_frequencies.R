#' @rdname computations
#' @export

compute_frequencies <- function(data, aggregate = TRUE) {
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

  raw_data <- data[data$"subsample_count_type" == "Relative", ]

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
    "sampling_device_type",
    "sample_volume_filtered"
  )

  raw_data <- raw_data[, !(colnames(raw_data) %in% cols_to_remove)]

  samples_to_convert <- data[
    data$"subsample_all_shells_present_were_counted" == 1,
  ]

  list_samples <- unique(samples_to_convert[["sample_id"]])

  ## Absolute data ----

  abs_data <- data[data$"sample_volume_filtered" > 0, ]
  abs_data <- abs_data[abs_data$"subsample_count_type" == "Absolute", ]

  abs_data <- tidyr::pivot_longer(
    data = abs_data,
    cols = tidyr::all_of(taxa_cols),
    names_to = "taxa",
    values_to = "counts"
  )

  abs_data <- abs_data[!is.na(abs_data$"counts"), ]
  abs_data <- abs_data[abs_data$"sample_id" %in% list_samples, ]

  abs_data[["counts"]] <- floor(
    abs_data$"counts" * abs_data$"sample_volume_filtered"
  )

  col_names <- colnames(abs_data)

  y <- stats::aggregate(
    counts ~ subsample_id,
    data = abs_data,
    function(x) sum(x, na.rm = TRUE)
  )

  colnames(y)[2] <- "tot_subsample"

  abs_data <- merge(abs_data, y, by = "subsample_id")
  abs_data <- abs_data[, c(col_names, "tot_subsample")]

  col_names <- colnames(abs_data)

  y <- stats::aggregate(
    counts ~ sample_id,
    data = abs_data,
    function(x) sum(x, na.rm = TRUE)
  )

  colnames(y)[2] <- "tot_sample"

  abs_data <- merge(abs_data, y, by = "sample_id")
  abs_data <- abs_data[, c(col_names, "tot_sample")]

  cols_to_remove <- c(
    "sample_volume_filtered",
    "subsample_count_type",
    "subsample_all_shells_present_were_counted",
    "total_of_forams_counted_ind",
    "sampling_device_type"
  )

  abs_data <- abs_data[, !(colnames(abs_data) %in% cols_to_remove)]

  ## Relative data ----

  rel_data <- data[data$"subsample_count_type" == "Raw", ]

  rel_data <- tidyr::pivot_longer(
    data = rel_data,
    cols = tidyr::all_of(taxa_cols),
    names_to = "taxa",
    values_to = "counts"
  )

  rel_data <- rel_data[!is.na(rel_data$"counts"), ]
  rel_data <- rel_data[rel_data$"sample_id" %in% list_samples, ]

  col_names <- colnames(rel_data)

  y <- stats::aggregate(
    counts ~ subsample_id,
    data = rel_data,
    function(x) sum(x, na.rm = TRUE)
  )

  colnames(y)[2] <- "tot_subsample"

  rel_data <- merge(rel_data, y, by = "subsample_id")
  rel_data <- rel_data[, c(col_names, "tot_subsample")]

  col_names <- colnames(rel_data)

  y <- stats::aggregate(
    counts ~ sample_id,
    data = rel_data,
    function(x) sum(x, na.rm = TRUE)
  )

  colnames(y)[2] <- "tot_sample"

  rel_data <- merge(rel_data, y, by = "sample_id")
  rel_data <- rel_data[, c(col_names, "tot_sample")]

  cols_to_remove <- c(
    "sample_volume_filtered",
    "subsample_count_type",
    "subsample_all_shells_present_were_counted",
    "total_of_forams_counted_ind",
    "sampling_device_type"
  )

  rel_data <- rel_data[, !(colnames(rel_data) %in% cols_to_remove)]

  merged_frequency <- rbind(abs_data, rel_data)

  ## Compute metrics for messages ----

  missing_volume <- data[data$"subsample_count_type" != "Absolute", ]

  missing_volume <- tidyr::pivot_longer(
    data = missing_volume,
    cols = tidyr::all_of(taxa_cols),
    names_to = "taxa",
    values_to = "counts"
  )
  missing_volume <- missing_volume[
    is.na(missing_volume$"sample_volume_filtered"),
  ]

  missing_volume <- length(unique(missing_volume$"sample_id"))

  no_conversion <- data[
    data$"subsample_all_shells_present_were_counted" == 0,
  ]

  no_conversion <- length(unique(no_conversion$"sample_id"))

  message(
    "Counts from ",
    missing_volume,
    " samples could not be converted ",
    "because of missing volume data"
  )

  message(
    "Counts from ",
    no_conversion,
    " samples could not be converted ",
    "because of missing data on total assemblage"
  )

  partial_data <- merged_frequency
  partial_data[["counts"]] <-
    100 * partial_data$"counts" / partial_data$"tot_subsample"

  cols_to_remove <- c("tot_subsample", "tot_sample")

  partial_data <- partial_data[, !(colnames(partial_data) %in% cols_to_remove)]

  tot_data <- rbind(partial_data, raw_data)

  colnames(tot_data)[grep("^counts$", colnames(tot_data))] <-
    "counts_rel_ab"

  if (aggregate) {
    partial_data <- merged_frequency

    partial_data[["counts"]] <-
      100 * partial_data$"counts" / partial_data$"tot_sample"

    cols_to_remove <- c("tot_subsample", "tot_sample")

    partial_data <- partial_data[,
      !(colnames(partial_data) %in% cols_to_remove)
    ]

    tot_data <- rbind(partial_data, raw_data)

    cols_to_remove <- c(
      "subsample_id",
      "subsample_size_fraction_max",
      "subsample_size_fraction_min"
    )

    tot_data <- tot_data[, !(colnames(tot_data) %in% cols_to_remove)]

    tot_data <- tot_data[!duplicated(tot_data), ]

    colnames(tot_data)[grep("^counts$", colnames(tot_data))] <-
      "counts_rel_ab"
  }

  tibble::as_tibble(tot_data)
}
