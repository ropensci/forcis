#' Get required column names
#'
#' @description
#' Gets required column names (except taxa names) for the package. This
#' function is designed to help users to add additional columns in
#' [select_forcis_columns()] (argument `cols`) if missing from this list.
#'
#' These columns are required by some functions (`compute_*()`, `plot_*()`,
#' etc.) of the package and shouldn't be deleted.
#'
#' @export
#'
#' @return A `character` vector.
#'
#' @examples
#' # Get required column names (expect taxa names) ----
#' get_required_columns()

get_required_columns <- function() {
  c(
    "data_type",
    "cruise_id",
    "profile_id",
    "sample_id",
    "sample_min_depth",
    "sample_max_depth",
    "profile_depth_min",
    "profile_depth_max",
    "profile_date_time",
    "cast_net_op_m2",
    "subsample_id",
    "sample_segment_length",
    "subsample_count_type",
    "subsample_size_fraction_min",
    "subsample_size_fraction_max",
    "site_lat_start_decimal",
    "site_lon_start_decimal",
    "sample_volume_filtered",
    "subsample_all_shells_present_were_counted",
    "total_of_forams_counted_ind",
    "sampling_device_type"
  )
}
