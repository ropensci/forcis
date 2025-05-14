#' Select a taxonomy in FORCIS data
#'
#' @description
#' Selects a taxonomy in FORCIS data. FORCIS database provides three different
#' taxonomies: `"LT"` (lumped taxonomy), `"VT"` (validated taxonomy) and `"OT"`
#' (original taxonomy). See \doi{10.1038/s41597-023-02264-2} for further
#' information.
#'
#' @param data a `tibble` or a `data.frame`. One obtained by `read_*_data()`
#'   functions.
#'
#' @param taxonomy a `character` of length 1. One among `"LT"`, `"VT"`, `"OT"`.
#'
#' @export
#'
#' @return A `tibble`.
#'
#' @examples
#' # Import example dataset ----
#' file_name <- system.file(file.path("extdata", "FORCIS_net_sample.csv"),
#'                          package = "forcis")
#'
#' net_data <- read.csv(file_name)
#'
#' # Dimensions of the data.frame ----
#' dim(net_data)
#'
#' # Select a taxonomy ----
#' net_data <- select_taxonomy(net_data, taxonomy = "VT")
#'
#' # Dimensions of the data.frame ----
#' dim(net_data)

select_taxonomy <- function(data, taxonomy) {
  ## Check args ----

  check_if_df(data)
  check_if_valid_taxonomy(taxonomy)

  ## Error for CPR North ----

  if (get_data_type(data) == "CPR North") {
    stop(
      paste0(
        "This function cannot be used with CPR North data. There is ",
        "no need to filter these data."
      ),
      call. = FALSE
    )
  }

  ## Remove species from others taxonomies ----

  species_to_del <- species_list()[
    which(species_list()[, "taxonomy"] != toupper(taxonomy)),
    "taxon"
  ]

  pos <- which(colnames(data) %in% species_to_del)

  if (length(pos) > 0) {
    data <- data[, -pos, drop = FALSE]
  }

  if (length(get_species_names(data)) == 0) {
    stop("No species match the desired taxonomy", call. = FALSE)
  }

  tibble::as_tibble(data)
}
