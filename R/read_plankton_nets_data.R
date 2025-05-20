#' Read FORCIS data
#'
#' @description
#' These functions read one specific `csv` file of the FORCIS database
#' (see below) stored in the folder `path`. The function [download_forcis_db()]
#' must be used first to store locally the database.
#'
#' @param path a `character` of length 1. The folder in which the FORCIS
#'   database has been saved.
#'
#' @inheritParams download_forcis_db
#'
#' @details
#'
#' - `read_plankton_nets_data()` reads the FORCIS plankton nets data
#' - `read_pump_data()` reads the FORCIS pump data
#' - `read_cpr_north_data()` reads the FORCIS CPR North data
#' - `read_cpr_south_data()` reads the FORCIS CPR South data
#' - `read_sediment_trap_data()` reads the FORCIS sediment traps data
#'
#' @return A `tibble`. See
#'   \url{https://zenodo.org/doi/10.5281/zenodo.7390791} for a preview of the
#'   datasets.
#'
#' @seealso [download_forcis_db()] to download the complete FORCIS database.
#'
#' @name read_data
#'
#' @examples
#' \donttest{
#' # Folder in which the database will be saved ----
#' # N.B. In this example we use a temporary folder but you should select an
#' # existing folder (for instance "data/").
#' path <- tempdir()
#'
#' # Download the database ----
#' download_forcis_db(path, timeout = 300)
#'
#' # Import plankton nets data ----
#' plankton_nets_data <- read_plankton_nets_data(path)
#' }

NULL


#' @rdname read_data
#' @export

read_plankton_nets_data <- function(
  path = ".",
  version = options()$"forcis_version",
  check_for_update = options()$"forcis_check_for_update"
) {
  data <- read_data_(
    check_file_fun = plankton_net_filename,
    data_msg = "Plankton net",
    path = path,
    version = version,
    check_for_update = check_for_update
  )

  data <- add_data_type(data, "Net")

  ## Check and convert columns ----

  taxa_columns <- get_species_names(data)

  for (i in seq_len(length(taxa_columns))) {
    data[, taxa_columns[i]] <- as.numeric(data[, taxa_columns[i]])
  }

  tibble::as_tibble(data)
}
