#' Download the FORCIS database
#'
#' @description
#' Downloads the entire FORCIS database as a collection of five `csv` files from
#' Zenodo (\url{https://zenodo.org/doi/10.5281/zenodo.7390791}). Additional
#' files will be also downloaded.
#'
#' @param path a `character` of length 1. The folder in which the FORCIS
#'   database will be saved. Note that a subdirectory will be created, e.g.
#'   `forcis-db/version-99/` (with `99` the version number).
#'
#' @param version a `character` of length 1. The version number (with two
#'   numbers, e.g. `08` instead of `8`) of the FORCIS database to use.
#'   Default is the latest version. Note that this argument can be handle with
#'   the global option `forcis_version`. For example, if user calls
#'   `options(forcis_version = "07")`, the version `07` will be used by default
#'   for the current R session. Note that it is recommended to use the latest
#'   version of the database.
#'
#' @param check_for_update a `logical`. If `TRUE` (default) the function will
#'   check if a newer version of the FORCIS database is available on Zenodo
#'   and will print an informative message. Note that this argument can be
#'   handle with the global option `forcis_check_for_update`. For example, if
#'   user calls `options(forcis_check_for_update = FALSE)`, the message to
#'   download the latest version will be disabled for the current R session.
#'
#' @param overwrite a `logical`. If `TRUE` it will override the downloaded
#'   files of the FORCIS database. Default is `FALSE`.
#'
#' @param timeout an `integer`. The timeout for downloading files from Zenodo.
#'   Default is `60`. This number can be increased for low Internet connection.
#'
#' @return No return value. The FORCIS files will be saved in the `path` folder.
#'
#' @details
#' The FORCIS database is regularly updated. The global structure of the tables
#' doesn’t change between versions but some bugs can be fixed and new records
#' can be added. This is why it is recommended to use the latest version of the
#' database. The package is designed to handle the versioning of the database on
#' Zenodo and will inform the user if a new version is available each time
#' he/she uses one of the `read_*_data()` functions.
#'
#' For more information, please read the vignette available at
#' \url{https://docs.ropensci.org/forcis/articles/database-versions.html}.
#'
#' @seealso [read_plankton_nets_data()] to import the FORCIS database.
#'
#' @references
#' Chaabane S, De Garidel-Thoron T, Giraud X, _et al._ (2023) The FORCIS
#' database: A global census of planktonic Foraminifera from ocean waters.
#' _Scientific Data_, 10, 354.
#' DOI: \doi{10.1038/s41597-023-02264-2}.
#'
#' @export
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
#' # Check the content of the folder ----
#' list.files(path, recursive = TRUE)
#' }

download_forcis_db <- function(
  path,
  version = options()$"forcis_version",
  check_for_update = options()$"forcis_check_for_update",
  overwrite = FALSE,
  timeout = 60
) {
  ## Check args ----

  check_if_character(path)
  check_version(version)

  ## Check/set version ----

  if (is.null(check_for_update)) {
    check_for_update <- TRUE
  }

  version <- set_version(version, ask = check_for_update)

  ## Create outputs directory if required ----

  path <- file.path(path, "forcis-db", paste0("version-", version))

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  ## Download files from Zenodo ----

  forcis_meta <- get_version_metadata(version = version)
  forcis_files <- forcis_meta$"files"

  for (i in seq_len(nrow(forcis_files))) {
    download_file(
      url = forcis_files[i, "self"],
      path = path,
      file = forcis_files[i, "key"],
      overwrite = overwrite,
      timeout = timeout
    )
  }

  invisible(NULL)
}
