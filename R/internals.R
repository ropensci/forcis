#' Download a csv file
#'
#' @noRd

download_file <- function(url, path, file, overwrite = FALSE, timeout = 60) {
  url <- utils::URLencode(url)
  file <- gsub("\\s", "_", file)

  check_if_path_exists(path)

  ## Check if the file already exists ----

  destination <- file.path(path, file)

  if (!overwrite && file.exists(destination)) {
    message(
      "The file '",
      file,
      "' already exists. If you want to download ",
      "again this file please use the argument 'overwrite'."
    )
    return(invisible(NULL))
  }

  ## Download the file if 'overwrite' is TRUE or it doesn't exist ----

  # change timeout for large file and slow connection
  user_opts <- options()
  on.exit(options(user_opts))
  options(timeout = max(timeout, getOption("timeout")))

  tryCatch(
    {
      utils::download.file(url = url, destfile = destination, mode = "wb")

      message("The file '", file, "' has been successfully downloaded")
    },
    error = function(e) {
      message("Download error: ", e$message)

      if (file.exists(destination)) {
        file.remove(destination)

        message("Temporary file deleted")
      }
    }
  )

  invisible(NULL)
}


#' Plankton nets file name
#'
#' @noRd

plankton_net_filename <- function() "FORCIS_net_"


#' Pumps file name
#'
#' @noRd

pump_filename <- function() "FORCIS_pump_"


#' CPR North file name
#'
#' @noRd

cpr_north_filename <- function() "FORCIS_cpr_north_"


#' CPR South file name
#'
#' @noRd

cpr_south_filename <- function() "FORCIS_cpr_south_"


#' Sediment Traps file name
#'
#' @noRd

sediment_trap_filename <- function() "FORCIS_trap_"


#' Vector of device types available in FORCIS database
#'
#' @noRd

data_types <- function()
  c("Net", "Pump", "Sediment trap", "CPR South", "CPR North")


#' Species names and taxonomy name
#'
#' @noRd

species_list <- function() {
  data.frame(
    "taxon" = c(
      "b_digitata_LT",
      "b_pumilio_LT",
      "b_variabilis_LT",
      "c_nitida_LT",
      "d_anfracta_LT",
      "g_adamsi_LT",
      "g_bulloides_LT",
      "g_calida_LT",
      "g_cavernula_LT",
      "g_conglobatus_LT",
      "g_conglomerata_LT",
      "g_crassaformis_LT",
      "g_falconensis_LT",
      "g_glutinata_LT",
      "g_hexagona_LT",
      "g_hirsuta_LT",
      "g_inflata_LT",
      "g_cultrata_LT",
      "g_minuta_LT",
      "g_ruber_any_LT",
      "g_rubescens_LT",
      "g_scitula_LT",
      "g_siphonifera_LT",
      "g_tenellus_LT",
      "g_theyeri_LT",
      "g_truncatulinoides_LT",
      "g_tumida_LT",
      "g_ungulata_LT",
      "g_uvula_LT",
      "n_vivans_LT",
      "h_digitata_LT",
      "h_pelagica_LT",
      "n_dutertrei_LT",
      "n_incompta_LT",
      "n_pachyderma_any_LT",
      "o_riedeli_LT",
      "o_universa_LT",
      "p_obliquiloculata_LT",
      "s_dehiscens_LT",
      "t_clarkei_LT",
      "t_fleisheri_LT",
      "t_humilis_LT",
      "t_iota_LT",
      "t_parkerae_LT",
      "t_quinqueloba_LT",
      "t_sacculifer_LT",
      "UniD_LT",

      "b_digitata_VT",
      "b_pumilio_VT",
      "b_variabilis_VT",
      "c_nitida_VT",
      "d_anfracta_VT",
      "g_adamsi_VT",
      "g_bulloides_VT",
      "g_calida_VT",
      "g_cavernula_VT",
      "g_conglobatus_VT",
      "g_conglomerata_VT",
      "g_crassaformis_VT",
      "g_elongatus_VT",
      "g_falconensis_VT",
      "g_glutinata_VT",
      "g_hexagona_VT",
      "g_hirsuta_VT",
      "g_inflata_VT",
      "g_cultrata_VT",
      "g_minuta_VT",
      "g_ruber_albus_VT",
      "g_ruber_albus_or_elongatus_VT",
      "g_ruber_any_VT",
      "g_ruber_ruber_VT",
      "g_rubescens_VT",
      "g_scitula_VT",
      "g_siphonifera_VT",
      "g_tenellus_VT",
      "g_theyeri_VT",
      "g_truncatulinoides_VT",
      "g_truncatulinoides_left_VT",
      "g_truncatulinoides_right_VT",
      "g_tumida_VT",
      "g_ungulata_VT",
      "g_uvula_VT",
      "n_vivans_VT",
      "h_digitata_VT",
      "h_pelagica_VT",
      "n_dutertrei_VT",
      "n_incompta_VT",
      "n_pachyderma_VT",
      "n_pachyderma_incompta_VT",
      "o_riedeli_VT",
      "o_universa_VT",
      "p_obliquiloculata_VT",
      "s_dehiscens_VT",
      "t_clarkei_VT",
      "t_fleisheri_VT",
      "t_humilis_VT",
      "t_iota_VT",
      "t_parkerae_VT",
      "t_quinqueloba_VT",
      "t_sacculifer_VT",
      "t_sacculifer_no_sac_VT",
      "t_sacculifer_sac_VT",
      "UnID_VT",

      "unidentified_specimens",
      "other",
      "pachyderma_incompta",
      "riedeli",
      "ruber__white",
      "ruber_alba",
      "ruber_sl",
      "ruber_ss",
      "g_ruber_white",
      "g_humilis",
      "g_anfracta",
      "g_siphonifera",
      "g_ruber_alba",
      "g_ruber_type_a",
      "ruber_type_b_and_c",
      "g_ruber_type_b_and_c",
      "g_elongatus",
      "globigerinella_spp",
      "aequilateralis",
      "aequilateralis__asym",
      "aequilateralis__sym",
      "cf_minuta",
      "cristata",
      "dutertrei__left",
      "elongatus",
      "d_anfracta",
      "g_radians",
      "g_bradyi",
      "g_crotonensis",
      "beella",
      "quinqueloba_sin",
      "g_dutertrei_left_",
      "g_menardii_tumida_bungulata",
      "g_ruber_sl",
      "g_pachyderma_incompta",
      "siphonifera",
      "sp",
      "g_sp",
      "humilis",
      "megastoma",
      "menardii__tumida___ungulata",
      "anfracta",
      "g_quinqueloba",
      "g_pachyderma",
      "n_pachyderma_nonencrusted",
      "n_pachyderma_encrusted",
      "g_pachyderma_left",
      "g_pachyderma_right",
      "g_aequilateralis",
      "g_aequilateralis_sym",
      "g_aequilateralis_asym",
      "g_sacculifer",
      "g_dutertrei",
      "g_dutertrei_right_",
      "g_truncatulinoides_r",
      "g_truncatulinoides_l",
      "g_menardii_neoflexulosa",
      "g_eggeri",
      "h_rhumbleri",
      "g_sacculifer_wo",
      "o_universa_spi",
      "o_universa_sph",
      "g_quinqueloba_egelida",
      "g_incompta",
      "g_iota",
      "pd_intergrade",
      "g_ruber_ss",
      "g_punctulata",
      "t_cocinnus",
      "orbulina",
      "s_globigerus",
      "scitula",
      "h_menardi_rt",
      "g_menardi_lt",
      "g_universa",
      "h_murrhayi",
      "t_compressa",
      "g_suleki",
      "suleki",
      "tenella",
      "theyeri",
      "trilobus",
      "g_bulloides",
      "hexagonus",
      "hirsuta",
      "incompta",
      "inflata",
      "iota",
      "menardii",
      "menardii_lt",
      "menardii_neoflexuosa",
      "menardii_rt",
      "minuta",
      "murrhayi",
      "nitida",
      "obliquiloculata",
      "pachyderma",
      "pachyderma_encrusted",
      "pachyderma_left",
      "pachyderma_nonencrusted",
      "pachyderma_right",
      "pachyderma_undifferentiated",
      "parkerae",
      "pelagica",
      "pumilio",
      "punctulata",
      "quadrilobatus",
      "quinqueloba",
      "quinqueloba_egelida",
      "rhumbleri",
      "ruber",
      "ruber__pink",
      "rubescens",
      "sacculifer",
      "sacculifer_w_sac",
      "sacculifer_wo",
      "truncatulinoides",
      "truncatulinoides__l",
      "truncatulinoides__r",
      "tumida",
      "g_inflata",
      "g_ruber",
      "g_glutinata",
      "g_conglobatus",
      "g_ruber_pink_",
      "o_universa",
      "h_pelagica",
      "g_truncatulinoides",
      "g_crassaformis",
      "g_menardii",
      "p_obliquiloculata",
      "c_nitida",
      "g_hirsuta",
      "g_scitula",
      "g_rubescens",
      "g_trilobus",
      "g_falconensis",
      "g_uvula",
      "g_conglomerata",
      "g_hexagona",
      "g_cf_minuta",
      "g_tumida",
      "s_dehiscens",
      "g_sacculife_w",
      "g_calida",
      "g_theyeri",
      "g_tenellus",
      "g_cavernula",
      "g_ungulata",
      "g_adamsi",
      "hexagonus_sp",
      "ruber_type_a",
      "g_conglomerata_and_hexagona",
      "t_parkerae",
      "g_digitata",
      "h_digitata",
      "bulloides",
      "calida",
      "cavernula",
      "clarkei",
      "compressa",
      "conglobatus",
      "conglomerata",
      "crassaformis",
      "crassula",
      "dehiscens",
      "dutertrei",
      "dutertrei__right",
      "eggeri",
      "falconensis",
      "fleisheri",
      "globigerus",
      "glutinata",
      "ungulata",
      "universa",
      "universa_sph",
      "universa_spi",
      "uvula",
      "vivans",
      "t_humilis",
      "t_iota",
      "g_minuta",
      "g_ruber_pink",
      "t_sacculifer",
      "g_tenella",
      "n_dutertrei",
      "n_incompta",
      "t_quinqueloba",
      "h_parapelagica",
      "quinqueloba_dex",
      "g_ca",
      "conglomerata_and_hexagona",
      "t_sacculifer_sac",
      "adamsi",
      "n_pachyderma",
      "g_ruber_ albus_or_ elongatus",
      paste0(
        "Globigerinoides_ruber_ruber_&Globigerinoides_ruber_albus&",
        "Globigerinoides_elongatus"
      ),
      "Globoturborotalita_rubescens&_Globigerinoides_tenellus",
      "benthics",
      "reworked_planktic_foraminifera",
      "g_ruber_.albus_or_.elongatus",
      paste0(
        "Globigerinoides_ruber_ruber_._Globigerinoides_ruber_albus_.",
        "_Globigerinoides_elongatus"
      ),
      "Globoturborotalita_rubescens_._Globigerinoides_tenellus",

      "forams_per_m3",
      "number_of_forams_per_1000m3",
      "number_of_species_counted_VT",
      "number_of_species_counted_LT"
    ),

    "taxonomy" = c(rep("LT", 47), rep("VT", 56), rep("OT", 203), rep("ZZ", 4))
  )
}


#' Record identifier in the Zenodo database
#'
#' @noRd

zenodo_id <- function() "7390791"


#' Date format used in raw data
#'
#' @noRd

date_format <- function() "%d/%m/%Y"


#' Retrieve data type
#'
#' @noRd

get_data_type <- function(data) {
  check_if_df(data)
  check_field_in_data(data, "data_type")

  data_type <- unique(data$"data_type")

  if (length(data_type) != 1) {
    stop(
      "The column 'data_type' cannot contain different values",
      call. = FALSE
    )
  }

  data_type
}


#' Check if a path exists
#'
#' @noRd

check_if_path_exists <- function(path) {
  check_if_character(path)

  if (!dir.exists(path)) {
    stop("The directory '", path, "' does not exist", call. = FALSE)
  }

  invisible(NULL)
}


#' Check for non-empty data.frame
#'
#' @noRd

check_if_df <- function(data) {
  if (missing(data)) {
    stop("Argument 'data' is required", call. = FALSE)
  }

  if (!is.data.frame(data)) {
    stop("Argument 'data' must be a data.frame", call. = FALSE)
  }

  if (!nrow(data)) {
    stop("Argument 'data' must have at least one row", call. = FALSE)
  }

  invisible(NULL)
}


#' Check if a column is present in a data.frame
#'
#' @noRd

check_field_in_data <- function(data, field) {
  check_if_df(data)
  check_if_character(field)

  if (!(field %in% colnames(data))) {
    stop(
      "The column '",
      deparse(substitute(field)),
      "' is missing from 'data'",
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Check for non-missing argument of type character and length 1
#'
#' @noRd

check_if_character <- function(str) {
  if (missing(str)) {
    stop("Argument '", deparse(substitute(str)), "' is required", call. = FALSE)
  }

  if (!is.character(str)) {
    stop(
      "Argument '",
      deparse(substitute(str)),
      "' must be a character",
      call. = FALSE
    )
  }

  if (length(str) != 1) {
    stop(
      "Argument '",
      deparse(substitute(str)),
      "' must be of length 1",
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Check for required columns
#'
#' @noRd

check_required_columns <- function(data) {
  check_if_df(data)

  if (any(!(get_required_columns() %in% colnames(data)))) {
    stop("Some required columns are absent from data", call. = FALSE)
  }

  invisible(NULL)
}


#' Check if a taxonomy name is valid
#'
#' @noRd

check_if_valid_taxonomy <- function(taxonomy) {
  check_if_character(taxonomy)
  taxonomy <- tolower(taxonomy)

  taxonomies <- unique(species_list()[, "taxonomy"])
  taxonomies <- tolower(taxonomies)

  if (!(taxonomy %in% taxonomies)) {
    stop(
      "Bad taxonomy. Valid taxonomies names are: ",
      toString(toupper(taxonomies)),
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Check for multiple taxonomies
#'
#' @noRd

check_unique_taxonomy <- function(data) {
  check_if_df(data)

  if (get_data_type(data) != "CPR North") {
    check_required_columns(data)

    pos <- which(species_list()[, "taxon"] %in% colnames(data))

    if (length(pos) > 0) {
      taxonomy <- unique(species_list()[pos, "taxonomy"])

      if (length(taxonomy) > 1) {
        stop(
          "Multiple taxonomies are not allowed. Please use the function ",
          "'select_taxonomy()' before going any further",
          call. = FALSE
        )
      }
    }
  }

  invisible(NULL)
}


#' Check Zenodo version
#'
#' @noRd

check_version <- function(version) {
  if (missing(version)) {
    stop("Argument 'version' is required", call. = FALSE)
  }

  if (!is.null(version)) {
    if (!is.character(version)) {
      stop("Argument 'version' must be character", call. = FALSE)
    }

    if (length(version) != 1) {
      stop("Argument 'version' must be character of length 1", call. = FALSE)
    }
  }

  invisible(NULL)
}


#' Set Zenodo version to latest is missing
#'
#' @noRd

set_version <- function(version, ask = TRUE) {
  check_version(version)

  versions <- get_available_versions()
  latest_version <- get_latest_version()

  if (is.null(version)) {
    version <- get_current_version()

    if (is.null(version)) {
      version <- latest_version
    }
  } else {
    if (!(version %in% versions$"version")) {
      stop(
        "The required version is not available. Please run ",
        "'get_available_versions()' to list available versions.",
        call. = FALSE
      )
    }
  }

  if (ask) {
    if (version != latest_version) {
      answer <- readline(paste0(
        "A newer version of the FORCIS database is ",
        "available. ",
        "Do you want to download it [Y/n]? "
      ))

      if (answer == "") answer <- "y"

      answer <- tolower(answer)

      if (!(answer %in% c("y", "n"))) {
        stop("Please type 'y' or 'n'", call. = FALSE)
      }

      if (answer == "y") {
        version <- latest_version
      }
    }
  }

  save_version(version)

  version
}


#' Get Zenodo latest version
#'
#' @noRd

get_latest_version <- function() {
  versions <- get_available_versions()

  versions[
    which.max(as.Date(versions$"publication_date")),
    "version",
    drop = TRUE
  ]
}


#' Robinson coordinate system
#'
#' @noRd

crs_robinson <- function() {
  paste0(
    "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84",
    " +units=m +no_defs"
  )
}


#' Set/update local database version number in an hidden file .forcis
#'
#' @noRd

save_version <- function(version) {
  saved_version <- get_current_version()

  if (is.null(saved_version) || saved_version != version) {
    version <- paste0("FORCIS_VERSION=", version)
    cat(version, file = ".forcis", append = FALSE, sep = "\n")
  }

  invisible(NULL)
}


#' Set/update local database version number in an hidden file .forcis
#'
#' @noRd

get_metadata <- function() {
  ## Prepare request ----

  endpoint <- "https://zenodo.org/api/records/"

  http_request <- httr2::request(endpoint) |>
    httr2::req_url_query(q = paste0("conceptrecid:", zenodo_id())) |>
    httr2::req_url_query(all_versions = "true")

  ## Send HTTP request  ----

  http_response <- httr2::req_perform(http_request)

  ## Check response status ----

  httr2::resp_check_status(http_response)

  ## Return content ----
  httr2::resp_body_json(http_response)
}


#' Add a column 'data_type' in data.frame (if required)
#'
#' @noRd

add_data_type <- function(data, type) {
  check_if_df(data)
  check_if_character(type)

  if ("data_type" %in% colnames(data)) {
    data$"data_type" <- type
  } else {
    data <- data.frame("data_type" = type, data)
  }

  data
}
