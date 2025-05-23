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
