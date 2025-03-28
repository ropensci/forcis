## Data for tests ----

df_cpr <- data.frame(
  "data_type" = rep("CPR North", 5),
  "species" = c(
    "n_pachyderma",
    "n_pachyderma",
    "conglobatus",
    "g_rubescens",
    "g_rubescens"
  )
)

df_net <- data.frame(
  "data_type" = rep("Net", 5),
  "t_parkerae_VT" = c(1:5),
  "conglobatus" = c(1:5),
  "g_rubescens" = c(1:5)
)


## select_taxonomy() ----

test_that("Test get_species_names() for error", {
  expect_error(
    select_taxonomy(df_cpr, "VT"),
    paste0(
      "This function cannot be used with CPR North data. ",
      "There is no need to filter these data."
    ),
    fixed = TRUE
  )

  expect_error(
    select_taxonomy(df_net, "LT"),
    "No species match the desired taxonomy",
    fixed = TRUE
  )
})

test_that("Test get_species_names() for success", {
  expect_silent(df <- select_taxonomy(df_net, "OT"))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), 3L)
  expect_equal(nrow(df), 5L)

  expect_silent(df <- select_taxonomy(df_net, "VT"))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), 2L)
  expect_equal(nrow(df), 5L)
})
