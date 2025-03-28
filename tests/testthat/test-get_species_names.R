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
  "n_pachyderma" = c(1:5),
  "conglobatus" = c(1:5),
  "g_rubescens" = c(1:5)
)


## get_species_names() ----

test_that("Test get_species_names() for success", {
  species <- get_species_names(df_cpr)

  expect_true(class(species) == "character")
  expect_true(length(species) == 3L)

  expect_equal(species[1], "n_pachyderma")
  expect_equal(species[2], "conglobatus")
  expect_equal(species[3], "g_rubescens")

  species <- get_species_names(df_net)

  expect_true(class(species) == "character")
  expect_true(length(species) == 3L)

  expect_equal(species[1], "n_pachyderma")
  expect_equal(species[2], "conglobatus")
  expect_equal(species[3], "g_rubescens")
})
