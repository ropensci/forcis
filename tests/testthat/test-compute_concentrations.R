## Data for test ----

df <- read.csv(
  system.file(
    file.path("extdata", "FORCIS_net_sample.csv"),
    package = "forcis"
  )
)

df <- add_data_type(df, "Net")
df <- select_taxonomy(df, "VT")

df2 <- df
df2 <- add_data_type(df2, "Sediment trap")

df3 <- vroom::vroom(
  system.file(
    file.path("extdata", "FORCIS_cpr_north_sample.csv"),
    package = "forcis"
  ),
  delim = ";",
  altrep = FALSE,
  show_col_types = FALSE
)

df3 <- add_data_type(df3, "CPR North")


## compute_concentrations() ----

test_that("Test compute_concentrations() for error", {
  expect_error(
    compute_concentrations(df2),
    paste0(
      "This function is not designed to work with ",
      "'Sediment trap' data"
    ),
    fixed = TRUE
  )
})

test_that("Test compute_concentrations() for success", {
  suppressMessages(res <- compute_concentrations(df))

  expect_true(is.data.frame(res))
  expect_equal(ncol(res), 19L)
  expect_equal(nrow(res), 39032L)

  suppressMessages(res <- compute_concentrations(df, aggregate = FALSE))

  expect_true(is.data.frame(res))
  expect_equal(ncol(res), 22L)
  expect_equal(nrow(res), 134680L)

  suppressMessages(res <- compute_concentrations(df3))

  expect_true(is.data.frame(res))
  expect_equal(ncol(res), 78L)
  expect_equal(nrow(res), 5L)
})
