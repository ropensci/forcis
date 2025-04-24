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
df2 <- add_data_type(df2, "CPR North")


## compute_abundances() ----

test_that("Test compute_abundances() for error", {
  expect_error(
    compute_abundances(df2),
    paste0(
      "This function is not designed to work with 'CPR North' ",
      "or 'Sediment trap' data"
    ),
    fixed = TRUE
  )
})

test_that("Test compute_abundances() for success", {
  expect_message(res <- compute_abundances(df))

  expect_true(is.data.frame(res))
  expect_equal(ncol(res), 19L)
  expect_equal(nrow(res), 39032L)

  expect_message(res <- compute_abundances(df, aggregate = FALSE))

  expect_true(is.data.frame(res))
  expect_equal(ncol(res), 22L)
  expect_equal(nrow(res), 134008L)
})
