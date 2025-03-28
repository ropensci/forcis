## Data for tests ----

df_net <- data.frame(
  "data_type" = rep("Net", 6),
  "profile_date_time" = c(
    "01/01/1999",
    "01/01/2020",
    "01/01/2022",
    "01/01/2023",
    "01/01/2024",
    NA
  )
)

df_trap <- data.frame(
  "data_type" = rep("Sediment trap", 6),
  "sample_date_time_start" = c(
    "01/01/1999",
    "01/01/2020",
    "01/01/2022",
    "01/01/2023",
    "01/01/2024",
    NA
  )
)

df_trap_na <- data.frame(
  "data_type" = rep("Sediment trap", 6),
  "sample_date_time_start" = rep(NA, 6)
)


df_net_na <- data.frame(
  "data_type" = rep("Net", 6),
  "profile_date_time" = rep(NA, 6)
)


## filter_by_year() ----

test_that("Test filter_by_year() for error", {
  expect_error(
    filter_by_year(df_net),
    "Argument 'years' is required",
    fixed = TRUE
  )

  expect_error(
    filter_by_year(df_net, year = "2000"),
    "Argument 'years' must be a numeric of length >= 1",
    fixed = TRUE
  )

  expect_error(
    filter_by_year(df_net, year = 2000),
    "The years provided are out of FORCIS temporal range",
    fixed = TRUE
  )

  expect_error(
    filter_by_year(df_net, year = 2000:2019),
    "The years provided are out of FORCIS temporal range",
    fixed = TRUE
  )

  expect_error(
    filter_by_year(df_trap, year = 2000),
    "The years provided are out of FORCIS temporal range",
    fixed = TRUE
  )

  expect_error(
    filter_by_year(df_trap, year = 2000:2019),
    "The years provided are out of FORCIS temporal range",
    fixed = TRUE
  )

  expect_error(
    filter_by_year(df_trap_na, year = 2000),
    "The column 'sample_date_time_start' contain only NA",
    fixed = TRUE
  )

  expect_error(
    filter_by_year(df_net_na, year = 2000),
    "The column 'profile_date_time' contain only NA",
    fixed = TRUE
  )
})

test_that("Test filter_by_year() for success", {
  expect_silent(df <- filter_by_year(df_net, years = 2022))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_net))
  expect_equal(nrow(df), 1L)

  expect_silent(df <- filter_by_year(df_net, years = 2022:2024))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_net))
  expect_equal(nrow(df), 3L)

  expect_silent(df <- filter_by_year(df_net, years = c(2022, 2024)))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_net))
  expect_equal(nrow(df), 2L)

  expect_silent(df <- filter_by_year(df_net, years = 2000:2022))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_net))
  expect_equal(nrow(df), 2L)

  expect_silent(df <- filter_by_year(df_trap, years = 2022))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_trap))
  expect_equal(nrow(df), 1L)

  expect_silent(df <- filter_by_year(df_trap, years = 2022:2024))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_trap))
  expect_equal(nrow(df), 3L)

  expect_silent(df <- filter_by_year(df_trap, years = c(2022, 2024)))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_trap))
  expect_equal(nrow(df), 2L)

  expect_silent(df <- filter_by_year(df_trap, years = 2000:2022))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_trap))
  expect_equal(nrow(df), 2L)
})
