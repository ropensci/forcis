## Data for tests ----

df_net <- data.frame(
  "data_type" = rep("Net", 6),
  "profile_date_time" = c(
    "01/02/1999",
    "01/03/2020",
    "01/03/2022",
    "01/04/2023",
    "01/07/2024",
    NA
  )
)

df_trap <- data.frame(
  "data_type" = rep("Sediment trap", 6),
  "sample_date_time_start" = c(
    "01/02/1999",
    "01/03/2020",
    "01/03/2022",
    "01/04/2023",
    "01/07/2024",
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


## filter_by_month() ----

test_that("Test filter_by_month() for error", {
  expect_error(
    filter_by_month(df_net),
    "Argument 'months' is required",
    fixed = TRUE
  )

  expect_error(
    filter_by_month(df_net, months = "02"),
    "Argument 'months' must be a numeric of length >= 1",
    fixed = TRUE
  )

  expect_error(
    filter_by_month(df_net, months = 1),
    "The months provided are out of FORCIS temporal range",
    fixed = TRUE
  )

  expect_error(
    filter_by_month(df_net, months = 10:11),
    "The months provided are out of FORCIS temporal range",
    fixed = TRUE
  )

  expect_error(
    filter_by_month(df_trap, months = 1),
    "The months provided are out of FORCIS temporal range",
    fixed = TRUE
  )

  expect_error(
    filter_by_month(df_trap, months = 10:11),
    "The months provided are out of FORCIS temporal range",
    fixed = TRUE
  )

  expect_error(
    filter_by_month(df_trap_na, months = 1),
    "The column 'sample_date_time_start' contain only NA",
    fixed = TRUE
  )

  expect_error(
    filter_by_month(df_net_na, months = 1),
    "The column 'profile_date_time' contain only NA",
    fixed = TRUE
  )
})

test_that("Test filter_by_month() for success", {
  expect_silent(df <- filter_by_month(df_net, months = 2))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_net))
  expect_equal(nrow(df), 1L)

  expect_silent(df <- filter_by_month(df_net, months = 2:4))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_net))
  expect_equal(nrow(df), 4L)

  expect_silent(df <- filter_by_month(df_net, months = c(2, 4)))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_net))
  expect_equal(nrow(df), 2L)

  expect_silent(df <- filter_by_month(df_net, months = 1:4))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_net))
  expect_equal(nrow(df), 4L)

  expect_silent(df <- filter_by_month(df_trap, months = 2))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_trap))
  expect_equal(nrow(df), 1L)

  expect_silent(df <- filter_by_month(df_trap, months = 2:4))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_trap))
  expect_equal(nrow(df), 4L)

  expect_silent(df <- filter_by_month(df_trap, months = c(2, 4)))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_trap))
  expect_equal(nrow(df), 2L)

  expect_silent(df <- filter_by_month(df_trap, months = 1:4))

  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_trap))
  expect_equal(nrow(df), 4L)
})
