## Data for tests ----

df <- data.frame(matrix(rep(1, 21), nrow = 1))
colnames(df) <- get_required_columns()

df <- rbind(df, df, df, df, df)

df[, "site_lat_start_decimal"] <- c(45.26, 34.18, -04.56, NA, -04.56)
df[, "site_lon_start_decimal"] <- c(-34.25, -48.29, 74.01, 74.01, NA)


## filter_by_ocean() ----

test_that("Test filter_by_ocean() for error", {
  expect_error(
    filter_by_ocean(df),
    "Argument 'ocean' is required",
    fixed = TRUE
  )

  expect_error(
    filter_by_ocean(df, 1),
    "Argument 'ocean' must be a character of length >= 1",
    fixed = TRUE
  )

  expect_error(
    filter_by_ocean(df, "Indian"),
    paste0(
      "Some ocean names are mispelled. Please use ",
      "'get_ocean_names()' to find the correct spelling"
    ),
    fixed = TRUE
  )

  expect_error(
    filter_by_ocean(df, c("Indian", "Arctic Ocean")),
    paste0(
      "Some ocean names are mispelled. Please use ",
      "'get_ocean_names()' to find the correct spelling"
    ),
    fixed = TRUE
  )
})


test_that("Test filter_by_ocean() for success", {
  expect_silent(df_sub <- filter_by_ocean(df, "Indian Ocean"))

  expect_true(is.data.frame(df_sub))
  expect_equal(ncol(df_sub), ncol(df))
  expect_equal(nrow(df_sub), 1L)

  expect_silent(df_sub <- filter_by_ocean(df, "North Atlantic Ocean"))

  expect_true(is.data.frame(df_sub))
  expect_equal(ncol(df_sub), ncol(df))
  expect_equal(nrow(df_sub), 2L)

  expect_silent(df_sub <- filter_by_ocean(df, "South Atlantic Ocean"))

  expect_true(is.data.frame(df_sub))
  expect_equal(ncol(df_sub), ncol(df))
  expect_equal(nrow(df_sub), 0L)
})
