## Data for tests ----

df <- data.frame(matrix(rep(1, 21), nrow = 1))
colnames(df) <- get_required_columns()

df <- rbind(df, df, df, df, df)

df[, "site_lat_start_decimal"] <- c(45.26, 34.18, -04.56, NA, -04.56)
df[, "site_lon_start_decimal"] <- c(-34.25, -48.29, 74.01, 74.01, NA)

pol <- iho_boundaries[1, ]

pol_1 <- pol
sf::st_crs(pol_1) <- NA

pts <- data_to_sf(df)


## filter_by_polygon() ----

test_that("Test filter_by_polygon() for error", {
  expect_error(
    filter_by_polygon(df),
    "Argument 'polygon' is required",
    fixed = TRUE
  )

  expect_error(
    filter_by_polygon(df, df),
    "Argument 'polygon' must be an 'sf' object",
    fixed = TRUE
  )

  expect_error(
    filter_by_polygon(df, pts),
    "Argument 'polygon' must be a 'POLYGON' or a 'MULTIPOLYGON'",
    fixed = TRUE
  )

  expect_error(
    filter_by_polygon(df, pol_1),
    "The object 'polygon' must have a CRS",
    fixed = TRUE
  )
})

test_that("Test filter_by_polygon() for success", {
  expect_silent(df_sub <- filter_by_polygon(df, iho_boundaries[2, ]))

  expect_true(is.data.frame(df_sub))
  expect_equal(ncol(df_sub), ncol(df))
  expect_equal(nrow(df_sub), 1L)

  expect_silent(df_sub <- filter_by_polygon(df, iho_boundaries[4, ]))

  expect_true(is.data.frame(df_sub))
  expect_equal(ncol(df_sub), ncol(df))
  expect_equal(nrow(df_sub), 2L)

  expect_silent(df_sub <- filter_by_polygon(df, iho_boundaries[1, ]))

  expect_true(is.data.frame(df_sub))
  expect_equal(ncol(df_sub), ncol(df))
  expect_equal(nrow(df_sub), 0L)
})
