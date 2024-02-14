## Data for tests ----

df <- data.frame(matrix(1:21, nrow = 1))
colnames(df) <- get_required_columns()

df_w_na <- rbind(df, df, df, df)
df_w_na[2, "site_lon_start_decimal"] <- NA
df_w_na[3, "site_lat_start_decimal"] <- NA
df_w_na[4, "site_lon_start_decimal"] <- NA
df_w_na[4, "site_lat_start_decimal"] <- NA



## data_to_sf() ----

test_that("Test data_to_sf() for success", {
  
  df_sf <- data_to_sf(df)
  
  expect_true(is.data.frame(df_sf))
  expect_true(inherits(df_sf, "sf"))
  expect_equal(ncol(df_sf), 20L)
  expect_equal(nrow(df_sf), 1L)
  
  expect_length(grep("robin", as.character(sf::st_crs(df_sf))), 1L)
  
  expect_equal(as.character(unique(sf::st_geometry_type(df_sf))), "POINT")
  
  df_sf <- data_to_sf(df_w_na)
  expect_equal(nrow(df_sf), 1L)
})
