## Data for tests ----

df <- data.frame(matrix(rep(1, 21), nrow = 1))
colnames(df) <- get_required_columns()

df <- rbind(df, df, df, df, df)

df[ , "site_lat_start_decimal"] <- c( 45.26,  34.18, -04.56, NA, -04.56)
df[ , "site_lon_start_decimal"] <- c(-34.25, -48.29,  74.01,  74.01, NA)

bbox_num <- c(-64.48, 16.05, -10.16, 54.08)

bbox <- sf::st_bbox(c(xmin = bbox_num[1], ymin = bbox_num[2], 
                      xmax = bbox_num[3], ymax = bbox_num[4]),
                    crs = sf::st_crs(4326))  

bbox_0 <- sf::st_bbox(c(xmin = bbox_num[1], ymin = bbox_num[2], 
                      xmax = bbox_num[3], ymax = bbox_num[4]),
                    crs = NA)  

bbox_num_2 <- c(64.48, 16.05, 10.16, 54.08)


## filter_by_bbox() ----

test_that("Test filter_by_bbox() for error", {
  
  expect_error(filter_by_bbox(df),
               "Argument 'bbox' is required", 
               fixed = TRUE)
  
  expect_error(filter_by_bbox(df, 1),
               "The object 'bbox' must a numeric of length 4 or a bbox object", 
               fixed = TRUE)
  
  expect_error(filter_by_bbox(df, iho_boundaries),
               "The object 'bbox' must a numeric of length 4 or a bbox object", 
               fixed = TRUE)
  
  expect_error(filter_by_bbox(df, bbox_0),
               "The object 'bbox' must have a CRS", 
               fixed = TRUE)
})


test_that("Test filter_by_bbox() for success", {
  
  expect_silent(df_sub <- filter_by_bbox(df, bbox_num))
  
  expect_true(is.data.frame(df_sub))
  expect_equal(ncol(df_sub), ncol(df))
  expect_equal(nrow(df_sub), 2L)
  
  expect_silent(df_sub <- filter_by_bbox(df, bbox))
  
  expect_true(is.data.frame(df_sub))
  expect_equal(ncol(df_sub), ncol(df))
  expect_equal(nrow(df_sub), 2L)
  
  expect_silent(df_sub <- filter_by_bbox(df, bbox_num_2))
  
  expect_true(is.data.frame(df_sub))
  expect_equal(ncol(df_sub), ncol(df))
  expect_equal(nrow(df_sub), 0L)
})
