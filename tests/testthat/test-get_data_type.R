## Data for tests ----

df_g <- data.frame("data_type" = rep("Net", 3),
                   "column"    = 1:3)

df_w <- data.frame("data_type" = c("Net", "Net", "Pump"),
                   "column"    = 1:3)


## get_data_type() ----

test_that("Test get_data_type() for error", {
  
  expect_error(get_data_type(df_w),
               "The column 'data_type' cannot contain different values",
               fixed = TRUE)
})

test_that("Test get_data_type() for success", {
  
  dt <- get_data_type(df_g)
  
  expect_true(is.character(dt))
  expect_equal(length(dt), 1L)
  expect_equal(dt, "Net")
})
