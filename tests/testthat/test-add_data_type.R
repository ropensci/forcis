## Data for tests ----

df_w  <- data.frame("data_type" = rep("Net", 3),
                    "column"    = 1:3)

df_wo <- data.frame("column"    = 1:3)


## add_data_type() ----

test_that("Test add_data_type() for success", {
  
  df <- add_data_type(df_w, "Net")
  
  expect_true(is.data.frame(df))
  expect_true(class(df[ , 1]) == "character")
  
  expect_equal(ncol(df), 2L)
  expect_equal(nrow(df), 3L)
  
  expect_equal(unique(df[ , 1]), "Net")
  
  df <- add_data_type(df_w, "Pump")
  
  expect_true(is.data.frame(df))
  expect_true(class(df[ , 1]) == "character")
  
  expect_equal(ncol(df), 2L)
  expect_equal(nrow(df), 3L)
  
  expect_equal(unique(df[ , 1]), "Pump")
  
  df <- add_data_type(df_wo, "Pump")
  
  expect_true(is.data.frame(df))
  expect_true(class(df[ , 1]) == "character")
  
  expect_equal(ncol(df), 2L)
  expect_equal(nrow(df), 3L)
  
  expect_equal(unique(df[ , 1]), "Pump")
})
