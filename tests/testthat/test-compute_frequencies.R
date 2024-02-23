## Data for test ----

df <- vroom::vroom(system.file(file.path("extdata", "FORCIS_pump_sample.csv"),
                               package = "forcis"), 
                   delim = ";", altrep = FALSE, show_col_types = FALSE)

df <- add_data_type(df, "Pump")
df <- select_taxonomy(df, "VT")

df2 <- df
df2 <- add_data_type(df2, "CPR North")


## compute_frequencies() ----

test_that("Test compute_frequencies() for error", {
  
  expect_error(compute_frequencies(df2),
               paste0("This function is not designed to work with 'CPR North' ", 
                      "or 'Sediment trap' data"),
               fixed = TRUE)
})

test_that("Test compute_frequencies() for success", {
  
  expect_message(res <- compute_frequencies(df))
  
  expect_true(is.data.frame(res))
  expect_equal(ncol(res), 68L)
  expect_equal(nrow(res), 6832L)
  
  expect_message(res <- compute_frequencies(df, aggregate = FALSE))
  
  expect_true(is.data.frame(res))
  expect_equal(ncol(res), 71L)
  expect_equal(nrow(res), 6832L)
})
