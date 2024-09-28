## Data for tests ----

req_cols <- get_required_columns()

df_net <- data.frame(matrix(1:(length(req_cols) + 4), nrow = 1))
colnames(df_net) <- c(req_cols, "n_pachyderma", "conglobatus", "g_rubescens",
                      "toto")

df_net$"data_type" <- "Net"

df_cpr <- data.frame(matrix(1:(length(req_cols) + 4), nrow = 1))
colnames(df_cpr) <- c(req_cols, "species", "count_bin_min", "count_bin_max",
                      "toto")

df_cpr$"data_type" <- "CPR North"


## convert_to_long_format() ----

test_that("Test convert_to_long_format() for error", {

  expect_error(convert_to_long_format(df_cpr),
               "This function is not designed to work with 'CPR North' data",
               fixed = TRUE)
})

test_that("Test convert_to_long_format() for success", {
  
  expect_silent(df <- convert_to_long_format(df_net))
  
  expect_true(is.data.frame(df))
  expect_equal(ncol(df), length(req_cols) + 2)
  expect_equal(nrow(df), 3L)
  
  expect_true("taxa" %in% colnames(df))
  expect_true("counts" %in% colnames(df))
  
  expect_false("n_pachyderma" %in% colnames(df))
})
