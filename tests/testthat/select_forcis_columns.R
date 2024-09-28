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


## select_forcis_columns() ----

test_that("Test select_forcis_columns() for error", {
  
  expect_error(select_forcis_columns(df_net, col = 2),
               "Argument 'cols' must be a character vector",
               fixed = TRUE)
  
  expect_error(select_forcis_columns(df_net, col = "titi"),
               "Some columns to select are absent from data",
               fixed = TRUE)
})

test_that("Test select_forcis_columns() for success", {
  
  expect_silent(df <- select_forcis_columns(df_net))
  
  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_net) - 1)
  expect_equal(nrow(df), 1L)
  
  expect_silent(df <- select_forcis_columns(df_net, cols = "toto"))
  
  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_net))
  expect_equal(nrow(df), 1L)
  expect_equal(colnames(df)[length(req_cols) + 1], "toto")
  
  expect_silent(df <- select_forcis_columns(df_cpr))
  
  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_cpr) - 1)
  expect_equal(nrow(df), 1L)
  
  expect_silent(df <- select_forcis_columns(df_cpr, cols = "toto"))
  
  expect_true(is.data.frame(df))
  expect_equal(ncol(df), ncol(df_cpr))
  expect_equal(nrow(df), 1L)
  expect_equal(colnames(df)[length(req_cols) + 1], "toto")
})
