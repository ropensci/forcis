## Data for tests ----

df <- data.frame("x" = 1:3, "y" = 4:6)

df_w_req_col <- data.frame(matrix(1:21, nrow = 1))
colnames(df_w_req_col) <- get_required_columns()

df_w_missing_req_col <- df_w_req_col[, -1]


## check_if_character() ----

test_that("Test check_if_character() for error", {
  expect_error(check_if_character(), "Argument '' is required", fixed = TRUE)

  expect_error(
    check_if_character(12),
    "Argument '12' must be a character",
    fixed = TRUE
  )

  expect_error(
    check_if_character(c("a", "b")),
    "Argument 'c(\"a\", \"b\")' must be of length 1",
    fixed = TRUE
  )
})

test_that("Test check_if_character() for success", {
  expect_invisible(check_if_character("string"))

  x <- check_if_character("string")
  expect_null(x)
})


## check_if_path_exists() ----

test_that("Test check_if_path_exists() for error", {
  expect_error(
    check_if_path_exists("path"),
    "The directory 'path' does not exist",
    fixed = TRUE
  )
})

test_that("Test check_if_path_exists() for success", {
  create_tempdir()

  expect_invisible(check_if_path_exists(getwd()))

  x <- check_if_path_exists(getwd())
  expect_null(x)

  dir.create("data")
  expect_invisible(check_if_path_exists("data"))
})


## check_if_df() ----

test_that("Test check_if_df() for error", {
  expect_error(check_if_df(), "Argument 'data' is required", fixed = TRUE)

  expect_error(
    check_if_df(NA),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    check_if_df(1:5),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    check_if_df(letters),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    check_if_df(TRUE),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    check_if_df(NULL),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    check_if_df(list()),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    check_if_df(matrix(1:4, ncol = 2)),
    "Argument 'data' must be a data.frame",
    fixed = TRUE
  )

  expect_error(
    check_if_df(data.frame()),
    "Argument 'data' must have at least one row",
    fixed = TRUE
  )
})

test_that("Test check_if_df() for success", {
  expect_invisible(check_if_df(df))

  x <- check_if_df(df)
  expect_null(x)
})


## check_field_in_data() ----

test_that("Test check_field_in_data() for error", {
  expect_error(
    check_field_in_data(df, "z"),
    "The column '\"z\"' is missing from 'data'",
    fixed = TRUE
  )
})

test_that("Test check_field_in_data() for success", {
  expect_invisible(check_field_in_data(df, "x"))

  x <- check_field_in_data(df, "y")
  expect_null(x)
})


## check_required_columns() ----

test_that("Test check_required_columns() for error", {
  expect_error(
    check_required_columns(df_w_missing_req_col),
    "Some required columns are absent from data",
    fixed = TRUE
  )
})

test_that("Test check_required_columns() for success", {
  expect_invisible(check_required_columns(df_w_req_col))

  x <- check_required_columns(df_w_req_col)
  expect_null(x)
})


## check_if_valid_taxonomy() ----

test_that("Test check_if_valid_taxonomy() for error", {
  expect_error(
    check_if_valid_taxonomy("AA"),
    "Bad taxonomy. Valid taxonomies names are: LT, VT, OT, ZZ",
    fixed = TRUE
  )
})

test_that("Test check_if_valid_taxonomy() for success", {
  expect_invisible(check_if_valid_taxonomy("LT"))

  x <- check_if_valid_taxonomy("vt")
  expect_null(x)
})


## check_unique_taxonomy() ----

test_that("Test check_unique_taxonomy() for error", {
  df <- data.frame(df_w_req_col, "d_anfracta_VT" = 1, "ungulata" = 1)
  df$"data_type" <- "CPR South"

  expect_error(
    check_unique_taxonomy(df),
    paste0(
      "Multiple taxonomies are not allowed. Please use the ",
      "function 'select_taxonomy()' before going any further"
    ),
    fixed = TRUE
  )
})

test_that("Test check_unique_taxonomy() for success", {
  df <- df_w_req_col
  df$"data_type" <- "CPR North"

  expect_invisible(check_unique_taxonomy(df))
  x <- check_unique_taxonomy(df)
  expect_null(x)

  df <- data.frame(df_w_req_col, "benthics" = 1, "ungulata" = 1)
  df$"data_type" <- "CPR South"

  expect_invisible(check_unique_taxonomy(df))
  x <- check_unique_taxonomy(df)
  expect_null(x)

  df$"data_type" <- "Net"

  expect_invisible(check_unique_taxonomy(df))
  x <- check_unique_taxonomy(df)
  expect_null(x)

  df$"data_type" <- "Pump"

  expect_invisible(check_unique_taxonomy(df))
  x <- check_unique_taxonomy(df)
  expect_null(x)

  df$"data_type" <- "Sediment trap"

  expect_invisible(check_unique_taxonomy(df))
  x <- check_unique_taxonomy(df)
  expect_null(x)
})
