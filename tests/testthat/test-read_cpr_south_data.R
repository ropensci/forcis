## Data for tests ----

df <- data.frame("crassula" = rep(1, 2), "dehiscens" = rep(1, 2))


## read_cpr_south_data() ----

test_that("Test read_cpr_south_data() for error", {
  create_tempdir()

  expect_error(
    read_cpr_south_data(version = "07"),
    paste0(
      "The directory './forcis-db/version-07' does not exist. ",
      "Please check the argument 'path' or use the function ",
      "'download_forcis_db()'."
    ),
    fixed = TRUE
  )

  dir.create(file.path("forcis-db", "version-07"), recursive = TRUE)

  expect_error(
    read_cpr_south_data(version = "07"),
    paste0(
      "The South CPR dataset does not exist. Please use the ",
      "function 'download_forcis_db()'."
    ),
    fixed = TRUE
  )
})

test_that("Test read_cpr_south_data() for success", {
  create_tempdir()

  dir.create(file.path("forcis-db", "version-07"), recursive = TRUE)

  write.csv2(
    df,
    file = file.path("forcis-db", "version-07", "FORCIS_cpr_south_test.csv"),
    row.names = FALSE
  )

  x <- read_cpr_south_data(version = "07", check_for_update = FALSE)

  expect_true("data.frame" %in% class(x))
  expect_equal(ncol(x), 3L)
  expect_equal(nrow(x), 2L)

  expect_true("data_type" %in% colnames(x))
  expect_true("CPR South" %in% x$"data_type")

  expect_message(
    {
      x <- read_cpr_south_data(version = "07")
    },
    paste0(
      "A newer version of the FORCIS database is available. ",
      "Use 'download_forcis_db(version = NULL)' to download ",
      "it."
    ),
    fixed = TRUE
  )
})
