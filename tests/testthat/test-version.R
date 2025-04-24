## zenodo_id() ----

test_that("Test zenodo_id() for success", {
  x <- zenodo_id()

  expect_equal(x, "7390791")

  expect_equal(class(x), "character")
  expect_equal(length(x), 1L)
})


## check_version() ----

test_that("Test check_version() for error", {
  expect_error(check_version(), "Argument 'version' is required", fixed = TRUE)

  expect_error(
    check_version(NA),
    "Argument 'version' must be character",
    fixed = TRUE
  )

  expect_error(
    check_version(6),
    "Argument 'version' must be character",
    fixed = TRUE
  )

  expect_error(
    check_version(character(0)),
    "Argument 'version' must be character of length 1",
    fixed = TRUE
  )

  expect_error(
    check_version(c("06", "07")),
    "Argument 'version' must be character of length 1",
    fixed = TRUE
  )
})

test_that("Test check_version() for success", {
  x <- check_version(NULL)
  expect_null(x)

  x <- check_version("06")
  expect_null(x)

  expect_invisible(check_version("06"))
})


## get_metadata() ----

test_that("Test get_metadata() for success", {
  with_mocked_bindings(
    get_metadata = get_metadata_mock,
    {
      x <- get_metadata()
    }
  )

  expect_equal(class(x), "list")
  expect_true("hits" %in% names(x))
  expect_true(x$"hits"$"total" > 0L)
})


## get_available_versions() ----

test_that("Test get_available_versions() for success", {
  with_mocked_bindings(
    get_metadata = get_metadata_mock,
    {
      x <- get_available_versions()
    }
  )

  expect_true("data.frame" %in% class(x))
  expect_true(nrow(x) > 0L)
  expect_equal(ncol(x), 3L)

  expect_true("publication_date" %in% colnames(x))
  expect_true("version" %in% colnames(x))
  expect_true("access_right" %in% colnames(x))
})


## get_version_metadata() ----

test_that("Test get_version_metadata() for error", {
  with_mocked_bindings(
    get_metadata = get_metadata_mock,
    {
      expect_error(
        get_version_metadata(version = "999"),
        paste0(
          "The required version is not available. Please run ",
          "'get_available_versions()' to list available versions."
        ),
        fixed = TRUE
      )
    }
  )
})


test_that("Test get_version_metadata() for success", {
  with_mocked_bindings(
    get_metadata = get_metadata_mock,
    {
      x <- get_version_metadata(version = NULL)

      expect_equal(class(x), "list")
      expect_true("version" %in% names(x))
      expect_true("files" %in% names(x))

      x <- get_version_metadata(version = "08")

      expect_equal(class(x), "list")
      expect_true("version" %in% names(x))
      expect_true("files" %in% names(x))

      expect_true(x$"version" == "08")
      expect_true(x$"publication_date" == "2024-02-09")
    }
  )
})


## get_latest_version() ----

test_that("Test get_latest_version() for success", {
  with_mocked_bindings(
    get_metadata = get_metadata_mock,
    {
      x <- get_latest_version()
    }
  )

  expect_equal(class(x), "character")
})


## set_version() ----

test_that("Test set_version() for error", {
  with_mocked_bindings(
    get_metadata = get_metadata_mock,
    {
      expect_error(
        set_version(version = "999", ask = FALSE),
        paste0(
          "The required version is not available. Please run ",
          "'get_available_versions()' to list available versions."
        ),
        fixed = TRUE
      )
    }
  )
})


test_that("Test set_version() for success", {
  create_tempdir()

  with_mocked_bindings(
    get_metadata = get_metadata_mock,
    {
      y <- get_latest_version()
      x <- set_version(version = NULL, ask = FALSE)
    }
  )

  expect_equal(class(x), "character")
  expect_equal(length(x), 1L)
  expect_equal(x, y)
})


test_that("Test set_version() for success", {
  create_tempdir()

  with_mocked_bindings(
    get_metadata = get_metadata_mock,
    {
      x <- set_version(version = "07", ask = FALSE)

      expect_equal(class(x), "character")
      expect_equal(length(x), 1L)
      expect_equal(x, "07")

      y <- get_latest_version()

      x <- set_version(version = NULL, ask = FALSE)

      expect_equal(class(x), "character")
      expect_equal(length(x), 1L)
      expect_equal(x, y)
    }
  )
})
