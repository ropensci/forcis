test_that("print_available_versions returns NULL invisibly", {
  # Mock get_available_versions
  stub(
    print_available_versions,
    "get_available_versions",
    data.frame(
      version = "01",
      access_right = "open",
      publication_date = "2022-01-01",
      stringsAsFactors = FALSE
    )
  )

  # Test that the function returns NULL invisibly
  expect_invisible(print_available_versions())
  expect_null(print_available_versions())
})

test_that("print_available_versions formats output correctly", {
  # Mock data matching real response format
  mock_versions <- data.frame(
    version = c("10", "09", "08", "01"),
    access_right = c("open", "open", "open", "restricted"),
    publication_date = c(
      "2024-07-11",
      "2024-07-08",
      "2024-02-09",
      "2022-12-02"
    ),
    stringsAsFactors = FALSE
  )

  # Mock get_available_versions
  stub(print_available_versions, "get_available_versions", mock_versions)

  # Capture the output and collapse into a single string
  output_lines <- capture.output(print_available_versions())
  output <- paste(output_lines, collapse = "\n")

  # Check overall format
  expect_true(grepl("^Available versions are:", output))

  # Check specific version entries
  expect_true(grepl("\"10\"\\s+\\(open - 2024-07-11\\)", output))
  expect_true(grepl("\"09\"\\s+\\(open - 2024-07-08\\)", output))
  expect_true(grepl("\"01\"\\s+\\(restricted - 2022-12-02\\)", output))

  # Check for commas in output
  expect_true(grepl(",", output))
})

test_that("print_available_versions handles empty version list", {
  # Mock empty data frame
  stub(
    print_available_versions,
    "get_available_versions",
    data.frame(
      version = character(0),
      access_right = character(0),
      publication_date = character(0),
      stringsAsFactors = FALSE
    )
  )

  # Capture the output
  output <- capture.output(print_available_versions())

  # Check output is just the header
  expect_equal(output, "Available versions are: ")
})

test_that("print_available_versions skips actual network calls during tests", {
  skip_on_cran()
  skip_if_offline()

  # Mock get_available_versions directly to avoid the need for network calls
  stub(
    print_available_versions,
    "get_available_versions",
    data.frame(
      version = c("10", "09"),
      access_right = c("open", "open"),
      publication_date = c("2024-07-11", "2024-07-08"),
      stringsAsFactors = FALSE
    )
  )

  # This test should now work without making an actual API call
  with_mock_api({
    expect_error(print_available_versions(), NA)
  })
})
