test_that("clean_forcis_cache removes entire cache when version is NULL", {
  # Create a temporary root directory
  root_dir <- tempfile("forcis_test_root")
  dir.create(root_dir, recursive = TRUE)

  # Create the expected directory structure
  data_dir <- get_data_dir(path = root_dir)
  dir.create(data_dir, recursive = TRUE)

  # Create version directories
  v1_dir <- get_data_dir(version = "01", path = root_dir)
  v2_dir <- get_data_dir(version = "02", path = root_dir)
  dir.create(v1_dir)
  dir.create(v2_dir)

  # Add some test files
  writeLines("test", file.path(v1_dir, "data.csv"))
  writeLines("test", file.path(v2_dir, "metadata.json"))

  # Capture messages
  messages <- capture_messages({
    result <- clean_forcis_cache(path = root_dir)
  })

  # Check that function returns TRUE
  expect_true(result)

  # Check that correct message was output
  # expect_match(messages, paste0("Cleaned all cached data: ", data_dir))

  expect_true(startsWith(messages, "Cleaned all cached data:"))

  expect_true(grepl(basename(root_dir), messages, fixed = TRUE))

  expect_true(grepl("forcis[/\\\\]data", messages, perl = TRUE))


  # Check that directory was removed
  expect_false(dir.exists(data_dir))
})

test_that("clean_forcis_cache handles non-existent cache directory", {
  # Use a path that definitely doesn't exist
  non_existent_dir <- tempfile("forcis_nonexistent")

  # Capture messages
  messages <- capture_messages({
    result <- clean_forcis_cache(path = non_existent_dir)
  })

  # Check that function returns TRUE
  expect_true(result)

  # Check that correct message was output
  expect_match(messages, "Cache directory does not exist, nothing to clean")
})

test_that("clean_forcis_cache removes specific version when version is provided", {
  # Create a temporary root directory
  root_dir <- tempfile("forcis_test_root")

  # Create the expected directory structure
  data_dir <- get_data_dir(path = root_dir)
  dir.create(data_dir, recursive = TRUE)

  # Create version directories
  v1_dir <- get_data_dir(version = "01", path = root_dir)
  v2_dir <- get_data_dir(version = "02", path = root_dir)
  dir.create(v1_dir)
  dir.create(v2_dir)

  # Add some test files
  writeLines("test", file.path(v1_dir, "data.csv"))
  writeLines("test", file.path(v2_dir, "metadata.json"))

  # Capture messages
  messages <- capture_messages({
    result <- clean_forcis_cache(version = "01", path = root_dir)
  })

  # Check that function returns TRUE
  expect_true(result)

  # Check that correct message was output
  expect_match(messages, "Cleaned cache for version 01")

  # Check that only version 01 directory was removed
  expect_false(dir.exists(v1_dir))
  expect_true(dir.exists(v2_dir))
})

test_that("clean_forcis_cache handles non-existent version", {
  # Create a temporary root directory
  root_dir <- tempfile("forcis_test_root")

  # Create the expected directory structure
  data_dir <- get_data_dir(path = root_dir)
  dir.create(data_dir, recursive = TRUE)

  # Create version directory (but not version 99)
  v1_dir <- get_data_dir(version = "01", path = root_dir)
  dir.create(v1_dir)

  # Capture warnings
  warnings <- capture_warnings({
    result <- clean_forcis_cache(version = "99", path = root_dir)
  })

  # Check that function returns FALSE
  expect_false(result)

  # Check that correct warning was output
  expect_match(warnings, "Version 99 is not cached")
})

test_that("clean_forcis_cache removes specific files when filenames are provided", {
  # Create a temporary root directory
  root_dir <- tempfile("forcis_test_root")

  # Create the expected directory structure
  data_dir <- get_data_dir(path = root_dir)
  dir.create(data_dir, recursive = TRUE)

  # Create version directory
  v1_dir <- get_data_dir(version = "01", path = root_dir)
  dir.create(v1_dir)

  # Create test files
  file1 <- file.path(v1_dir, "data.csv")
  file2 <- file.path(v1_dir, "metadata.json")
  file3 <- file.path(v1_dir, "other.txt")

  writeLines("test", file1)
  writeLines("test", file2)
  writeLines("test", file3)

  # Capture messages
  messages <- capture_messages({
    result <- clean_forcis_cache(
      version = "01",
      filenames = c("data.csv", "metadata.json"),
      path = root_dir
    )
  })

  # Check that function returns TRUE
  expect_true(result)

  # Check that correct messages were output
  expect_match(messages[1], "Cleaned cache file: data.csv")
  expect_match(messages[2], "Cleaned cache file: metadata.json")

  # Check that specified files were removed
  expect_false(file.exists(file1))
  expect_false(file.exists(file2))

  # Check that other files remain
  expect_true(file.exists(file3))
})

test_that("clean_forcis_cache handles non-existent files", {
  # Create a temporary root directory
  root_dir <- tempfile("forcis_test_root")

  # Create the expected directory structure
  data_dir <- get_data_dir(path = root_dir)
  dir.create(data_dir, recursive = TRUE)

  # Create version directory
  v1_dir <- get_data_dir(version = "01", path = root_dir)
  dir.create(v1_dir)

  # Create a test file (but not nonexistent.json)
  file1 <- file.path(v1_dir, "data.csv")
  writeLines("test", file1)

  # Capture warnings and messages
  warnings <- capture_warnings({
    messages <- capture_messages({
      result <- clean_forcis_cache(
        version = "01",
        filenames = c("data.csv", "nonexistent.json"),
        path = root_dir
      )
    })
  })

  # Check that function returns TRUE
  expect_true(result)

  # Check correct message for existing file
  expect_match(messages, "Cleaned cache file: data.csv")

  # Check warning for non-existent file
  expect_match(warnings, "File not found in cache: nonexistent.json")

  # Check that existing file was removed
  expect_false(file.exists(file1))
})
