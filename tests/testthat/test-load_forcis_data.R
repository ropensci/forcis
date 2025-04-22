MOCK_CSV_DATA <- paste(
  "ID;Date;Latitude;Longitude;Species1;Species2;Species3",
  "1;2020-01-01;45.5;-30.5;10;5;2",
  "2;2020-01-02;46.0;-31.0;8;7;3",
  sep = "\n"
)

forcis_datasets <- forcis_datasets_info()

## Helper functions ---

# mock downloading a file by creating a dummy file
download_file_mock <- function(url, file, path, overwrite = FALSE, ...) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  dest_path <- file.path(path, file)
  log_message("Writing mock data to:", dest_path)
  writeLines(MOCK_CSV_DATA, dest_path)
  invisible(NULL)
}

# always return TRUE for checksum verification
verify_file_checksum_mock <- function(file_path, expected_checksum) {
  return(TRUE)
}

# set of data checks for a dataset
verify_result <- function(
  result,
  expected_rows,
  expected_cols,
  expected_species1
) {
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), expected_rows)
  expect_equal(ncol(result), expected_cols)
  expect_type(result$Species1, "double")
  expect_equal(result$Species1, expected_species1)
}

# set of cache checks for a dataset
verify_dataset_cache <- function(
  root_dir,
  dataset_prefix,
  expect_files_exist = TRUE
) {
  cached_versions <- list_cached_versions(root_dir)
  expect_true(length(cached_versions) > 0)

  cached_versions_path <- get_data_dir(cached_versions[1], root_dir)
  cached_meta_path <- file.path(cached_versions_path, meta_cache_filename())
  expect_equal(file.exists(cached_meta_path), expect_files_exist)

  cached_files <- list.files(cached_versions_path)
  dataset_files <- cached_files[grepl(
    paste0("^", dataset_prefix),
    cached_files
  )]
  expect_equal(length(dataset_files) > 0, expect_files_exist)
}

## Test cases ---

# Test loading a dataset (latest)
test_that("Test load_forcis (latest dataset version) for success", {
  root_dir <- tempfile("load_forcis_root")

  log_message("load_forcis_root :", root_dir)

  # Mock the get_metadata function
  mock_metadata_env <- new.env()

  with_mock_dir(
    test_path("mockdata", "latest"),
    {
      mock_metadata_env$metadata <- get_metadata()
    },
    simplify = FALSE
  )

  # Mock the get_metadata function to return our controlled metadata
  get_metadata_mock <- function(...) {
    return(mock_metadata_env$metadata)
  }

  ## Load a dataset (cached = TRUE) ---

  with_mocked_bindings(
    get_metadata = get_metadata_mock,
    download_file = download_file_mock,
    verify_file_checksum = verify_file_checksum_mock,
    {
      messages <- capture_messages({
        result <- load_forcis("pump", path = root_dir)
      })
      messages_cache <- capture_messages({
        result_cache <- load_forcis("pump", path = root_dir)
      })
    }
  )

  # dataset filename prefix
  dataset_pump_prefix <- forcis_datasets$filename_prefixes()["pump"]

  # Expect a message for a missing file
  expect_match(
    messages,
    paste0("File ", dataset_pump_prefix, ".*\\.csv is missing, downloading...")
  )

  # Verify the result
  verify_result(result, 2, 8, c(10, 8))

  # Expect no messages for the 2nd call
  expect_length(messages_cache, 0)

  # 2nd call (cache)
  verify_result(result_cache, 2, 8, c(10, 8))

  # Check cache
  verify_dataset_cache(
    root_dir,
    dataset_pump_prefix,
    expect_files_exist = TRUE
  )

  ## Load a dataset (cached = FALSE) ---

  with_mocked_bindings(
    get_metadata = get_metadata_mock,
    download_file = download_file_mock,
    verify_file_checksum = verify_file_checksum_mock,
    {
      result_no_cache <- load_forcis(
        "pump",
        version = "latest",
        path = root_dir,
        cached = FALSE
      )
    }
  )

  # Verify the result
  verify_result(result_no_cache, 2, 8, c(10, 8))

  # Check cache (Only dataset files & metadata should be deleted)
  verify_dataset_cache(
    root_dir,
    dataset_pump_prefix,
    expect_files_exist = FALSE
  )
})

# Test loading a dataset (version 08)
test_that("Test load_forcis (specific dataset version) for success", {
  root_dir <- tempfile("load_forcis_root")

  log_message("load_forcis_root :", root_dir)

  # Mock the get_metadata function
  mock_metadata_env <- new.env()

  with_mock_dir(
    test_path("mockdata", "v08"),
    {
      mock_metadata_env$metadata <- get_metadata("08")
    },
    simplify = FALSE
  )

  # Mock the get_metadata function to return our controlled metadata
  get_metadata_mock <- function(...) {
    return(mock_metadata_env$metadata)
  }

  ## Load a dataset (cached = TRUE) ---

  with_mocked_bindings(
    get_metadata = get_metadata_mock,
    download_file = download_file_mock,
    verify_file_checksum = verify_file_checksum_mock,
    {
      messages <- capture_messages({
        result <- load_forcis("net", version = "08", path = root_dir)
      })
      messages_cache <- capture_messages({
        result_cache <- load_forcis("net", version = "08", path = root_dir)
      })
    }
  )

  # dataset filename prefix
  dataset_net_prefix <- forcis_datasets$filename_prefixes()["net"]

  # Expect a message for a missing file
  expect_match(
    messages,
    paste0("File ", dataset_net_prefix, ".*\\.csv is missing, downloading...")
  )

  # Verify the result
  verify_result(result, 2, 8, c(10, 8))

  # Expect no messages for the 2nd call
  expect_length(messages_cache, 0)

  # 2nd call (cache)
  verify_result(result_cache, 2, 8, c(10, 8))

  # Check cache
  verify_dataset_cache(
    root_dir,
    dataset_net_prefix,
    expect_files_exist = TRUE
  )

  ## Load a dataset (cached = FALSE) ---

  with_mocked_bindings(
    get_metadata = get_metadata_mock,
    download_file = download_file_mock,
    verify_file_checksum = verify_file_checksum_mock,
    {
      warnings_no_cache <- capture_warnings({
        messages_no_cache <- capture_messages({
          result_no_cache <- load_forcis(
            "net",
            version = "08",
            path = root_dir,
            cached = FALSE
          )
        })
      })
    }
  )

  # Expect warning: missing meta cache (already deleted at the start).
  # Cache cleaning done at start/end when cached=FALSE.
  expect_match(
    warnings_no_cache,
    paste0("File not found in cache: zenodo_metadata.rds")
  )

  # Expect a message for a missing file
  expect_match(
    messages_no_cache[3],
    paste0("File ", dataset_net_prefix, ".*\\.csv is missing, downloading...")
  )

  # Verify the result
  verify_result(result_no_cache, 2, 8, c(10, 8))

  # Check cache (Only dataset files & metadata should be deleted)
  verify_dataset_cache(
    root_dir,
    dataset_net_prefix,
    expect_files_exist = FALSE
  )
})

# TODO: add more test cases (extreme cases)
# Version dosen't exist
# Version exists but not open
# Dataset name not defined
# Missing columns (cpr_north o_O)
