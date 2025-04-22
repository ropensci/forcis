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

  download_file_mock <- function(url, file, path, overwrite = FALSE, ...) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)

    mock_data <- paste(
      "ID;Date;Latitude;Longitude;Species1;Species2;Species3",
      "1;2020-01-01;45.5;-30.5;10;5;2",
      "2;2020-01-02;46.0;-31.0;8;7;3",
      sep = "\n"
    )
    dest_path <- file.path(path, file)
    log_message("Writing mock data to:", dest_path)
    writeLines(mock_data, dest_path)
    invisible(NULL)
  }

  # Always return TRUE for checksum verification
  verify_file_checksum_mock <- function(file_path, expected_checksum) {
    return(TRUE)
  }

  ## Load a dataset (cached = TRUE) ---

  with_mocked_bindings(
    get_metadata = get_metadata_mock,
    download_file = download_file_mock,
    verify_file_checksum = verify_file_checksum_mock,
    {
      result <- load_forcis("pump", path = root_dir)
      result_cache <- load_forcis("pump", path = root_dir)
    }
  )

  # Verify the result
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 8) # 7 columns + data_type

  # 2nd call (cache)
  expect_s3_class(result_cache, "tbl_df")
  expect_equal(nrow(result_cache), 2)
  expect_equal(ncol(result_cache), 8) # 7 columns + data_type

  # Check numeric conversion worked
  expect_type(result$Species1, "double")
  expect_equal(result$Species1, c(10, 8))

  # 2nd call (cache)
  expect_type(result_cache$Species1, "double")
  expect_equal(result_cache$Species1, c(10, 8))

  # Check cache
  cached_versions <- list_cached_versions(root_dir)
  expect_true(length(cached_versions) > 0)
  cached_versions_path <- get_data_dir(cached_versions[1], root_dir)
  cached_meta_path <- file.path(cached_versions_path, meta_cache_filename())

  # Meta file is cached
  expect_true(file.exists(cached_meta_path))

  # Check for specific dataset files (file that starts with "pump")
  cached_files <- list.files(cached_versions_path)
  forcis_datasets <- forcis_datasets_info()
  dataset_pump_prefix <- forcis_datasets$filename_prefixes()["pump"]
  dataset_files <- cached_files[grepl(
    paste0("^", dataset_pump_prefix),
    cached_files
  )]
  expect_true(
    length(dataset_files) > 0,
    info = "No dataset files with prefix 'pump' found in cache"
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
  expect_s3_class(result_no_cache, "tbl_df")
  expect_equal(nrow(result_no_cache), 2)
  expect_equal(ncol(result_no_cache), 8) # 7 columns + data_type

  # Check numeric conversion worked
  expect_type(result_no_cache$Species1, "double")
  expect_equal(result_no_cache$Species1, c(10, 8))

  # Check cache
  cached_versions <- list_cached_versions(root_dir)
  expect_true(length(cached_versions) > 0) # version dir should exist

  # Only dataset files & metadata should be deleted
  cached_versions_path <- get_data_dir(cached_versions[1], root_dir)
  cached_meta_path <- file.path(cached_versions_path, meta_cache_filename())

  # Meta file is cached
  expect_false(file.exists(cached_meta_path))

  # Check for specific dataset files (file that starts with "pump")
  cached_files <- list.files(cached_versions_path)
  dataset_files <- cached_files[grepl(
    paste0("^", dataset_pump_prefix),
    cached_files
  )]
  expect_false(
    length(dataset_files) > 0,
    info = "Dataset files with prefix 'pump' found in cache"
  )
})

# TODO: add more test cases (specific versions & datasets)
