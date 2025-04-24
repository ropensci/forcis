# skip_on_cran()

## Data for tests ----
with_mocked_bindings(
  get_metadata = get_metadata_mock,
  {
    forcis_meta <- get_version_metadata(version = "08")
  }
)

forcis_files <- forcis_meta$"files"


## download_forcis_db() ----

test_that("Test download_forcis_db() for success", {
  root_dir <- tempfile("download_forcis_db")
  version_dir <- file.path(root_dir, "forcis-db", "version-08")

  dir.create(version_dir, recursive = TRUE)

  for (i in c(1:3, 5:8)) {
    invisible(file.create(file.path(
      version_dir,
      forcis_files[i, "key"]
    )))
  }

  # mock downloading a file by creating a dummy file
  download_file_mock <- function(url, destfile, mode) {
    file.create(destfile)
  }

  # Capture all messages
  with_mocked_bindings(
    get_metadata = get_metadata_mock,
    utils_download_file = download_file_mock,
    {
      messages <- capture_messages({
        download_forcis_db(
          path = root_dir,
          version = "08",
          check_for_update = FALSE,
          overwrite = FALSE,
          timeout = 300
        )
      })
    }
  )

  # Check file was downloaded
  expect_true(file.exists(file.path(
    version_dir,
    forcis_files[4, "key"]
  )))

  # Verify the successful download message
  expect_true(any(grepl(
    paste0(
      "The file '",
      forcis_files[4, "key"],
      "' has been successfully downloaded"
    ),
    messages
  )))

  # Verify "already exists" messages for other files
  for (i in c(1:3, 5:8)) {
    expect_true(any(grepl(
      paste0("The file '", forcis_files[i, "key"], "' already exists"),
      messages
    )))
  }
})
