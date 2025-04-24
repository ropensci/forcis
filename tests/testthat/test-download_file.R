#skip_on_cran()

## Data for tests ----

with_mocked_bindings(
  get_metadata = get_metadata_mock,
  {
    forcis_meta <- get_version_metadata(version = "08")
  }
)

forcis_files <- forcis_meta$"files"
forcis_files <- forcis_files[
  which(forcis_files$"key" == "FORCIS_taxonomy_levels.xlsx"),
]

forcis_url <- forcis_files$"self"
forcis_file <- forcis_files$"key"

## download_file() ----

# mock downloading a file by creating a dummy file
download_file_mock <- function(url, destfile, mode, ...) {
  file.create(destfile)
}

test_that("Test download_file() for success", {
  root_dir <- tempfile("download_file")
  dir.create(root_dir, recursive = TRUE, showWarnings = FALSE)

  with_mocked_bindings(
    utils_download_file = download_file_mock,
    {
      messages <- capture_messages(
        expect_invisible(download_file(
          url = forcis_url,
          path = root_dir,
          file = forcis_file,
          overwrite = FALSE,
          timeout = 300
        ))
      )
    }
  )

  expect_match(
    messages,
    "The file \'FORCIS_taxonomy_levels.xlsx\' has been successfully downloaded"
  )

  expect_true(file.exists(file.path(root_dir, forcis_file)))
})
