skip_on_cran()

## Data for tests ----

with_mock_dir(
  test_path("mockdata", "v08"),
  {
    forcis_meta <- get_version_metadata(version = "08")
    forcis_files <- forcis_meta$"files"
  },
  simplify = FALSE
)

## download_forcis_db() ----

test_that("Test download_forcis_db() for success", {
  create_tempdir()

  dir.create(file.path("forcis-db", "version-08"), recursive = TRUE)

  for (i in c(1:3, 5:8)) {
    invisible(file.create(file.path(
      "forcis-db",
      "version-08",
      forcis_files[i, "key"]
    )))
  }
  with_mock_dir(
    test_path("mockdata", "all"),
    {
      # "download_forcis_db" call "set_version" which make an extra call
      # with different endpoint "all"
      expect_message(download_forcis_db(
        path = ".",
        version = "08",
        check_for_update = FALSE,
        overwrite = FALSE,
        timeout = 300
      ))
    },
    simplify = FALSE
  )
  expect_true(file.exists(file.path(
    "forcis-db",
    "version-08",
    forcis_files[4, "key"]
  )))
})
