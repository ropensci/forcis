with_mock_dir("tmp/download_file", {
  skip_on_cran()

  ## Data for tests ----

  forcis_meta <- get_version_metadata(version = "08")
  forcis_files <- forcis_meta$"files"
  forcis_files <- forcis_files[
    which(forcis_files$"key" == "FORCIS_taxonomy_levels.xlsx"),
  ]

  forcis_url <- forcis_files$"self"
  forcis_file <- forcis_files$"key"

  ## download_file() ----

  test_that("Test download_file() for success", {
    create_tempdir()

    expect_invisible(download_file(
      url = forcis_url,
      path = ".",
      file = forcis_file,
      overwrite = FALSE,
      timeout = 300
    ))

    expect_true(file.exists(forcis_file))
  })
})
