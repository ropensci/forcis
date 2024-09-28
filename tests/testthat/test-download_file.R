## Data for tests ----

forcis_meta  <- get_version_metadata(version = "08")
forcis_files <- forcis_meta$"files"
forcis_files <- forcis_files[which(forcis_files$"key" == 
                                     "FORCIS_taxonomy_levels.xlsx"), ]

forcis_url  <- forcis_files$ "links"$"self"
forcis_file <- forcis_files$ "key"


## download_file() ----

test_that("Test download_file() for success", {
  
  create_tempdir()

  expect_invisible(download_file(url       = forcis_url,
                                 path      = ".", 
                                 file      = forcis_file, 
                                 overwrite = FALSE, 
                                 timeout   = 300))
  
  expect_true(file.exists(forcis_file))
  
  expect_message(download_file(url       = forcis_url,
                               path      = ".", 
                               file      = forcis_file, 
                               overwrite = FALSE, 
                               timeout   = 300), 
                 paste0("The file '", forcis_file, "' already exists. If you ",
                        "want to download again this file please use the ",
                        "argument 'overwrite'."),
                 fixed = TRUE)
})
