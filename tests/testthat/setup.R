#' Setup Tests Infrastructure
#'

## Temporary Directory ----

create_tempdir <- function(path = file.path(tempdir(), "sandbox")) {
  old_wd <- getwd()

  withr::defer(fs::dir_delete(path), envir = parent.frame())

  dir.create(path)

  setwd(path)

  withr::defer(setwd(old_wd), envir = parent.frame())

  invisible(path)
}


## Setup httptest2

library(httptest2)

# Mock the get_metadata function
mock_metadata_env <- new.env()

with_mock_dir(
  test_path("mockdata", "all-records"),
  {
    mock_metadata_env$metadata_all_records <- get_metadata()
  },
  simplify = FALSE
)

# Mock the get_metadata function to return our controlled metadata
get_metadata_mock <- function(...) {
  return(mock_metadata_env$metadata_all_records)
}
