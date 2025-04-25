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

# Mock get_metadata to avoid 429 (Too Many Requests) errors
# by limiting external calls (e.g., allowing only one real call).
mock_metadata_env <- new.env()

mock_metadata_env$metadata_all_records <- get_metadata()

get_metadata_mock <- function(...) {
  return(mock_metadata_env$metadata_all_records)
}
