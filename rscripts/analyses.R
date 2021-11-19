## Analyses ----

## Load R functions and packages ---

devtools::load_all()


## Start of analyses ----

## Define filename path ----
file_path <- system.file("extdata", "example_dataset.csv", package = "forcis")

## Read CSV file ----
dat <- readr::read_csv(file = file_path)


