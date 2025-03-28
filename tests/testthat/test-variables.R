## date_format() ----

test_that("Test date_format() for success", {
  x <- date_format()

  expect_equal(x, "%d/%m/%Y")
  expect_equal(class(x), "character")
  expect_equal(length(x), 1L)

  x <- as.Date("01/01/1900", date_format())

  expect_equal(x, as.Date("1900-01-01"))
  expect_equal(class(x), "Date")
  expect_equal(length(x), 1L)
})


## cpr_north_filename() ----

test_that("Test cpr_north_filename() for success", {
  x <- cpr_north_filename()

  expect_equal(x, "FORCIS_cpr_north_")
  expect_equal(class(x), "character")
  expect_equal(length(x), 1L)
})


## cpr_south_filename() ----

test_that("Test cpr_south_filename() for success", {
  x <- cpr_south_filename()

  expect_equal(x, "FORCIS_cpr_south_")
  expect_equal(class(x), "character")
  expect_equal(length(x), 1L)
})


## plankton_net_filename() ----

test_that("Test plankton_net_filename() for success", {
  x <- plankton_net_filename()

  expect_equal(x, "FORCIS_net_")
  expect_equal(class(x), "character")
  expect_equal(length(x), 1L)
})


## pump_filename() ----

test_that("Test pump_filename() for success", {
  x <- pump_filename()

  expect_equal(x, "FORCIS_pump_")
  expect_equal(class(x), "character")
  expect_equal(length(x), 1L)
})


## sediment_trap_filename() ----

test_that("Test sediment_trap_filename() for success", {
  x <- sediment_trap_filename()

  expect_equal(x, "FORCIS_trap_")
  expect_equal(class(x), "character")
  expect_equal(length(x), 1L)
})


## get_required_columns() ----

test_that("Test get_required_columns() for success", {
  x <- get_required_columns()

  expect_equal(x[1], "data_type")
  expect_equal(class(x), "character")
  expect_equal(length(x), 21L)
})


## data_types() ----

test_that("Test data_types() for success", {
  x <- data_types()

  expect_true(all(
    c("Net", "Pump", "Sediment trap", "CPR South", "CPR North") %in% x
  ))

  expect_equal(class(x), "character")
  expect_equal(length(x), 5L)
})


## crs_robinson() ----

test_that("Test crs_robinson() for success", {
  y <- paste0(
    "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 ",
    "+units=m +no_defs"
  )

  x <- crs_robinson()

  expect_equal(x, y)

  expect_equal(class(x), "character")
  expect_equal(length(x), 1L)
})


## species_list() ----

test_that("Test species_list() for success", {
  x <- species_list()

  expect_equal(class(x), "data.frame")
  expect_equal(ncol(x), 2L)
  expect_equal(nrow(x), 310L)

  expect_equal(colnames(x)[1], "taxon")
  expect_equal(colnames(x)[2], "taxonomy")

  expect_true(all(c("LT", "VT", "OT", "ZZ") %in% unique(x$"taxonomy")))
})
