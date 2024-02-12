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
