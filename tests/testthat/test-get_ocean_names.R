## get_ocean_names() ----

test_that("Test get_ocean_names() for success", {
  
  oceans <- get_ocean_names()
  
  expect_true(class(oceans) == "character")
  expect_true(length(oceans) == 8L)
  
  expect_equal(oceans[1], "Arctic Ocean")
  expect_equal(oceans[2], "Indian Ocean")
  expect_equal(oceans[3], "Mediterranean Sea")
  expect_equal(oceans[4], "North Atlantic Ocean")
  expect_equal(oceans[5], "North Pacific Ocean")
  expect_equal(oceans[6], "South Atlantic Ocean")
  expect_equal(oceans[7], "South Pacific Ocean")
  expect_equal(oceans[8], "Southern Ocean")
})
