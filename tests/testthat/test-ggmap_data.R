## Data for tests ----

df <- data.frame(matrix(1:21, nrow = 1))
colnames(df) <- get_required_columns()


## ggmap_data() ----

test_that("Test ggmap_data() for success", {
  expect_silent({
    gg <- ggmap_data(df)
  })

  expect_true("gg" %in% class(gg))
  expect_true("ggplot" %in% class(gg))

  vdiffr::expect_doppelganger("Map data", gg)
})
