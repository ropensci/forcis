## Data for tests ----

df <- data.frame(matrix(1:21, nrow = 1))
colnames(df) <- get_required_columns()


## map_distribution() ----

test_that("Test map_distribution() for success", {
  
  expect_silent({ gg <- map_distribution(df) })
  
  expect_true("gg" %in% class(gg))
  expect_true("ggplot" %in% class(gg))
})
