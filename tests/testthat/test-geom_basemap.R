## geom_basemap() ----

test_that("Test geom_basemap() for success", {

  expect_silent({ geom <- geom_basemap() })
  
  expect_true("list" %in% class(geom))
  expect_equal(length(geom), 5L)
  
  for (i in 1:4) {
    expect_true("Layer" %in% class(geom[[i]][[1]]))
    expect_true("LayerInstance" %in% class(geom[[i]][[1]]))
    expect_true("ggproto" %in% class(geom[[i]][[1]]))
    expect_true("gg" %in% class(geom[[i]][[1]]))
    expect_true("LayerSf" %in% class(geom[[i]][[1]]))
  }
  
  expect_true("element_blank" %in% class(geom[[5]][[1]]))
})
