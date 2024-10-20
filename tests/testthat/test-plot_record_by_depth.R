## Data for tests ----

df_net <- data.frame("data_type"        = rep("Net", 6),
                     "sample_min_depth" = c(100, 100, 200, 300, 300, 300),
                     "sample_max_depth" = c(300, 300, 400, 500, 500, 500),
                     "sample_id"        = c(1, 1, 2, 3, 3, 3))

df_trap <- data.frame("data_type"              = rep("Sediment trap", 6),
                      "sample_date_time_start" = c("01/02/1999", "01/03/2020",
                                                   "01/03/2022", "01/04/2023",
                                                   "01/07/2024", NA),
                      "sample_id"              = c(1, 1, 2, 3, 3, 3))


## plot_record_by_month() ----

test_that("Test plot_record_by_depth() for error", {
  
  expect_error(plot_record_by_depth(df_trap),
               "This function is designed to work only with Net data",
               fixed = TRUE)
})

test_that("Test plot_record_by_depth() for success", {
  
  expect_silent({ gg <- plot_record_by_depth(df_net) })
  
  expect_true("gg" %in% class(gg))
  expect_true("ggplot" %in% class(gg))
  
  vdiffr::expect_doppelganger("Plot record by depth", gg)
})
