## Data for tests ----

df_net <- data.frame("data_type"         = rep("Net", 6),
                     "profile_date_time" = c("01/02/1999", "01/03/2020",
                                             "01/03/2022", "01/04/2023",
                                             "01/07/2024", NA),
                     "sample_id"         = c(1, 1, 2, 3, 3, 3))

df_trap <- data.frame("data_type"              = rep("Sediment trap", 6),
                      "sample_date_time_start" = c("01/02/1999", "01/03/2020",
                                                   "01/03/2022", "01/04/2023",
                                                   "01/07/2024", NA),
                      "sample_id"              = c(1, 1, 2, 3, 3, 3))


## plot_record_by_month() ----

test_that("Test plot_record_by_month() for success", {
  
  expect_silent({ gg <- plot_record_by_month(df_net) })
  
  expect_true("gg" %in% class(gg))
  expect_true("ggplot" %in% class(gg))
  
  expect_silent({ gg <- plot_record_by_month(df_trap) })
  
  expect_true("gg" %in% class(gg))
  expect_true("ggplot" %in% class(gg))
})

