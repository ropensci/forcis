## Data for tests ----

df_cpr  <- data.frame("data_type" = rep("CPR North", 5),
                      "species"   = c("n_pachyderma", "n_pachyderma", 
                                      "conglobatus", "g_rubescens", 
                                      "g_rubescens"))

df_net  <- data.frame("data_type"     = rep("Net", 5),
                      "t_parkerae_VT" = c(1:5),
                      "conglobatus"   = c(1:5),
                      "g_rubescens"   = c(1:5))

df <- data.frame("data_type" = rep("Net", 5),
                 "taxa"      = c("n_pachyderma", "n_pachyderma", 
                                 "conglobatus", "g_rubescens", 
                                 "g_rubescens"),
                 "counts"    = c(1:4, NA))


## filter_by_species() ----

test_that("Test filter_by_species() for error", {
  
  expect_error(filter_by_species(df_cpr),
               "This function is not designed to work with 'CPR North' data",
               fixed = TRUE)
  
  expect_error(filter_by_species(df),
               paste0("This function is not designed to work on long format", 
                      " data.frame"),
               fixed = TRUE)
  
  expect_error(filter_by_species(df[ , 1, drop = FALSE]),
               "No species columns detected",
               fixed = TRUE)
  
  expect_error(filter_by_species(df_net),
               "Argument 'species' is required",
               fixed = TRUE)
  
  expect_error(filter_by_species(df_net, species = 1),
               "Argument 'species' must be a character of length >= 1",
               fixed = TRUE)
  
  expect_error(filter_by_species(df_net, species = "toto"),
               "The species provided are absent from 'data'",
               fixed = TRUE)
  
  expect_error(filter_by_species(df_net, species = c("toto", "titi")),
               "The species provided are absent from 'data'",
               fixed = TRUE)
})

test_that("Test filter_by_species() for success", {
  
  expect_silent(res <- filter_by_species(df_net, species = c("conglobatus")))
  
  expect_true(is.data.frame(res))
  expect_equal(ncol(res), 2L)
  expect_equal(nrow(res), 5L)
  
  expect_silent(res <- filter_by_species(df_net, species = c("conglobatus", 
                                                             "g_rubescens")))
  
  expect_true(is.data.frame(res))
  expect_equal(ncol(res), 3L)
  expect_equal(nrow(res), 5L)
  
  expect_silent(res <- filter_by_species(df_net, species = c("conglobatus", 
                                                             "g_rubescens",
                                                             "titi")))
  
  expect_true(is.data.frame(res))
  expect_equal(ncol(res), 3L)
  expect_equal(nrow(res), 5L)
})
