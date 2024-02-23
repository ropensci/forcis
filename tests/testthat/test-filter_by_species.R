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

df2 <- data.frame("data_type" = rep("Net", 5),
                 "taxa"      = c("n_pachyderma", "n_pachyderma", 
                                 "conglobatus", "g_rubescens", 
                                 "g_rubescens"),
                 "counts"    = c(1:3, NA, NA))


## filter_by_species() ----

test_that("Test filter_by_species() for error", {
  
  expect_error(filter_by_species(df_cpr),
               "This function is not designed to work with 'CPR North' data",
               fixed = TRUE)
  
  expect_error(filter_by_species(df_net),
               paste0("This function requires data in long format. Please use ", 
                      "the function 'reshape_data()'"),
               fixed = TRUE)
  
  expect_error(filter_by_species(df),
               "Argument 'species' is required",
               fixed = TRUE)
  
  expect_error(filter_by_species(df, species = 1),
               "Argument 'species' must be a character of length >= 1",
               fixed = TRUE)
  
  expect_error(filter_by_species(df, species = "toto"),
               "The species provided are absent from 'data'",
               fixed = TRUE)
  
  expect_error(filter_by_species(df, species = c("toto", "titi")),
               "The species provided are absent from 'data'",
               fixed = TRUE)
})

test_that("Test filter_by_species() for success", {
  
  expect_silent(res <- filter_by_species(df, species = c("conglobatus")))
  
  expect_true(is.data.frame(res))
  expect_equal(ncol(res), ncol(df))
  expect_equal(nrow(res), 1L)
  
  expect_silent(res <- filter_by_species(df, species = c("conglobatus", 
                                                         "g_rubescens")))
  
  expect_true(is.data.frame(res))
  expect_equal(ncol(res), ncol(df))
  expect_equal(nrow(res), 3L)
  
  expect_silent(res <- filter_by_species(df, species = c("conglobatus", 
                                                         "g_rubescens"),
                                         rm_na = TRUE))
  
  expect_true(is.data.frame(res))
  expect_equal(ncol(res), ncol(df))
  expect_equal(nrow(res), 2L)
  
  expect_silent(res <- filter_by_species(df2, species = "g_rubescens",
                                         rm_na = TRUE))
  
  expect_true(is.data.frame(res))
  expect_equal(ncol(res), ncol(df))
  expect_equal(nrow(res), 0L)
  
  expect_silent(res <- filter_by_species(df, species = c("conglobatus", 
                                                         "g_rubescens",
                                                         "titi")))
  
  expect_true(is.data.frame(res))
  expect_equal(ncol(res), ncol(df))
  expect_equal(nrow(res), 3L)
})
