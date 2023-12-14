data("DAT_df")
DAT_df$ID <- as.character(DAT_df$ID)
DAT_df$var <- as.factor(DAT_df$var)

test_that("uses probability calculation when cumulative = TRUE", {
  expect_warning(datsteps(DAT_df[4:8, ],
                          stepsize = 1,
                          calc = "weight",
                          cumulative = TRUE,
                          verbose = FALSE),
                 "cumulative")
})

test_that("colnames are as expected", {
  test <- datsteps(DAT_df[4:8, ], stepsize = 1,
                   calc = "probability", cumulative = TRUE,
                   verbose = FALSE)
  expect_equal(colnames(test), c("ID", "variable", "DAT_min",
                                 "DAT_max", "probability",
                                 "DAT_step", "cumul_prob"))
})

test_that("warns for unreasonable stepsize when using probability", {
  expect_warning(datsteps(DAT_df[4:8, ],
                          stepsize = 20,
                          calc = "probability",
                          verbose = FALSE),
                 "meaningful")
})


test_that("calculation argument is guessed correctly from partial word", {
  expect_message(datsteps(DAT_df[4:5, ],
                          stepsize = 1,
                          calc = "pro",
                          verbose = TRUE),
                 "probability")
  expect_message(datsteps(DAT_df[4:5, ],
                          stepsize = 1,
                          calc = "we",
                          verbose = TRUE),
                 "weight")
})

test_that("error for non-expected calc-argument", {
  expect_error(datsteps(DAT_df[4:5, ], stepsize = 1,
                        calc = "börek"),
                 "probability")
})


test_that("error for non-expected calc-argument", {
  DAT_df[12, 3] <- NA
  expect_warning(datsteps(DAT_df[10:12, ], stepsize = 1, verbose = FALSE),
                 "NA-values")
  test <- suppressWarnings(datsteps(DAT_df[10:12, ], verbose = FALSE)$ID)
  expect_false("12" %in% test)
})



test_that("stepsize = auto can be used", {
  expect_message(datsteps(DAT_df[3:4, ], stepsize = "auto", verbose = TRUE), "auto")
  test <- datsteps(DAT_df[3:4, ], stepsize = "auto", verbose = FALSE)
  expect_equal(attributes(test)$stepsize, 1)
})


test_that("stepsize attribute is attached", {
  stepsize <- 2
  test <- suppressWarnings(datsteps(DAT_df[3:4, ],
                                    stepsize = stepsize,
                                    verbose = FALSE))
  expect_equal(attributes(test)$stepsize, stepsize)
})


test_that("stepsize attribute is attached", {
  stepsize <- "no"
  expect_error(datsteps(DAT_df[3:4, ], stepsize = stepsize, verbose = FALSE),
               "numeric")
})







