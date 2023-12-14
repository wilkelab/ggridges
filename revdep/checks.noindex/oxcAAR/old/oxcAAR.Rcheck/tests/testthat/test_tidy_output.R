context("tidy output")

# prepare test data
result <- readOxcalOutput("ox_output.js")
this_date <- parseOxcalOutput(result)[[1]]
this_date$name <- "my_date"
this_date_list <- parseOxcalOutput(result)
this_date_list[[1]]$name <- "date"
this_multiple_dates_list <- this_date_list
this_multiple_dates_list[[2]] <- this_date_list[[1]]
names(this_multiple_dates_list) <- c("date1", "date2")

# call tidy output functions
tidy_res <- get_tidy_oxcalresult(this_date)
tidy_res_list <- get_tidy_oxcalresult(this_multiple_dates_list)

test_that("get_tidy_oxcalresult produces a data.frame", {
  expect_true(is.data.frame(tidy_res))
  expect_true(is.data.frame(tidy_res_list))
})

test_that("get_tidy_oxcalresult produces a data.frame with the correct amount of rows", {
  expect_equal(nrow(tidy_res), 1)
  expect_equal(nrow(tidy_res_list), 2)
})

test_that("get_tidy_oxcalresult produces a data.frame with the correct cols (names", {
  expect_true(all(c("name", "bp", "std", "cal_curve", "sigma_ranges", "raw_probabilities", "posterior_sigma_ranges", "posterior_probabilities") %in% colnames(tidy_res)))
  expect_true(all(c("name", "bp", "std", "cal_curve", "sigma_ranges", "raw_probabilities", "posterior_sigma_ranges", "posterior_probabilities") %in% colnames(tidy_res_list)))
})

test_that("get_tidy_oxcalresult produces a data.frame with the correct cols (classes)", {
  expect_true(all(c("character", "integer", "integer", "character", "AsIs", "AsIs", "AsIs", "AsIs") %in% sapply(tidy_res, class)))
  expect_true(all(c("character", "integer", "integer", "character", "AsIs", "AsIs", "AsIs", "AsIs") %in% sapply(tidy_res_list, class)))
})
