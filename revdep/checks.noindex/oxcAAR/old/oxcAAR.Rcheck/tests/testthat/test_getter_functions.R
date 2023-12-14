context("getter functions")

# prepare test data
result <- readOxcalOutput("ox_output.js")
this_date <- parseOxcalOutput(result)[[1]]
this_date$name <- "my_date"
this_date_list <- parseOxcalOutput(result)
this_date_list[[1]]$name <- "date"
this_multiple_dates_list <- this_date_list
this_multiple_dates_list[[2]] <- this_date_list[[1]]
names(this_multiple_dates_list) <- c("date1", "date2")

test_that("get_name returns name", {
  expect_equal(get_name(this_date), this_date[['name']])
  expect_equal(
    get_name(this_multiple_dates_list),
    unname(sapply(this_multiple_dates_list, function(x) {x[["name"]]}))
  )
})

test_that("get_bp returns BP", {
  expect_equal(get_bp(this_date), this_date[['bp']])
  expect_equal(
    get_bp(this_multiple_dates_list),
    unname(sapply(this_multiple_dates_list, function(x) {x[["bp"]]}))
  )
})

test_that("get_std returns std", {
  expect_equal(get_std(this_date), this_date[['std']])
  expect_equal(
    get_std(this_multiple_dates_list),
    unname(sapply(this_multiple_dates_list, function(x) {x[["std"]]}))
  )
})

test_that("get_cal_curve returns std", {
  expect_equal(get_cal_curve(this_date), this_date[['cal_curve']])
  expect_equal(
    get_cal_curve(this_multiple_dates_list),
    unname(lapply(this_multiple_dates_list, function(x) {x[["cal_curve"]]}))
  )
})

test_that("get_sigma_ranges returns sigma_ranges", {
  expect_equal(get_sigma_ranges(this_date), this_date[['sigma_ranges']])
  expect_equal(
    get_sigma_ranges(this_multiple_dates_list),
    unname(lapply(this_multiple_dates_list, function(x) {x[["sigma_ranges"]]}))
  )
})

test_that("get_raw_probabilities returns raw_probabilities", {
  expect_equal(get_raw_probabilities(this_date), this_date[['raw_probabilities']])
  expect_equal(
    get_raw_probabilities(this_multiple_dates_list),
    unname(lapply(this_multiple_dates_list, function(x) {x[["raw_probabilities"]]}))
  )
})
