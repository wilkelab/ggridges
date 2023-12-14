result <- readOxcalOutput("ox_output.js")
this_date_list <- parseOxcalOutput(result)
this_date_list[[1]]$name <- "date"

this_multiple_dates_list <- this_date_list
this_multiple_dates_list[[2]] <- this_date_list[[1]]
names(this_multiple_dates_list) <- c("date1", "date2")
this_multiple_dates_list[[1]]$name <- "date1"
this_multiple_dates_list[[2]]$name <- "date2"

context("print.oxcAARCalibratedDatesList")

test_that("print is delegated to print.oxcAARCalibratedDate on a oxcAARCalibratedDatesList with just one entry", {
  expect_output(print(this_date_list), "BP")
  expect_failure(expect_output(print(this_date_list), "produced output"))
})

test_that("print is delegated to print.oxcAARCalibratedDatesList on a oxcAARCalibratedDatesList with multiple entries", {
  expect_output(print(this_multiple_dates_list), "List of 2")
})


# Dummy test for plots

context("plot.oxcAARCalibratedDatesList")

test_that("plot produces no error", {
  expect_error(plot( this_multiple_dates_list), NA)
})

context("is.oxcAARCalibratedDatesList")

test_that("is.oxcAARCalibratedDatesList distinguishes between objects", {
  expect_true(is.oxcAARCalibratedDatesList(this_multiple_dates_list))
  expect_false(is.oxcAARCalibratedDatesList(data.frame(test=NA)))
})
