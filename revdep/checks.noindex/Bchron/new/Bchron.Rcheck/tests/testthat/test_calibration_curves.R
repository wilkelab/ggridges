set.seed(123)
library(Bchron)
co <- function(expr) capture.output(expr, file = "NUL")


test_that("intcal20 data correct", {
  data(intcal20)
  expect_true(nrow(intcal20) == 9501)
  expect_true(ncol(intcal20) == 5)
})

test_that("intcal13 data correct", {
  data(intcal13)
  expect_true(nrow(intcal13) == 5141)
  expect_true(ncol(intcal13) == 5)
})

test_that("marine13 data correct", {
  data(marine13)
  expect_true(nrow(marine13) == 4801)
  expect_true(ncol(marine13) == 5)
})

test_that("marine20 data correct", {
  data(marine20)
  expect_true(nrow(marine20) == 5501)
  expect_true(ncol(marine20) == 5)
})

test_that("shcal13 data correct", {
  data(shcal13)
  expect_true(nrow(shcal13) == 5141)
  expect_true(ncol(shcal13) == 5)
})

test_that("shcal20 data correct", {
  data(shcal20)
  expect_true(nrow(shcal20) == 9501)
  expect_true(ncol(shcal20) == 5)
})
