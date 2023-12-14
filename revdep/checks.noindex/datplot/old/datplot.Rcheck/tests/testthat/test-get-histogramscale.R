test_that("works for single number", {
  expect_equal(get.histogramscale(2, binwidth = 2), 4)
})

test_that("fails for single number with binwidth = stepsize", {
  expect_error(get.histogramscale(2, binwidth = "stepsize"),
               "datsteps")
})


test_that("fails for vector with binwidth = stepsize", {
  expect_error(get.histogramscale(c(1, 2, 3, 4), binwidth = "stepsize"),
               "datsteps")
})


data("DAT_df")
test <- datsteps(DAT_df[4:10,], stepsize = 3,
                 calc = "weight", verbose = FALSE)

test_that("fails for wrong value of binwidth", {
  expect_error(get.histogramscale(test, binwidth = "fail"), "numeric")
})

test_that("returns a number when attribute is used", {
  expect_true(is.numeric(get.histogramscale(test, binwidth = "stepsize")))
})

test_that("returns a number when numeric binwidth is used", {
  expect_true(is.numeric(get.histogramscale(test, binwidth = 1)))
})

test_that("returns a number when a vector is supplied", {
  expect_equal(get.histogramscale(test$DAT_step, binwidth = 2),
               get.histogramscale(test, binwidth = 2))
})
