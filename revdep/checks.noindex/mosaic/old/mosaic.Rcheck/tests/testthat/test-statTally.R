# context('statTally()')

expect_output <- function(x, expectation) {
  expect_true(any(grepl(expectation, x)))
}

test_that("Two-sided test", {
  set.seed(1)
  rdata <- data.frame("null" = rnorm(1000))
  expect_output(capture.output(tmp <- statTally(1, rdata, alternative="two.sided")),
                 "168 \\( 16.78 % \\) had test stats <= -1")
  expect_output(capture.output(tmp <- statTally(1, rdata, alternative="two.sided")),
                 "159 \\( 15.88 % \\) had test stats >= 1")
})

test_that("One-sided tests", {
  set.seed(1)
  rdata <- data.frame("null" = rnorm(1000))
  expect_output(capture.output(tmp <- statTally(1, rdata, alternative="greater")),
                 "*159 \\( 15.88 % \\) had test stats >= 1*")
  expect_output(capture.output(tmp <- statTally(1, rdata, alternative="less")),
                 "*843 \\( 84.22 % \\) had test stats <= 1*")
  expect_output(capture.output(tmp <- statTally(-1, rdata, alternative="greater")),
                 "*833 \\( 83.22 % \\) had test stats >= -1*")
  expect_output(capture.output(tmp <- statTally(-1, rdata, alternative="less")),
                 "*169 \\( 16.88 % \\) had test stats <= -1*")

})

require(mosaicData)
x <- c(10, 18, 9, 15)   # counts in four cells
set.seed(16)
rdata <- rmultinom(999, sum(x), prob = rep(.25, 4))
set.seed(17)
D <- diffmean( age ~ sex, data = HELPrct)
set.seed(19)
nullDist <- do(999) * diffmean( age ~ shuffle(sex), data = HELPrct)

testthat::test_that("statTally works", {
  
  wrapped_expect_doppelganger("statTally1", statTally(x, rdata, fun = max, binwidth = 1))
  wrapped_expect_doppelganger("statTally2", statTally(x, rdata, fun = var, shade = "red", binwidth = 2))
  wrapped_expect_doppelganger("statTally3", statTally(x, rdata, fun = max, binwidth = 1))
  #wrapped_expect_doppelganger("statTally4", statTally(D, nullDist))
  #wrapped_expect_doppelganger("statTally5", statTally(D, nullDist, system = "lattice"))
})
