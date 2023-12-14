# context("plotCumfreq")

testthat::test_that("plotCumfreq works", {
  wrapped_expect_doppelganger("plotCumfreq1", plotCumfreq(~eruptions, faithful, xlab = 'duration of eruptions'))
})
