# context("plotModel")

testthat::test_that("plotModel works", {

  mod <- lm( mpg ~ factor(cyl), data = mtcars)
  mod2 <- lm( mpg ~ wt, data = mtcars)
  mod3 <- lm( mpg ~ wt + factor(cyl), data=mtcars)
  # Default
  wrapped_expect_doppelganger("plotModel1", plotModel(mod))
  # SLR
  wrapped_expect_doppelganger("plotModel2", plotModel(mod2, pch = 19))
  # parallel slopes
  wrapped_expect_doppelganger("plotModel3", plotModel(mod3))
})
