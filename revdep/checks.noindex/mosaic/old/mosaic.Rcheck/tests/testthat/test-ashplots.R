# context("ashplot()")
ashplot( ~age | substance, groups = sex, data = HELPrct)

testthat::test_that("Ashplots work", {
  wrapped_expect_doppelganger("ashplots1", ashplot( ~age | substance, groups = sex, data = HELPrct))
})
