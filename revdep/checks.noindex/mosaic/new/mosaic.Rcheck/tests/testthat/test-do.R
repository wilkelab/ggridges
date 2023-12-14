
# context('do()')

test_that("naming as expected", {
  expect_equal(ignore_attr = TRUE,  names( do(2) * mean(~cesd, data=mosaicData::HELPrct)), "mean")
  expect_equal(ignore_attr = TRUE,  names( do(2) * var(~cesd, data=mosaicData::HELPrct)), "var")
  expect_equal(ignore_attr = TRUE,  names( do(2) * {var(~cesd, data=mosaicData::HELPrct)}), "result")
  expect_equal(ignore_attr = TRUE,  names( do(2) * 5 + 3), "result")
  expect_equal(ignore_attr = TRUE,  names( do(2) * lm(cesd ~ age + sex, data=mosaicData::HELPrct)), 
     c("Intercept", "age", "sexmale", "sigma", "r.squared", "F", "numdf", "dendf", ".row", ".index"))
})


test_that("dimension as expected", {
  expect_equal(ignore_attr = TRUE,  dim( do(2) * mean(~cesd, data=mosaicData::HELPrct)), c(2,1))
  expect_equal(ignore_attr = TRUE,  dim( do(2) * var(~cesd, data=mosaicData::HELPrct)), c(2,1))
  expect_equal(ignore_attr = TRUE,  dim( do(2) * {var(~cesd, data=mosaicData::HELPrct)}), c(2,1))
  expect_equal(ignore_attr = TRUE,  dim( do(2) * 5 + 3), c(2,1))
  expect_equal(ignore_attr = TRUE,  dim( do(2) * lm(cesd ~ age + sex, data=mosaicData::HELPrct)), 
                      c(2,10))
  expect_equal(ignore_attr = TRUE,  dim( do(2) * anova(lm(cesd ~ age + sex, data=mosaicData::HELPrct))), 
                      c(6,8))
})
