
# context('Environment Checks (with(), etc.)')

test_that("with() works in place of data = ",{
  expect_equal(ignore_attr = TRUE,  mean(~length, data=mosaicData::KidsFeet), with(mosaicData::KidsFeet, mean(length)) )
  expect_equal(ignore_attr = TRUE,  sd(~length, data=mosaicData::KidsFeet), with(mosaicData::KidsFeet, sd(length)) )
  expect_equal(ignore_attr = TRUE,  diffmean(length ~ sex, data=mosaicData::KidsFeet), with(mosaicData::KidsFeet, diffmean(length ~ sex)) )
  expect_equal(ignore_attr = TRUE,  diffprop(domhand ~ sex, data=mosaicData::KidsFeet), with(mosaicData::KidsFeet, diffprop(domhand ~ sex)) )
  expect_equal(ignore_attr = TRUE,  t.test(length ~ sex, data=mosaicData::KidsFeet), with(mosaicData::KidsFeet, t.test(length ~ sex)) )
})
