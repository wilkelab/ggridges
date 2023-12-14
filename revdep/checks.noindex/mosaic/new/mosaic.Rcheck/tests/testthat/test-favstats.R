
# context('favstats()')

test_that("favstats works for vectors ", {
  expect_equal(ignore_attr = TRUE,  favstats(1:10)$mean,  mean(1:10) )
  expect_equal(ignore_attr = TRUE,  favstats(1:10)$sd,  sd(1:10) )
  expect_equal(ignore_attr = TRUE,  favstats(1:10)$min,  min(1:10) )
  expect_equal(ignore_attr = TRUE,  favstats(1:10)$max,  max(1:10) )
  expect_equal(ignore_attr = TRUE,  favstats(1:10)$Q1,  quantile(1:10)[2] )
  expect_equal(ignore_attr = TRUE,  favstats(1:10)$Q3,  quantile(1:10)[4] )
})

test_that("data interface works", {
  expect_equal(ignore_attr = TRUE,  favstats(mosaicData::HELPrct$age), favstats(age, data=mosaicData::HELPrct) )
})

test_that("formula interface works", {
  expect_equal(ignore_attr = TRUE,  favstats(mosaicData::HELPrct$age), favstats(~age, data=mosaicData::HELPrct) )
})

test_that("formulas work without data", {
  expect_equal(ignore_attr = TRUE,  favstats(1:10), favstats(~1:10) )
})


test_that("missing data handled correctly", {
  myHELP <- mosaicData::HELPrct
  myHELP$age[1] <- NA
  expect_equal(ignore_attr = TRUE,  favstats(myHELP$age)$missing, 1 ) 
  expect_equal(ignore_attr = TRUE,  favstats(myHELP$age)$mean, mean(mosaicData::HELPrct$age[-1]) )
  expect_equal(ignore_attr = TRUE,  favstats(myHELP$age)$sd, sd(mosaicData::HELPrct$age[-1]) )
})


