
# context('Aggregating Functions')

test_that("formula interface works", {
  expect_equal(ignore_attr = TRUE,  mean(~cesd, data=mosaicData::HELPrct), mean(mosaicData::HELPrct$cesd))
  expect_equal(ignore_attr = TRUE,  var(~cesd, data=mosaicData::HELPrct), var(mosaicData::HELPrct$cesd))
  expect_equal(ignore_attr = TRUE,  sd(~cesd, data=mosaicData::HELPrct), sd(mosaicData::HELPrct$cesd))
  expect_equal(ignore_attr = TRUE,  max(~cesd, data=mosaicData::HELPrct), max(mosaicData::HELPrct$cesd))
  expect_equal(ignore_attr = TRUE,  min(~cesd, data=mosaicData::HELPrct), min(mosaicData::HELPrct$cesd))
  expect_equal(ignore_attr = TRUE,  min(~cesd | sex, data=mosaicData::HELPrct), min(cesd ~ sex, data=mosaicData::HELPrct))
  expect_equal(ignore_attr = TRUE,  max(~cesd | sex, data=mosaicData::HELPrct), max(cesd ~ sex, data=mosaicData::HELPrct))
  expect_equal(ignore_attr = TRUE,  mean(~cesd | sex, data=mosaicData::HELPrct), mean(cesd ~ sex, data=mosaicData::HELPrct))
  expect_equal(ignore_attr = TRUE,  sd(~cesd | sex, data=mosaicData::HELPrct), sd(cesd ~ sex, data=mosaicData::HELPrct))
  expect_equal(ignore_attr = TRUE,  var(~cesd | sex, data=mosaicData::HELPrct), var(cesd ~ sex, data=mosaicData::HELPrct))
})

# no longer supporting this
# test_that("data frame interface works", {
#   expect_equal(ignore_attr = TRUE,  mean(cesd, data=mosaicData::HELPrct), mean(mosaicData::HELPrct$cesd))
#   expect_equal(ignore_attr = TRUE,  var(cesd, data=mosaicData::HELPrct), var(mosaicData::HELPrct$cesd))
#   expect_equal(ignore_attr = TRUE,  sd(cesd, data=mosaicData::HELPrct), sd(mosaicData::HELPrct$cesd))
#   expect_equal(ignore_attr = TRUE,  max(cesd, data=mosaicData::HELPrct), max(mosaicData::HELPrct$cesd))
#   expect_equal(ignore_attr = TRUE,  min(cesd, data=mosaicData::HELPrct), min(mosaicData::HELPrct$cesd))
# })


test_that("formulas work without data", {
  age <<- mosaicData::HELPrct$age
  sex <<- mosaicData::HELPrct$sex
  expect_equal(ignore_attr = TRUE,  min( age ), min( ~age ))
  expect_equal(ignore_attr = TRUE,  min( ~ age ), min( ~age, data=mosaicData::HELPrct ))
  expect_equal(ignore_attr = TRUE,  max( age ), max( ~age ))
  expect_equal(ignore_attr = TRUE,  max( ~ age ), max( ~age, data=mosaicData::HELPrct ))
  expect_equal(ignore_attr = TRUE,  sd( age ), sd( ~age ))
  expect_equal(ignore_attr = TRUE,  sd( ~ age ), sd( ~age, data=mosaicData::HELPrct ))
  expect_equal(ignore_attr = TRUE,  var( age ), var( ~age ))
  expect_equal(ignore_attr = TRUE,  var( ~ age ), var( ~age, data=mosaicData::HELPrct ))
  expect_equal(ignore_attr = TRUE,  median( ~ age ), median( ~age, data=mosaicData::HELPrct ))
  expect_equal(ignore_attr = TRUE,  median( age ~ sex ), median( age ~ sex, data=mosaicData::HELPrct ))
  expect_equal(ignore_attr = TRUE,  mean( age ), mean( ~age ))
  expect_equal(ignore_attr = TRUE,  mean( ~ age ), mean( ~age, data=mosaicData::HELPrct ))
})

test_that(
  "var grabs two vectors from data frame", {
    expect_equal(ignore_attr = TRUE, var( ~ age, ~ cesd, data = mosaicData::HELPrct), 
                      var(mosaicData::HELPrct$age, mosaicData::HELPrct$cesd))
  })

test_that("na.rm works", {
  x <<- 1:6; y <<- c(1,2,5,6)
  x[3] <- NA
  x[4] <- NA
  expect_equal(ignore_attr = TRUE,    mean(x, na.rm=TRUE), mean(y))
  expect_equal(ignore_attr = TRUE,      sd(x, na.rm=TRUE), sd(y))
  expect_equal(ignore_attr = TRUE,     var(x, na.rm=TRUE), var(y))
  expect_equal(ignore_attr = TRUE,  median(x, na.rm=TRUE), median(y))
  expect_equal(ignore_attr = TRUE,     max(x, na.rm=TRUE), max(y))
  expect_equal(ignore_attr = TRUE,     var(x, x, na.rm=TRUE), var(x, x, na.rm=TRUE))
  expect_equal(ignore_attr = TRUE,     var(x, 1:6, na.rm=TRUE), stats::var(x, 1:6, na.rm=TRUE))
})

test_that("sum( a + x ) works, etc.", {
  x <- 1:6; a <- 11:16
  expect_equal(ignore_attr = TRUE,  sum( a + x) , base::sum( a + x) )
  expect_equal(ignore_attr = TRUE,  mean( a + x) , base::mean( a + x) )
  expect_equal(ignore_attr = TRUE,  median( a + x) , stats::median( a + x) )
})

test_that("cov() and cor() work.", {
  expect_equal(ignore_attr = TRUE, cor( age ~ i1, data = HELPrct), cor(HELPrct$age, HELPrct$i1))
  expect_equal(ignore_attr = TRUE, cor( ~ age, ~ i1, data = HELPrct), cor(HELPrct$age, HELPrct$i1))
  expect_equal(ignore_attr = TRUE, cor( ~ age | i1, data = HELPrct), cor(HELPrct$age, HELPrct$i1))
  expect_equal(ignore_attr = TRUE, cov( age ~ i1, data = HELPrct), cov(HELPrct$age, HELPrct$i1))
  expect_equal(ignore_attr = TRUE, cov( ~ age, ~ i1, data = HELPrct), cov(HELPrct$age, HELPrct$i1))
  expect_equal(ignore_attr = TRUE, cov( ~ age | i1, data = HELPrct), cov(HELPrct$age, HELPrct$i1))
})

test_that("var() works.", {
  expect_equal(ignore_attr = TRUE, var( age ~ sex, data = HELPrct), 
                    aggregate(HELPrct$age, by = list(HELPrct$sex), FUN = var)$x)
  expect_equal(ignore_attr = TRUE, var( ~ age, ~ i1, data = HELPrct), var(HELPrct$age, HELPrct$i1))
  expect_equal(ignore_attr = TRUE, var( ~ age | sex, data = HELPrct), var(age ~ sex, data = HELPrct))
})

# no longer supporting this
# test_that("bare names work", {
#   expect_equal(ignore_attr = TRUE,  mean(   age, data = HELPrct), 
#                      mean( ~ age, data = HELPrct) )
#   expect_equal(ignore_attr = TRUE,  cor( age,  cesd, data = HELPrct), 
#                      cor( age ~ cesd, data = HELPrct) )
#   expect_equal(ignore_attr = TRUE,  var(  age, data = HELPrct), 
#                      var( ~ age, data = HELPrct) )
#   expect_equal(ignore_attr = TRUE,  favstats(   age, data = HELPrct), 
#                      favstats( ~ age, data = HELPrct) )
# })

test_that("aggregatingFunctions work", {
  require(mosaicData)
  foo <- aggregatingFunction1(base::mean)
  expect_equal(ignore_attr = TRUE, 24.7230769230769, foo( ~ length, data = KidsFeet))
  
 
  foo <- aggregatingFunction2(stats::cor)
  expect_equal(ignore_attr = TRUE, 0.641096051081777, foo(length ~ width, data = KidsFeet))
})


test_that("aggregating functions are compatible with dplyr", {
  require(dplyr)
  expect_equal(ignore_attr = TRUE, 
    data.frame(x = 1:10, y = 1:10) |> 
      summarise(mean = mean(~x), cor = cor(y ~ x)),
    data.frame(x = 1:10, y = 1:10) |> 
      summarise(mean = base::mean(x), cor = stats::cor(y, x))
  )
})
