# context("smoothers")
require(mosaicData)
testthat::test_that("smoothers works", {
  
  data(CPS85)
  f <- smoother(wage ~ age, span=.9, data=CPS85)
  g <- linearModel(log(wage) ~ age + educ + 1, data=CPS85)
  # an alternative way to define g (Note: + 1 is the default for lm().)
  g2 <- makeFun(lm(log(wage) ~ age + educ, data=CPS85))
  
  x<-1:5; y=c(1, 2, 4, 8, 8.2)
  f1 <- spliner(y ~ x)
  f1(x=8:10)
  f2 <- connector(x~y)
  
  testcase1 <- function (age) 
  {
    D <- eval(parse(text = makeDF))
    predict(L, newdata = D)
  }
  testcase2 <- c(`1` = 10.2530736275772)
  
  testcase3 <- structure(function (age, educ, showcoefs = FALSE) 
  {
    if (showcoefs) 
      coef(L)
    else {
      D <- eval(parse(text = makeDF))
      predict(L, newdata = D)
    }
  }, mosaicType = "Fitted Linear Model")
  
  testcase4 <- c(`1` = 2.01015841331998)
  
  testcase5 <- function (x, deriv = 0) 
  {
    x <- get(fnames[2])
    if (connect) 
      SF(x)
    else SF(x, deriv = deriv)
  }
  
  testcase6 <- c(-74.2666666666667, -148.777777777778, -256.466666666667)
  
  
  testcase7 <- function (y) 
  {
    x <- get(fnames[2])
    if (connect) 
      SF(x)
    else SF(x, deriv = deriv)
  }
  
  expect_equal(ignore_attr = TRUE, testcase2, f(40))
  expect_equal(ignore_attr = TRUE, testcase4, g(age=40, educ=12))
  
  expect_equal(ignore_attr = TRUE, testcase6, f1(x=8:10))
  
  
})
