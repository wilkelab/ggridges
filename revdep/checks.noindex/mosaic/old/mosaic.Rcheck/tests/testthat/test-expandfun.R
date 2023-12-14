# context("expandFun()")

testthat::test_that("expandFun works", {
  
  testcase <- list(formula = (z)^2 ~ z, formals = as.pairlist(alist(x = )))
  
  expect_equal(ignore_attr = TRUE, testcase, expandFun(makeFun(x^2~x)(z)~z))
})

