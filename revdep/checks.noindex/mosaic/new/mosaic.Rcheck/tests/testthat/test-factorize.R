# context("factorize")

testthat::test_that("factorize works", {
  data(KidsFeet, package="mosaicData")
  testcase <- structure(c(2L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 
                          2L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 
                          2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), 
                         .Label = c("87", "88"), 
                         class = "factor")
  
  expect_equal(ignore_attr = TRUE, testcase, factorize(KidsFeet$birthyear))
  # alternative spelling
  expect_equal(ignore_attr = TRUE, testcase, factorise(KidsFeet$birthyear))
})
