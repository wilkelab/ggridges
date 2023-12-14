# context("is.wholenumber")

testthat::test_that("is.wholenumber works", {
  expect_true(is.wholenumber(1))
  expect_true(all(is.wholenumber(rbinom(100,10,.5))))
  
  testcase <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, 
                TRUE)
  expect_equal(testcase, is.wholenumber((1:10)/2))
})
