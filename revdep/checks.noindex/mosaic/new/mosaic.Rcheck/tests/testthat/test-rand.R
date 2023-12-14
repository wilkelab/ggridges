# context("rand()")

testthat::test_that("rand works", {
  
  testcase1 <- structure(c(-1.18945374525972, 0.388581229632553, -0.344333349210725, 
                          -0.547896138418295, 0.980662215750192, -0.236646007272852, 0.809739704347715, 
                          -0.744779528664041), .Dim = c(4L, 2L))
  
  testcase2 <- structure(c(1L, 3L, 4L, 1L, 2L, 2L, 2L, 3L), .Dim = c(4L, 2L))
  
  
  
  set.seed(19)
  expect_equal(ignore_attr = TRUE, testcase1, rand(2,nrow=4))
  set.seed(19)
  expect_equal(ignore_attr = TRUE, testcase2, rand(2,rdist=rpois, args=list(lambda=3), nrow=4))
  
  
})




