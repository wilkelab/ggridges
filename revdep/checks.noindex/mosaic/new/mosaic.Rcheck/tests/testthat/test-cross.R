# context("cross")

testthat::test_that("Cross works", {
  x <- letters[1:3]
  y <- c(1,2,1,1,3,1,3)
  
  testcase1 <- structure(c(1L, 5L, 7L, 1L, 6L, 7L, 3L), 
                         .Label = c("a:1", "a:2","a:3", "b:1", 
                                    "b:2", "b:3", "c:1", "c:2", 
                                    "c:3"), 
                         class = "factor")
  
  testcase2 <- structure(c(1L, 3L, 5L, 1L, 4L, 5L, 2L), 
                         .Label = c("a:1", "a:3", "b:2", "b:3", "c:1"), 
                         class = "factor")
  
  expect_equal(ignore_attr = TRUE, testcase1, cross(x, y))
  expect_equal(ignore_attr = TRUE, testcase2, cross(x, y, drop.unused.levels=TRUE))
})
