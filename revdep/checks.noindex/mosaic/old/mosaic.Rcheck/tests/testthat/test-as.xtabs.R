# context("as.xtabs()")

df <- data.frame( X=c('Tea','Milk'), Tea=c(3,1), Milk=c(1,3) )
xt <- as.xtabs(df, rowvar="Guess", colvar="Truth")
xt

testthat::test_that("as.xtabs works", {
  example <- structure(c(3, 1, 1, 3), .Dim = c(2L, 2L), .Dimnames = list(Guess = c("Tea", 
                                                                                   "Milk"), Truth = c("Tea", "Milk")), class = c("xtabs", "table"
                                                                                   ))
  df <- data.frame( X=c('Tea','Milk'), Tea=c(3,1), Milk=c(1,3) )
  xt <- as.xtabs(df, rowvar="Guess", colvar="Truth")
  xt
  expect_equal(example, xt)
})
