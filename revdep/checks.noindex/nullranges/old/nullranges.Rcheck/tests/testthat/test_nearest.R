test_that("nearest matching handles categorical data", {
  
  ## Should not give error
  set.seed(123)
  x <- makeExampleMatchedDataSet()
  mr <- matchRanges(focal = x[x$feature1,],
                    pool = x[!x$feature1,],
                    covar = ~feature3,
                    method = "n",
                    replace = TRUE)
  expect_equal(nrow(mr), 500)
  
})