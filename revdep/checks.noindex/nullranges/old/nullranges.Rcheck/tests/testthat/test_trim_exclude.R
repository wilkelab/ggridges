library(nullranges)
test_that("trim excludeOption works as expected", {

  y <- GRanges("chr1", IRanges(1 + 2 * 0:45, width=10),
               strand=rep(c("+","-"),length=46),
               seqlengths=c(chr1=100))
  exclude <- GRanges("chr1", IRanges(36,45),
                     seqlengths=c(chr1=100))
  y <- sort(y)
  y$block <- 1
  br <- bootRanges(y, blockLength=20, exclude=exclude, excludeOption="trim")

  expect_true(!any(overlapsAny(br, exclude)))
  
})
