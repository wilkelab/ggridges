testthat::test_that("chisq works", {
  require(mosaicData)
  Mites.table <- tally( ~ outcome + treatment, data=Mites )
  ## Randomization test.  Increase replications to decrease Monte Carlo error.
  set.seed(5)
  do(3) * chisq( tally( ~ outcome + shuffle(treatment),  data=Mites ) )
  Mites.rand <- do(1000) * chisq( tally( ~ outcome + shuffle(treatment),  data=Mites ) )
  
  testcase1 <- 
    structure(
      list(statistic = c(`X-squared` = 5.68847857969099), 
           parameter = c(df = 1L), p.value = 0.0170766534882757, method = "Pearson's Chi-squared test with Yates' continuity correction", 
           data.name = "Mites.table", observed = structure(c(15L, 11L, 4L, 17L), 
                                                           .Dim = c(2L, 2L), 
                                                           .Dimnames = list(
                                                             outcome = c("no wilt", "wilt"), 
                                                             treatment = c("mites", "no mites")), 
                                                           class = "table"), 
           expected = structure(c(10.5106382978723, 15.4893617021277, 
                                  8.48936170212766, 12.5106382978723), 
                                .Dim = c(2L, 2L), 
                                .Dimnames = list(
                                  outcome = c("no wilt", "wilt"), 
                                  treatment = c("mites",  "no mites"))), 
           residuals = structure(c(1.38474578496035,  -1.14069053162584, -1.54080286914851, 1.26924325246454), 
                                 .Dim = c(2L, 2L), class = "table", 
                                 .Dimnames = list(outcome = c("no wilt", "wilt"), 
                                                  treatment = c("mites", "no mites"))), 
           stdres = structure(c(2.68397982358462,  -2.68397982358462, -2.68397982358462, 2.68397982358462), 
                              .Dim = c(2L, 2L), class = "table", 
                              .Dimnames = list(outcome = c("no wilt",  "wilt"), 
                                               treatment = c("mites", "no mites")))), class = "htest")
  
  testcase2 <- c(X.squared = 7.20374769340935)
  testcase3 <- c(X.squared = 5.68847857969099)
  
  expect_equal(ignore_attr = TRUE, chisq.test(Mites.table), testcase1)
  expect_equal(ignore_attr = TRUE, chisq(Mites.table), testcase2)
  expect_equal(ignore_attr = TRUE, chisq(chisq.test(Mites.table)), testcase3)
})
