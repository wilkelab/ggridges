# context("cor.test()")

testthat::test_that("Cor test works", {
  require(graphics)
  
  testcase1 <- structure(list(statistic = c(t = -0.860504567943113), parameter = c(df = 41L), 
                              p.value = 0.394515192722409, estimate = c(cor = -0.133190890463189), 
                              null.value = c(correlation = 0), alternative = "two.sided", 
                              method = "Pearson's product-moment correlation", data.name = "CONT and INTG", 
                              conf.int = structure(c(-0.41685910878005, 0.174118233649785
                              ), conf.level = 0.95)), class = "htest")
  
  testcase2 <- structure(list(statistic = c(t = -0.860504567943113), parameter = c(df = 41L), 
                              p.value = 0.394515192722409, estimate = c(cor = -0.133190890463189), 
                              null.value = c(correlation = 0), alternative = "two.sided", 
                              method = "Pearson's product-moment correlation", data.name = "CONT and INTG", 
                              conf.int = structure(c(-0.41685910878005, 0.174118233649785
                              ), conf.level = 0.95)), class = "htest")
  
  expect_equal(ignore_attr = TRUE, testcase1, cor.test(~ CONT + INTG, data = USJudgeRatings))
  expect_equal(ignore_attr = TRUE, testcase2, cor.test(CONT ~ INTG, data = USJudgeRatings))
})
