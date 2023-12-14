# context("oddsRatio")

M1 <- matrix(c(14, 38, 51, 11), nrow = 2)
M2 <- matrix(c(18515, 18496, 1427, 1438), nrow = 2)
rownames(M2) <- c("Placebo", "Aspirin")
colnames(M2) <- c("No", "Yes")
testthat::test_that("oddsRatio works", {
  
  testcase1 <- structure(c(OR = 12.5844155844156), 
                         p1 = 0.215384615384615, 
                         p2 = 0.775510204081633, 
                         o1 = 0.274509803921569, 
                         o2 = 3.45454545454545, 
                         OR = c(OR = 12.5844155844156), 
                         lower.OR = c(OR = 5.14495591295458), 
                         upper.OR = c(OR = 30.7811219922264), 
                         RR = c(RR = 3.60058309037901), 
                         lower.RR = c(RR = 2.21058967342817), 
                         upper.RR = c(RR = 5.86458841573184), 
                         conf.level = 0.95, 
                         class = c("oddsRatio", "numeric"))
  
  testcase2 <- structure(c(OR = 0.991332141702194), 
                         p1 = 0.928442483201284, 
                         p2 = 0.927861944416575, 
                         o1 = 12.9747722494744, 
                         o2 = 12.8623087621697, 
                         OR = c(OR = 0.991332141702194), 
                         lower.OR = c(OR = 0.918767332114681), 
                         upper.OR = c(OR = 1.06962816463003), 
                         RR = c(RR = 0.999374717556324), 
                         lower.RR = c(RR = 0.993931410834506), 
                         upper.RR = c(RR = 1.00484783477386), 
                         conf.level = 0.95, 
                         class = c("oddsRatio", "numeric"))
  
  testcase3 <- structure(c(RR = 0.999374717556324), 
                         p1 = 0.928442483201284, 
                         p2 = 0.927861944416575, 
                         o1 = 12.9747722494744, 
                         o2 = 12.8623087621697, 
                         OR = c(OR = 0.991332141702194), 
                         lower.OR = c(OR = 0.918767332114681), 
                         upper.OR = c(OR = 1.06962816463003), 
                         RR = c(RR = 0.999374717556324), 
                         lower.RR = c(RR = 0.993931410834506), 
                         upper.RR = c(RR = 1.00484783477386), 
                         conf.level = 0.95, 
                         class = c("relrisk", "numeric"))
  
  expect_equal(ignore_attr = TRUE, testcase1, oddsRatio(M1))
  expect_equal(ignore_attr = TRUE, testcase2, oddsRatio(M2))
  expect_output(oddsRatio(M2, verbose = TRUE))
  expect_output(relrisk(M2, verbose = TRUE))
  require(mosaicData)
  
  testcase4 <- structure(c(RR = 1.43473360655738), 
                         p1 = 0.191387559808612, 
                         p2 = 0.274590163934426, 
                         o1 = 0.236686390532544, 
                         o2 = 0.378531073446328, 
                         OR = c(OR = 1.59929378531073), 
                         lower.OR = c(OR = 1.0251193072434), 
                         upper.OR = c(OR = 2.49506627536988), 
                         RR = c(RR = 1.43473360655738), 
                         lower.RR = c(RR = 1.01577963780372), 
                         upper.RR = c(RR = 2.02648334853006), 
                         conf.level = 0.95, 
                         class = c("relrisk", "numeric"))
  
  expect_equal(ignore_attr = TRUE, testcase4, relrisk(tally(~ homeless + sex, data = HELPrct) ))
})
