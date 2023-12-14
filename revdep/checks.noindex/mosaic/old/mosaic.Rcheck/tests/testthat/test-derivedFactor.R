# context("derivedFactor()")

testthat::test_that("derivedFactor Works", {
  Kf <- mutate(KidsFeet, biggerfoot2 = derivedFactor(
                     dom = biggerfoot == domhand,
                     nondom = biggerfoot != domhand)
                     )
  # Method 1: explicitly define all levels
  modHELP <- mutate(HELPrct, drink_status = derivedFactor(
    abstinent = i1 == 0,
    moderate = (i1>0 & i1<=1 & i2<=3 & sex=='female') |
      (i1>0 & i1<=2 & i2<=4 & sex=='male'),
    highrisk = ((i1>1 | i2>3) & sex=='female') |
      ((i1>2 | i2>4) & sex=='male'),
    .ordered = TRUE)
  )
  
  # Method 2: Use .default for last level
  modHELP2 <- mutate(HELPrct, drink_status = derivedFactor(
    abstinent = i1 == 0,
    moderate = (i1<=1 & i2<=3 & sex=='female') |
      (i1<=2 & i2<=4 & sex=='male'),
    .ordered = TRUE,
    .method = "first",
    .default = "highrisk")
  )
  
  # Method 3: use TRUE to catch any fall through slots
  modHELP3 <- mutate(HELPrct, drink_status = derivedFactor(
    abstinent = i1 == 0,
    moderate = (i1<=1 & i2<=3 & sex=='female') |
      (i1<=2 & i2<=4 & sex=='male'),
    highrisk=TRUE,
    .ordered = TRUE,
    .method = "first"
  )
  )
  regressionTest <- structure(list(name = structure(c(10L, 24L, 36L, 20L, 23L, 34L, 13L, 4L, 14L, 
                                                      8L, 29L, 33L, 5L, 6L, 21L, 22L, 7L, 28L, 26L, 
                                                      19L, 3L, 20L, 25L, 15L, 31L, 16L, 1L, 10L, 30L, 
                                                      11L, 9L, 4L, 27L, 12L, 32L, 17L, 35L, 18L, 2L), 
                                                    .Label = c("Abby", "Alisha", "Andy", "Caitlin", "Cal", 
                                                               "Cam", "Caroline", "Damon", "Danielle", "David",
                                                               "Dwayne", "Dylan", "Edward", "Eleanor", "Erica", "Glen", "Hannah",
                                                               "Hayley", "Heather", "Josh", "Julie", "Kate", "Lang", "Lars",
                                                               "Laura", "Lee", "Leigh", "Maggie", "Mark", "Mike", "Peggy", "Peter", 
                                                               "Ray", "Scotty", "Teshanna", "Zach"), class = "factor"), 
                                   birthmonth = c(5L,  10L, 12L, 1L, 2L, 3L, 2L, 6L, 5L, 9L, 9L, 3L, 8L, 3L, 11L, 4L,  12L, 3L, 6L, 
                                                  3L, 6L, 7L, 9L, 9L, 10L, 7L, 2L, 12L, 11L, 8L, 6L, 7L, 3L, 4L, 4L, 3L, 3L, 1L, 9L), 
                                   birthyear = c(88L, 87L, 87L, 88L, 88L, 88L, 88L, 88L, 88L, 88L, 87L, 88L, 87L, 88L, 87L, 88L, 87L, 
                                                 88L, 88L, 88L, 88L, 88L, 88L, 88L, 88L, 88L, 88L, 87L, 88L, 88L, 88L, 88L, 88L, 88L, 
                                                 88L, 88L, 88L, 88L, 88L), 
                                   length = c(24.4,  25.4, 24.5, 25.2, 25.1, 25.7, 26.1, 23, 23.6, 22.9, 27.5, 24.8,  26.1, 27, 26, 
                                              23.7, 24, 24.7, 26.7, 25.5, 24, 24.4, 24, 24.5,   24.2, 27.1, 26.1, 25.5, 24.2, 23.9, 
                                              24, 22.5, 24.5, 23.6, 24.7,   22.9, 26, 21.6, 24.6), 
                                   width = c(8.4, 8.8, 9.7, 9.8, 8.9, 9.7,  9.6, 8.8, 9.3, 8.8, 9.8, 8.9, 9.1, 9.8, 9.3, 7.9, 8.7, 8.8, 
                                             9,  9.5, 9.2, 8.6, 8.3, 9, 8.1, 9.4, 9.5, 9.5, 8.9, 9.3, 9.3, 8.6, 8.6, 9, 8.6, 8.5, 9, 7.9, 8.8), 
                                   sex = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 
                                                     1L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L,  2L, 2L, 2L, 2L), 
                                                   .Label = c("B", "G"), 
                                                   class = "factor"), 
                                   biggerfoot = structure(c(1L,    1L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L,2L, 
                                                            1L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L), 
                                                          .Label = c("L", "R"), 
                                                          class = "factor"), 
                                   domhand = structure(c(2L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                         1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 
                                                         2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L), 
                                                       .Label = c("L", "R"), class = "factor"), 
                                   biggerfoot2 = structure(c(2L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 1L,
                                                             2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 
                                                             2L, 2L, 2L, 2L, 2L, 1L, 2L), .Label = c("dom", "nondom"), 
                                                           class = "factor")), 
                              class = "data.frame", 
                              row.names = c(NA, -39L))
  
  
  testcase1 <- structure(c(2L, 11L, 20L, 6L), 
                         .Dim = c(2L, 2L), 
                         .Dimnames = list(biggerfoot = c("L", "R"), 
                                          biggerfoot2 = c("dom", "nondom")), 
                         class = "table")
  testcase2 <- structure(c(2L, 6L, 20L, 11L), 
                         .Dim = c(2L, 2L), 
                         .Dimnames = list(biggerfoot = c("L", "R"), 
                                          domhand = c("L", "R")), 
                         class = "table")
  
  testcase3 <- structure(c(abstinent = 68L, 
                           moderate = 28L, 
                           highrisk = 357L), 
                         .Dim = 3L, 
                         .Dimnames = list(drink_status = c("abstinent", "moderate", "highrisk")), 
                         class = "table")
  
  testcase4 <- structure(c(abstinent = 68L, moderate = 28L, highrisk = 357L), .Dim = 3L, .Dimnames = list(
    drink_status = c("abstinent", "moderate", "highrisk")), class = "table")
  
  testcase5 <- structure(c(abstinent = 68L, moderate = 28L, highrisk = 357L), .Dim = 3L, .Dimnames = list(
    drink_status = c("abstinent", "moderate", "highrisk")), class = "table")
  
  expect_equal(ignore_attr = TRUE, regressionTest, Kf)
  expect_equal(ignore_attr = TRUE, testcase1, tally( ~ biggerfoot + biggerfoot2, data = Kf) )
  expect_equal(ignore_attr = TRUE, testcase2, tally( ~ biggerfoot + domhand, data = Kf) )
  expect_equal(ignore_attr = TRUE, testcase3, tally( ~ drink_status, data = modHELP) )
  expect_equal(ignore_attr = TRUE, testcase4, tally( ~ drink_status, data = modHELP2) )
  expect_equal(ignore_attr = TRUE, testcase5, tally( ~ drink_status, data = modHELP3) )
})
