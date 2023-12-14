# context("resample")

testthat::test_that("resample works", {
  require(mosaicData)
  
  testcase1 <- c(1L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 0L, 
                 0L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 
                 1L, 0L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 
                 1L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 
                 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 1L, 
                 1L, 1L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 
                 1L, 0L, 0L, 1L, 1L)
  
  testcase2 <- structure(list(name = structure(c(7L, 10L, 15L, 7L, 11L, 15L, 20L, 18L, 8L, 20L), 
                                               .Label = c("Abby", "Alisha", "Andy", "Caitlin", "Cal", "Cam", "Caroline", "Damon", "Danielle", "David", "Dwayne", 
                                                                                "Dylan", "Edward", "Eleanor", "Erica", "Glen", "Hannah", "Hayley", 
                                                                                "Heather", "Josh", "Julie", "Kate", "Lang", "Lars", "Laura", 
                                                                                "Lee", "Leigh", "Maggie", "Mark", "Mike", "Peggy", "Peter", "Ray", 
                                                                                "Scotty", "Teshanna", "Zach"), class = "factor"), 
                              birthmonth = c(12L,5L, 9L, 12L, 8L, 9L, 7L, 1L, 9L, 7L), 
                              birthyear = c(87L, 88L, 88L, 87L, 88L, 88L, 88L, 88L, 88L, 88L), 
                              length = c(24, 24.4,   24.5, 24, 23.9, 24.5, 24.4, 21.6, 22.9, 24.4), 
                              width = c(8.7,  8.4, 9, 8.7, 9.3, 9, 8.6, 7.9, 8.8, 8.6), sex = structure(c(2L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 1L), 
                                                                                                        .Label = c("B", "G"), 
                                                                                                        class = "factor"), 
                              biggerfoot = structure(c(2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L), 
                                                     .Label = c("L", "R"), 
                                                     class = "factor"), 
                              domhand = structure(c(1L,  2L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 2L), 
                                                  .Label = c("L", "R"), 
                                                  class = "factor"), 
                              orig.id = c("5", "4", "9", "5", "2", "9", "3", "8", "6", "3")), 
                         row.names = c("17", "1", "24", "17.1", "30", "24.1", "22", "38", "10", "22.1"),
                         class = "data.frame")
  
  testcase3 <- structure(c(B = 6L, G = 4L), 
                         .Dim = 2L, 
                         .Dimnames = list(sex = c("B", "G")), 
                         class = "table")
  
  
  testcase4 <- structure(list(name = structure(c(33L, 3L, 33L, 30L, 24L, 24L, 
                                                 4L, 27L, 22L, 22L), 
                                               .Label = c("Abby", "Alisha", "Andy", "Caitlin", 
                                                                                "Cal", "Cam", "Caroline", "Damon", "Danielle", "David", "Dwayne", 
                                                                                "Dylan", "Edward", "Eleanor", "Erica", "Glen", "Hannah", "Hayley", 
                                                                                "Heather", "Josh", "Julie", "Kate", "Lang", "Lars", "Laura", 
                                                                                "Lee", "Leigh", "Maggie", "Mark", "Mike", "Peggy", "Peter", "Ray", 
                                                                                "Scotty", "Teshanna", "Zach"), class = "factor"), 
                              birthmonth = c(3L,   6L, 3L, 11L, 10L, 10L, 6L, 3L, 4L, 4L), 
                              birthyear = c(88L, 88L,     88L, 88L, 87L, 87L, 88L, 88L, 88L, 88L), 
                              length = c(24.8, 24,  24.8, 24.2, 25.4, 25.4, 23, 24.5, 23.7, 23.7), 
                              width = c(8.9,  9.2, 8.9, 8.9, 8.8, 8.8, 8.8, 8.6, 7.9, 7.9), 
                              sex = structure(c(1L,  1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L), 
                                              .Label = c("B", "G"), 
                                              class = "factor"), 
                              biggerfoot = structure(c(1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                       2L, 2L), 
                                                     .Label = c("L", "R"), 
                                                     class = "factor"), 
                              domhand = structure(c(2L,  2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L), 
                                                  .Label = c("L", "R"), 
                                                  class = "factor"), 
                              orig.id = c("9.4.9", "4.7.4", "9.7.9", "10.9.7", "7.9.7", 
                                          "7.10.10", "6.3.3", "1.6.3", "3.1.6", "3.3.1"), 
                              id1 = c("B:4", "B:7", "B:7", "B:9", "B:9", "B:10", "G:3", "G:6", "G:1",  "G:3"), 
                              id2 = c("B:9", "B:4", "B:9", "B:7", "B:7", "B:10",  "G:3", "G:3", "G:6", "G:1")), 
                         row.names = c("9", "4", "9.1",   "10", "7", "7.1", "6", "1", "3", "3.1"), 
                         class = "data.frame")
  
  
  set.seed(13)
  expect_equal(ignore_attr = TRUE, testcase1, resample(0:1, 100))
  tally(resample(0:1, 100))
  set.seed(17) 
  Small <- sample(KidsFeet, 10)
  expect_equal(ignore_attr = TRUE, testcase2, resample(Small))
  set.seed(21) 
  Small <- sample(KidsFeet, 10)
  expect_equal(ignore_attr = TRUE, testcase3, tally(~ sex, data=resample(Small, groups=sex)))
  # shuffled can be used to reshuffle some variables within groups
  # orig.id shows where the values were in original data frame.
  set.seed(6)
  Small <- mutate(Small,
     id1 = paste(sex,1:10, sep=":"),
     id2 = paste(sex,1:10, sep=":"))
  expect_equal(ignore_attr = TRUE, testcase4, resample(Small, groups=sex, shuffled=c("id1","id2")))
  
})


testthat::test_that("rflip works", {
  
  
  testcase1 <- structure(5L, 
                         n = 10, 
                         prob = 0.5, 
                         sequence = c("H", "H", "H",  "T", "T", 
                                      "H", "T", "H", "T", "T"), 
                         verbose = TRUE, 
                         class = "cointoss")
  testcase2 <- structure(3L, 
                         n = 10, 
                         prob = 0.166666666666667, 
                         sequence = c("T", "T", "T", "H", "T", 
                                      "T", "T", "H", "T", "H"), 
                         verbose = FALSE, 
                         class = "cointoss")
  testcase3 <- structure(list(n = 10, 
                              heads = 1L, 
                              tails = 9, 
                              prob = 0.166666666666667), 
                         class = "data.frame", 
                         row.names = c(NA, -1L))
  
  set.seed(18)
  expect_equal(ignore_attr = TRUE, testcase1, rflip(10))
  set.seed(28)
  expect_equal(ignore_attr = TRUE, testcase2, rflip(10, prob = 1/6, quiet = TRUE))
  set.seed(5)
  expect_equal(ignore_attr = TRUE, testcase3, rflip(10, prob = 1/6, summarize = TRUE))
})
