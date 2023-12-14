# context("rgeo()")

testthat::test_that("rgeo works", {
  set.seed(17)
   
  testcase1 <- structure(list(lat = c(-43.622061623627, 69.5139810975232, -3.63922895500869, 
                                     33.6170444353752), lon = c(-33.1611331645399, 13.9669734612107, 
                                                                -105.52044111304, -112.642720090225)), class = "data.frame", row.names = c(NA, 
                                                                                                                                           -4L))
  testcase2 <- structure(list(lat = c(28.4158008253407, 49.0415283876069, 35.6922996703392, 
                                      43.5825112699598), lon = c(-100.526855527423, -92.6721710897982, 
                                                                 -112.586740185507, -113.773786681704)), class = "data.frame", row.names = c(NA, 
                                                                                                                                             -4L))
  
  expect_equal(ignore_attr = TRUE, testcase1, rgeo(4))
  # sample from a region that contains the continental US
  set.seed(17)
  expect_equal(ignore_attr = TRUE, testcase2, rgeo(4, latlim = c(25,50), lonlim = c(-65, -125)))
  
})
