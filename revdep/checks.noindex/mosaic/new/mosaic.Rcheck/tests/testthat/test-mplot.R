# context("mplot")

testthat::test_that("lm mplot works", {
  
  
  testcases1 <- lm( width ~ length * sex, data = KidsFeet) |>
    mplot(multiplot=FALSE, which = 1:3, id.n = 5)
  
  testcases2 <- lm( width ~ length * sex, data = KidsFeet) |>
    mplot(smooth.color = "blue", smooth.size = 1.2, smooth.alpha = 0.3, id.size = 3)
  
  singletest <- lm(width ~ length * sex, data = KidsFeet) |>
    mplot(rows = 2:3, which = 7)
  
  
  num <- 1
  
  for(case in testcases1) {
    wrapped_expect_doppelganger(paste("mplot", as.character(num), sep = ""), case)
    num <- num + 1
  }
  
  for(case in testcases2) {
    wrapped_expect_doppelganger(paste("mplot", as.character(num), sep = ""), case)
    num <- num + 1
  }
  
  wrapped_expect_doppelganger(paste("mplot", as.character(num), sep = ""), singletest)
  num <- num + 1
  
  testthat::test_that("getVarFormula Works", {
    testcase <- structure(c(2.62, 2.875, 2.32, 3.215, 3.44, 3.46, 3.57, 3.19, 
                            3.15, 3.44, 3.44, 4.07, 3.73, 3.78, 5.25, 5.424, 5.345, 2.2, 
                            1.615, 1.835, 2.465, 3.52, 3.435, 3.84, 3.845, 1.935, 2.14, 1.513, 
                            3.17, 2.77, 3.57, 2.78, 21, 21, 22.8, 21.4, 18.7, 18.1, 14.3, 
                            24.4, 22.8, 19.2, 17.8, 16.4, 17.3, 15.2, 10.4, 10.4, 14.7, 32.4, 
                            30.4, 33.9, 21.5, 15.5, 15.2, 13.3, 19.2, 27.3, 26, 30.4, 15.8, 
                            19.7, 15, 21.4), .Dim = c(32L, 2L), 
                          .Dimnames = list(c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710", 
                                             "Hornet 4 Drive", "Hornet Sportabout",  
                                             "Valiant", "Duster 360", "Merc 240D", 
                                             "Merc 230", "Merc 280", "Merc 280C", "Merc 450SE", 
                                             "Merc 450SL", "Merc 450SLC", "Cadillac Fleetwood",
                                             "Lincoln Continental", "Chrysler Imperial", 
                                             "Fiat 128", "Honda Civic", "Toyota Corolla", 
                                             "Toyota Corona", "Dodge Challenger", "AMC Javelin", 
                                             "Camaro Z28", "Pontiac Firebird", "Fiat X1-9", 
                                             "Porsche 914-2", "Lotus Europa", "Ford Pantera L", 
                                             "Ferrari Dino", "Maserati Bora", "Volvo 142E"), 
                                           c("wt", "mpg")))
    
    expect_equal(ignore_attr = TRUE, testcase, getVarFormula( ~ wt + mpg, data = mtcars))
  })
})
