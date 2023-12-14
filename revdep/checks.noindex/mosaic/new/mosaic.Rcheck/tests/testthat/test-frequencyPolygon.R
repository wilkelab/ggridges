# context("frequencypolygon")

testthat::test_that("freqpolygon() examples are working",{
 
  wrapped_expect_doppelganger(
    "FirstExample", 
    freqpolygon(~age | substance, data = HELPrct, v = 35))
  
  wrapped_expect_doppelganger(
    "SecondExample", 
    freqpolygon(~age, data = HELPrct, labels = TRUE, type = 'count'))
  
  wrapped_expect_doppelganger(
    "ThirdExample", 
    freqpolygon(~age | substance, data = HELPrct, groups = sex))
  
  wrapped_expect_doppelganger(
    "FourthExample", 
    freqpolygon(~age | substance, data = HELPrct, groups = sex, ylim = c(0,0.11)))
})
