
# context("Confidence Intervals")

# Examples to test
#' 
#' 
#' 
#' confint(bootstrap)
#'   confint(bootstrap, method = "percentile")
#'   confint(bootstrap, method = "boot")
#'   confint(bootstrap, method = "se", df = nrow(HELPrct) - 1)
#'   confint(bootstrap, margin.of.error = FALSE)
#'   confint(bootstrap, margin.of.error = TRUE, level = 0.99, 
#'     method = c("boot", "se", "perc") )
#'   bootstrap2 <- do(500) * mean( resample(1:10) ) 
#'   confint(bootstrap2)
getData <- function() {
  structure(list(diffmean = c(-0.865114560236513, 0.36778884147305, 

  -1.69679327521794, -0.515578957538288, -1.94702093397746, -0.0728026194617826, 
  -1.86344537815125, 0.230871807028976, -0.403286509408957, -1.10612170087977, 
  -0.0187560975609742, -0.937450304797245, 0.336827195467421, -1.20303776683087, 
  0.197786961013534, -0.150530871031052, -1.53303441197126, -2.03656156156156, 
  -1.80459133832628, -0.148356940509913, -1.44662309368191, -1.66299316729116, 
  -2.29584489012161, -0.731077910853188, 0.46941966871308, -1.24377675993229, 
  -0.806121886709278, -1.19370508766887, 1.55603328710125, -0.169223921240054, 
  -2.57113314447592, -2.41906367244848, -1.04849036959916, -0.828693977591037, 
  1.25994772775763, 0.0965588084232181, -0.737388972683092, -0.471512012342956, 
  -0.542079207920793, -2.34084066000241, -1.6808725353206, -0.341452425125894, 
  -0.661978492482326, -0.0747721258453424, -2.27637620874075, -1.66087138667783, 
  -1.70732009311079, -0.523727422003283, -1.61968125474323, -1.90693817628188
  )), class = c("do.data.frame", "data.frame"), row.names = c(NA, -50L), 
  lazy = ~diffmean(age ~ sex, data = resample(HELPrct)), culler = function (object,...) 
  { UseMethod("cull_for_do") })
}

getDataAlternate <- function() {
  structure(list(mean = c(4.9, 5.2, 6, 7.3, 6.1, 6.7, 7.1, 5.7, 

    5.1, 4.3, 6, 4.5, 6.4, 5.3, 5.6, 3.8, 5.9, 6.1, 5.8, 5.7, 5.6, 
    4.5, 5.6, 6.7, 4.5, 6, 6.8, 4.3, 7.5, 6.4, 6.7, 4.6, 3.9, 7.2, 
    4.6, 4.9, 4.7, 7.3, 6.7, 5.2, 5.4, 6.1, 5.6, 5.5, 6.5, 4.8, 5.1, 
    5.8, 5, 4.9, 5.7, 6.3, 4.7, 6.2, 6.2, 6.1, 5.3, 6.6, 3.6, 5, 
    4.5, 3.9, 3.9, 4.9, 6.3, 4.8, 6.7, 6.5, 5.1, 6.1, 5.8, 4.1, 8.3, 
    6.6, 4.8, 6.6, 5.7, 4.8, 5.3, 6.9, 5.7, 4.3, 4.1, 6.5, 4.6, 5.4, 
    6, 4.3, 5, 4.7, 6.7, 7, 4.5, 7.7, 5.4, 4.6, 7.8, 7.2, 5.7, 6.1, 
    6.2, 4.5, 4.3, 6.7, 5, 6, 6, 6.1, 5.7, 4.7, 5.6, 5, 5.9, 5.8, 
    5.1, 6.4, 7.2, 6.3, 6, 4.4, 4.1, 7.2, 5.7, 5.3, 5.2, 5.6, 5.9, 
    5.8, 7.2, 5.7, 6.1, 4.8, 5.5, 5.1, 5.2, 3.6, 6.3, 5.5, 5.7, 6.3, 
    5.5, 6.1, 6.7, 5.2, 5.5, 4.7, 4.3, 6.1, 5.1, 5.2, 5.9, 5.6, 5.8, 
    4, 6.6, 5.6, 5.8, 5.3, 6.1, 3.2, 6.7, 4.8, 4.1, 5.2, 4.1, 4.8, 
    5.5, 3.2, 5.4, 6, 4, 4.6, 5, 6.2, 6.4, 6.4, 6.3, 5.5, 6.7, 7.5, 
    6.4, 6.2, 4.7, 6, 5.5, 5.2, 5.4, 6.7, 7.7, 5.3, 5.2, 5.9, 6.2, 
    4.8, 5.7, 5.9, 6.2, 4.8, 7.3, 5.3, 5.9, 6.8, 6.1, 5.3, 5.9, 3.4, 
    3.7, 4.6, 5.4, 3.9, 5.8, 7.7, 5.6, 5.9, 5.1, 5.7, 5.3, 5.1, 5.2, 
    5.9, 6.2, 6.3, 4.8, 5.1, 5.8, 5.5, 6, 5.6, 4.7, 4.7, 5.7, 5, 
    6.3, 5.5, 4.1, 4.7, 4.9, 4.4, 6.5, 4.1, 5.3, 6.6, 4.3, 5.7, 7.9, 
    6.7, 5.1, 5.9, 5.6, 5.6, 5.6, 6, 5.3, 6.6, 5.3, 5.7, 5.2, 5.1, 
    5.8, 5.2, 6.7, 5, 4, 5.7, 6.1, 4.7, 4.5, 4.3, 6, 5.5, 4, 5.3, 
    4.2, 4.3, 6.6, 5.1, 4.4, 6.5, 7.1, 5.1, 6.3, 5.9, 5.5, 4.5, 5.7, 
    4.4, 7.7, 5, 4.8, 5.3, 7.2, 5.1, 4.6, 6.9, 3.5, 5.5, 6.2, 5.6, 
    5.8, 5.9, 6.6, 6.7, 5.8, 6.5, 6.2, 5.6, 5, 5, 5.3, 4.9, 5.7, 
    5.7, 5.5, 5.8, 3.3, 6.2, 6.4, 5, 4.8, 7.3, 4.9, 6.7, 5.7, 5.2, 
    6.2, 7.4, 5.9, 4.3, 6, 6, 6, 4.3, 6, 6.9, 6.4, 5.6, 6.4, 5.2, 
    5, 5.8, 6.1, 5.4, 4.8, 4.9, 5.6, 5.7, 6, 5, 6.9, 7.4, 4.5, 5.1, 
    4.8, 6.1, 5.9, 6.7, 5.3, 7, 6.8, 6.4, 5.3, 5.3, 6.4, 6.8, 4.6, 
    6, 4.8, 6.3, 5.5, 6.3, 6.2, 5.5, 6.2, 6.3, 7, 6.3, 5.2, 5, 5.3, 
    5.5, 4.6, 6.4, 6.4, 6.9, 5.8, 4.3, 4.6, 6.6, 6, 4.9, 5.2, 5.1, 
    5.5, 4.7, 5, 6, 4.7, 6.7, 5.6, 7.2, 6.7, 5.8, 5.8, 5.8, 4.6, 
    5.2, 6.3, 4.9, 5.2, 6.1, 7.7, 4.5, 5.1, 6, 6.4, 5.5, 5.1, 4.5, 
    4.9, 5.6, 6.6, 3.9, 4.9, 4.7, 5.5, 5.9, 5, 5.2, 4.9, 5.4, 6.4, 
    4.6, 5.2, 5, 6.4, 4.9, 5.9, 5.4, 5.1, 6, 4, 5.1, 6, 5.7, 4.2, 
    5.8, 4, 4.5, 4.2, 5.8, 5.9, 5.3, 5.2, 4.1, 4.4, 7, 4, 6, 6.2, 
    6.2, 6.6, 4.8, 5.3, 6.6, 5.5, 5.3, 7.5, 6, 7.3, 4.9, 5.6, 5.7, 
    6.2, 5.1, 4.1, 5.3, 6, 4.3, 6.4, 6.2, 7.2, 4.3, 6.4, 5, 6, 6.1, 
    6.2, 6.5, 4.4, 6.9, 5.1, 5.3, 6.2, 5.4, 6.1, 6.1, 5.7, 4.5, 4.6, 
    3.9)), class = c("do.data.frame", "data.frame"), 
    row.names = c(NA, -500L), lazy = ~mean(resample(1:10)), 
    culler = function (object, ...) { UseMethod("cull_for_do") })

}

testthat::test_that("Confidence Intervals created", {
  bootstrap <- getData()
  bootstrap2 <- getDataAlternate()
  

  result1 <- 
    structure(list(name = "diffmean", lower = -2.40146349464812, 
                   upper = 1.0820789144726, level = 0.95, method = "percentile", 
                   estimate = -0.784128356112582), row.names = c(NA, -1L), 
              class = "data.frame")
  
  result2 <- 
    structure(list(name = "diffmean", lower = -2.77996366912654, 
                   upper = 0.989971357711456, level = 0.95, method = "stderr", 
                   estimate = -0.784128356112582, margin.of.error = 1.884967513419, 
                   df = 452), row.names = c(NA, -1L), class = "data.frame")
  
  result3 <- 
    structure(list(name = "diffmean", lower = -2.77996366912654, 
                   upper = 0.989971357711456, level = 0.95, method = "stderr", 
                   estimate = -0.784128356112582, margin.of.error = 1.884967513419, 
                   df = 452), row.names = c(NA, -1L), class = "data.frame")
  
  result4 <- 
    structure(list(name = "diffmean", lower = -2.40146349464812, 
                   upper = 1.0820789144726, level = 0.95, method = "percentile", 
                   estimate = -0.784128356112582), row.names = c(NA, -1L), 
              class = "data.frame")
  
  result5 <- 
    structure(list(name = c("diffmean", "diffmean"), 
                   lower = c(-3.37610477860545, -2.53387612382919), 
                   upper = c(1.58611246719036, 1.48349232506206), 
                   level = c(0.99, 0.99), method = c("stderr", "percentile"), 
                   estimate = c(-0.784128356112582, -0.784128356112582), 
                   margin.of.error = c(2.4811086228979, NA), df = c(452, NA)), 
              row.names = c(NA, -2L), class = "data.frame")
  
  result6 <- 
    structure(list(name = "mean", lower = 3.9, upper = 7.3525, level = 0.95, 
                   method = "percentile", estimate = 5.5), row.names = c(NA, -1L), 
              class = "data.frame")
  
  result7 <- 
    structure(list(name = logical(0), lower = logical(0), upper = logical(0), 
                   level = logical(0), method = logical(0)), 
              class = "data.frame", row.names = integer(0))
  
  expect_equal(confint(bootstrap), result1)
  
  expect_equal(confint(bootstrap, method="stderr"), result2)
  
  expect_equal(confint(bootstrap, method = "se", df = nrow(HELPrct) - 1), result3)
  
  expect_equal(confint(bootstrap, margin.of.error = FALSE), result4)
  
  expect_equal(confint(bootstrap, margin.of.error = TRUE, level = 0.99, 
                       method = c("boot", "se", "perc")), result5)
  
  expect_equal(confint(bootstrap2), result6)
  

  # expect_equal(confint(bootstrap, method = "boot"), result7)

  
})


