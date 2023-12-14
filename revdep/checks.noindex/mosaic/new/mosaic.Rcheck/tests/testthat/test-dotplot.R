# context("dotPlot()")

testthat::test_that("Dot Plot works", {
  require(mosaicData)
  
  wrapped_expect_doppelganger("dotplot1", dotPlot( ~ age, data = HELPrct))
  wrapped_expect_doppelganger("dotplot2", dotPlot( ~ age, nint=42, data = HELPrct))
  wrapped_expect_doppelganger("dotplot3", dotPlot( ~ height | voice.part, data = singer, nint = 17,
                                                   endpoints = c(59.5, 76.5), layout = c(4,2), aspect = 1,
                                                   xlab = "Height (inches)"))
  
  
})

