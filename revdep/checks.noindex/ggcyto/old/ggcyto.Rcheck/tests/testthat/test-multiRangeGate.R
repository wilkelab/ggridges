context("ggcyto-flowSet")


set.seed(1)#due to subsampling
fs <- GvHD[1:2]

test_that("multiRangeGate", {
  
  gates = list(multiRangeGate(ranges = list(min=c(100, 350), max=c(250, 400))),
               multiRangeGate(ranges = list(min=c(150), max=c(380))))
  
  names(gates)<-sampleNames(fs)
  ## ------------------------------------------------------------------------
  suppressWarnings(expect_doppelganger("ggcyto-fs-1d-multiRangeGate", autoplot(fs, "Time") + geom_gate(gates) + geom_stats()))  
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-multiRangeGate", autoplot(fs, "Time",  "FSC-H") + geom_gate(gates) + geom_stats()))  
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-multiRangeGate-flip", autoplot(fs, "FSC-H", "Time") + geom_gate(gates) + geom_stats()))  
  

})
  