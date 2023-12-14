context("ggcyto-flowSet")

# options(warn = -1)#suppress removing rows warnings from ggplot

set.seed(1)#due to subsampling
fs <- GvHD[subset(pData(GvHD), Patient %in%5:7 & Visit %in% c(5:6))[["name"]]]
fs <- flowSet_to_cytoset(fs)
fr <- fs[[1, return = "cyto"]]

test_that("fs", {

  ## ------------------------------------------------------------------------
  p <- ggcyto(fs, aes(x = `FSC-H`)) 
  expect_is(p, "ggplot")
  
  p1 <- p + geom_histogram() 
  suppressWarnings(expect_doppelganger("ggcyto-fs-1d", p1))  
  
  suppressWarnings(expect_doppelganger("ggcyto-fs-1d-facet", p1 + facet_grid(Patient~Visit)))  
  
  suppressWarnings(expect_doppelganger("ggcyto-fs-1d-density", p + geom_density()))  
  
  suppressWarnings(expect_doppelganger("ggcyto-fs-1d-density-black", p + geom_density(fill = "black")))  
  
  suppressWarnings(expect_doppelganger("ggcyto-fs-1d-density-alpha", ggcyto(fs, aes(x = `FSC-H`, fill = name)) + geom_density(alpha = 0.2)))  
  #TODO:reproduce and fix this test case once cairo2.0 is installed on rhino
  # suppressWarnings(expect_doppelganger("ggplot-fs-1d-density-black", ggplot(fs, aes(x = `FSC-H`, fill = name)) + geom_density(alpha = 0.2)))  
  
  p <- ggcyto(fs, aes(x = `FSC-H`, y =  `SSC-H`))
  p <- p + geom_hex(bins = 128)
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-hex", p))  
  
  p <- p + ylim(c(10,9e2)) + xlim(c(10,9e2))   
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-hex-lim", p))  
  
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-hex-gradien", p + scale_fill_gradientn(colours = rainbow(7), trans = "sqrt")))  
  lg <- flowStats::lymphGate(fs, channels=c("FSC-H", "SSC-H"),scale=0.6)
  #Apply lg to multiple samples
  fres <- filter(fs, lg)
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-hex-polygates", p + geom_gate(lg)))  
  
  ## ------------------------------------------------------------------------
  rect.g <- rectangleGate(list("FSC-H" =  c(300,500), "SSC-H" = c(50,200)))
  rect.gates <- sapply(sampleNames(fs), function(sn)rect.g)
  
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-hex-rectgate", p + geom_gate(rect.gates)))  
  
  ## ------------------------------------------------------------------------
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-hex-rectgate-stats", p + geom_gate(rect.gates) + geom_stats(size = 3)))  
  
  ## ------------------------------------------------------------------------
  den.gates.x <- fsApply(fs, openCyto::gate_mindensity, channel = "FSC-H", gate_range = c(100, 300), adjust = 1)
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-hex-1dgate", p + geom_gate(den.gates.x) + geom_stats()))  
  
  ## ------------------------------------------------------------------------
  den.gates.y <- fsApply(fs, openCyto::gate_mindensity, channel = "SSC-H", gate_range = c(100, 500), adjust = 1, positive = FALSE)
  
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-hex-1dgate-static-stats", p + geom_gate(den.gates.y) + geom_stats(value = lapply(rect.gates, function(g)0.1))))  
  
  ## ------------------------------------------------------------------------
  suppressWarnings(expect_doppelganger("ggcyto-fs-1d-den-stats", ggcyto(fs, aes(x = `FSC-H`)) + geom_density(fill = "black", aes(y = ..scaled..)) + geom_gate(den.gates.x)  + geom_stats(type = "count")))  
  
  ## ------------------------------------------------------------------------
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-multi-gates", p + geom_gate(lg) + geom_gate(rect.gates) + geom_stats(size = 3)))  
  
  ## ------------------------------------------------------------------------
  suppressWarnings(expect_doppelganger("ggcyto-fs-2d-multi-gates-single-stats", p + geom_gate(lg) + geom_gate(rect.gates) + geom_stats(gate = lg, size = 3)))  
  
  ## ------------------------------------------------------------------------
  expect_is(p, "ggcyto_flowSet")
  expect_is(p$data, "cytoset")
  
  p <- as.ggplot(p)
  expect_is(p, "gg")
  expect_is(p$data, "data.frame")
  

})