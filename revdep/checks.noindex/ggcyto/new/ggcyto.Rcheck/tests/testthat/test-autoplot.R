context("autoplot")
#no effect since vdiffr capture and report the warnings anyway
# options(warn = -1)#suppress removing rows warnings from ggplot

fs <- GvHD[subset(pData(GvHD), Patient %in%5 & Visit %in% c(5:6))[["name"]]]
fs <- flowSet_to_cytoset(fs)
set.seed(1)#due to subsampling
test_that("autoplot -- cytoframe", {
  suppressWarnings(expect_doppelganger("autoplot-fr-1d", autoplot(fs[[1]], x = 'FSC-H')))
  suppressWarnings(expect_doppelganger("autoplot-fr-2d", autoplot(fs[[1]], x = 'FSC-H', y = 'SSC-H', bins = 128)))
  
})
test_that("autoplot -- flowset", {
  suppressWarnings(expect_doppelganger("autoplot-fs-1d", autoplot(fs, x = 'FSC-H')))
  suppressWarnings(expect_doppelganger("autoplot-fs-2d", autoplot(fs, x = 'FSC-H', y = 'SSC-H', bins = 128)))
  
})


test_that("autoplot -- gatingset", {
 #exaggerate the gate difference between two samples in order to test whether plot will refect it
  g <- gh_pop_get_gate(gs[[2]], "CD3")
  g@min[1] <- 1800
  gh_pop_set_gate(gs[[2]], "CD3", g)
  suppressWarnings(expect_doppelganger("autoplot-gs-1-gate", autoplot(gs, "CD3")))
  suppressWarnings(expect_doppelganger("autoplot-gs-2-gate", autoplot(gs, c("CD3", "CD19"))))
  
  suppressWarnings(expect_doppelganger("autoplot-gs-bool-gate", autoplot(gs, "CD19andCD20")))
  
})

#TODO:tackle error of 'svglite only supports one page'
# test_that("autoplot -- flowFrame", {
#   expect_doppelganger("autoplot-fr", autoplot(fs[[1]]) + labs_cyto("marker"))
#   
# })

# test_that("autoplot -- gatinghierarchy", {
#   gh <- gs[[1]]
#   nodes <- gs_get_pop_paths(gh, path = "auto")[c(3:6)]
#   
#   expect_doppelganger("autoplot-gs-1-gate", autoplot(gh, nodes))
# })
# 
# test_that("autoplot -- ggcyto_arrange", {
#   nodes <- gs_get_pop_paths(gs[[1]], path = "auto")[c(3:6)]
#   res <- autoplot(gs[[1]], nodes)
#   expect_is(res, "ggcyto_GatingLayout")
#   # arrange it as one-row gtable object 
#   gt <- ggcyto_arrange(res, nrow = 1)
#   expect_is(gt, "gtable")
#   # do the same to the second sample
#   gt2 <- ggcyto_arrange(autoplot(gs[[2]], nodes), nrow = 1)
#   # combine the two and print it on the sampe page
#   gt3 <- gridExtra::gtable_rbind(gt, gt2)
#   # plot(gt3)
#   expect_doppelganger("autoplot-ggcyto_arrange", gt3)
#   
# })

# options(warn = 0)#restore default