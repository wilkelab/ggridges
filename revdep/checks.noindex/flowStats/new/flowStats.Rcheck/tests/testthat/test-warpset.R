context("test warpset")

test_that("normalize", {
  #prepare data
  library(flowCore)
  data(ITN)
  library(openCyto)
  fs <- flowSet_to_cytoset(ITN[c(1,5,8)])
  gs <- GatingSet(fs)
  transList <- transformerList(colnames(fs)[3:7], logicle_trans())
  gs <- transform(gs, transList)  
  cs_lock(fs)
  
  gs_add_gating_method(gs, "L", "-", "root", "SSC", "mindensity")
  gs_add_gating_method(gs, "cd3", "+", "L", "cd3", "mindensity")
  gs_add_gating_method(gs, "cd4", "+", "cd3", "cd4", "mindensity")

  #' ## normalize cd4
  # library(ggridges)  
  # ggcyto(gs_pop_get_data(gs, "cd4"), aes(x = cd4)) + geom_density_ridges(aes(y = name)) + facet_null()
  reference_sample <- "sample01"
  pop_to_norm <- "cd4"
  dim <- "CD4"
  set.seed(1)
  gs1 <- normalize(gs, target = reference_sample
            , populations = pop_to_norm
            , dims = dim
            , minCountThreshold = 10
            , nPeaks = list('cd4' = 1)
            , chunksize = 10
            , bwFac = 2
            )
  # ggcyto(gs_pop_get_data(gs1, "cd4"), aes(x = cd4)) + geom_density_ridges(aes(y = name)) + facet_null()
  #the original gs unchanged and MFI variance is large
  expect_gt(var(gs_pop_get_stats(gs, type = pop.MFI, node = c("cd4"))[, "CD4"]), 1e-3)#TODO:has to quote CD4 here because somehow j expression of dt not working properly within devtools::test()
  #new gs is aligned on cd4
  expect_lt(var(gs_pop_get_stats(gs1, type = pop.MFI, node = c("cd4"))[, "CD4"]), 7e-5)
  
  })
