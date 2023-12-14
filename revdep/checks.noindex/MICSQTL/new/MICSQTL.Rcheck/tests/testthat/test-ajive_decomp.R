test_that("ajive decomposition works", {
  data(se)
  se <- ajive_decomp(se, use_marker = TRUE, level = "bulk", ini_rank = c(5,5))
  expect_equal(
    ncol(metadata(se)$cns),
    metadata(se)$joint_rank
  )
  expect_equal(
    nrow(metadata(se)$cns),
    ncol(assay(se))
  )
})
