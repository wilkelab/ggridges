test_that("test gpd functions accept zero samples", {
  # All other features are well-tested by dist_genpareto
  expect_length(rgpd(0L), 0L)
  expect_length(dgpd(numeric()), 0L)
  expect_length(pgpd(numeric()), 0L)
  expect_length(qgpd(numeric()), 0L)
})
