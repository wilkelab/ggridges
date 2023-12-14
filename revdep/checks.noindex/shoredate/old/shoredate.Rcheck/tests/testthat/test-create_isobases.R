test_that("returns object of expected class", {
  skip_on_cran()
  isobases <- create_isobases(327)
  expect_equal(class(isobases), c("sf", "data.frame"))
})
