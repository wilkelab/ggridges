hpar = gr_help_params()

test_that("Params help is correct", {

  expect_type(hpar, 'list')
  expect_s3_class(hpar, 'data.frame')
  expect_equal(nrow(hpar), 39)
  
})

test_that("Every parameter has name, example, description and units", {
  
  expect_equal(sum(is.na(hpar$name)), 0)
  expect_equal(sum(is.na(hpar$example)), 0)
  expect_equal(sum(is.na(hpar$desc)), 0)
  expect_equal(sum(is.na(hpar$units)), 0)
  
})