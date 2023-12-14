test_that("Params are correct", {
  
  params = gr_get_params(reg = 'center')
  
  expect_equal(length(params), 39)
  expect_type(params, 'list')
  expect_type(params$filter, 'character')
  
  params$filter = NULL
  for (p in params)
    expect_type(p, 'double')
  
})