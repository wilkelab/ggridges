skip_if_not_installed('ggridges')

test_that("Ridgeline plot has the correct content", {
  
  data(spas)
  sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
  plt = gr_plot_ridge(sep, years = c(1960, 1965, 1989, 2001, 2012)) 
  
  expect_type(plt, 'list')
  expect_s3_class(plt, 'ggplot')
  expect_s3_class(plt$layers[[1]]$geom, 'GeomRidgeline')
  expect_equal(plt$layers[[1]]$geom$default_aes$datatype, 'ridgeline')
  
})