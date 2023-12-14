data(spas) 
sep = gr_separate(spas)

test_that("Matrix ggplot has the correct content", {
  
  plt = suppressWarnings(gr_plot_matrix(sep, type = 'runoff'))
  expect_type(plt, 'list')
  expect_s3_class(plt, 'ggplot')
  expect_s3_class(plt$layers[[1]]$geom, 'GeomRaster')
  
  plt = suppressWarnings(gr_plot_matrix(sep, type = 'season'))
  expect_type(plt, 'list')
  expect_s3_class(plt, 'ggplot')
  expect_s3_class(plt$layers[[1]]$geom, 'GeomRaster')
  
  plt = suppressWarnings(gr_plot_matrix(sep, type = 'component'))
  expect_type(plt, 'list')
  expect_s3_class(plt, 'ggplot')
  expect_s3_class(plt$layers[[1]]$geom, 'GeomRaster')
  
})