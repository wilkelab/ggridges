data(spas) # example Spas-Zagorye data is included with grwat package

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))

test_that("Period plot has the correct content", {
  
  # One year
  plt = suppressWarnings(gr_plot_sep(sep, 1978))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  
  # Two years
  plt = suppressWarnings(gr_plot_sep(sep, c(1978, 1989)))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  
  # Four years in a matrix layout
  plt = suppressWarnings(gr_plot_sep(sep, 1988:1991, layout = matrix(1:4, nrow = 2, byrow = TRUE))) 
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  
  # Add temperature
  plt = suppressWarnings(gr_plot_sep(sep, 1991, temp = TRUE))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  
  # Add precipitation
  plt = suppressWarnings(gr_plot_sep(sep, 1991, prec = TRUE))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  
  # Increase cumulative sum span for precipitation
  plt = suppressWarnings(gr_plot_sep(sep, 1991, prec = TRUE, span = 10))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  
  # Add both
  plt = suppressWarnings(gr_plot_sep(sep, 1991, temp = TRUE, prec = TRUE))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  
})
