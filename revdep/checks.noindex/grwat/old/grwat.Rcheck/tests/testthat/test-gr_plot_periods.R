data(spas) 
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
vars = suppressWarnings(gr_summarize(sep))

test_that("Period plot has the correct content", {
  
  plt = suppressWarnings(gr_plot_periods(vars, Qygr, year = 1978))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  
  plt = suppressWarnings(gr_plot_periods(vars, Qygr, tests = TRUE))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  
  tests = gr_test_vars(vars)
  plt = suppressWarnings(gr_plot_periods(vars, Qspmax, Qygr, tests = tests))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  
  plt = suppressWarnings(gr_plot_periods(vars, Qygr, Qspmax, D10w1, Wsprngr,
                  layout = matrix(1:4, nrow = 2),
                  tests = tests))
  
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  
})










