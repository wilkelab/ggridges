data(spas)
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
vars = suppressWarnings(gr_summarize(sep))

test_that("Minimum month plot has the correct content", {
  
  plt = gr_plot_minmonth(vars, tests = gr_test_vars(vars))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  expect_s3_class(plt[[2]], 'ggplot')
  expect_equal(plt[[1]][["plot_env"]][["periodtitle1_summer"]], "before 2000")
  
  plt = gr_plot_minmonth(vars, year = 1978)
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  expect_s3_class(plt[[2]], 'ggplot')
  expect_equal(plt[[1]][["plot_env"]][["periodtitle1_summer"]], "before 1978")
})

