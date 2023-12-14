skip_if_not_installed('ggHoriPlot')
skip_if_not_installed('ggthemes')

test_that("Horizon plot has the correct content", {
  
  data(spas)
  sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
  plt = suppressWarnings(gr_plot_hori(sep, years = 1960:1980))
  
  expect_s3_class(plt, 'ggplot')
  expect_type(plt, 'list')
  expect_equal(plt$layers[[1]]$geom$default_aes$size, 0.5)
  
})

