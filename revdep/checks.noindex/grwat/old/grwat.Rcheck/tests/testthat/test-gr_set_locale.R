data(spas)
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
vars = suppressWarnings(gr_summarize(sep))
tests = suppressWarnings(gr_test_vars(vars))

test_that('Unknown locales are handled', {
  
  expect_warning(gr_set_locale('DE'))

  })

test_that('English locale works', {
  
  expect_silent(gr_set_locale('EN'))
  suppressWarnings(gr_plot_sep(sep, 1978))
  suppressWarnings(gr_plot_vars(vars, Qy, tests = tests))
  suppressWarnings(gr_plot_periods(vars, Qy, tests = tests))
  suppressWarnings(gr_plot_tests(tests))
  suppressWarnings(gr_plot_minmonth(vars, tests = tests))
  suppressWarnings(gr_plot_ridge(sep, years = c(1960, 1965, 1989, 2001, 2012)))
  suppressWarnings(gr_plot_matrix(sep, type = 'runoff'))
  suppressWarnings(gr_plot_hori(sep, years = 1960:1980))
  
})

