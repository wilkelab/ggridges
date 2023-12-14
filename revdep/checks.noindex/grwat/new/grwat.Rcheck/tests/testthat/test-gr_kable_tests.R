data(spas)

sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
vars = suppressWarnings(gr_summarize(sep))
tests = gr_test_vars(vars)

test_that("Tests table has the knitr class", {
  
  kbl = gr_kable_tests(tests)
  
  expect_s3_class(kbl, 'knitr_kable')
  expect_s3_class(kbl, 'kableExtra')
  
})
