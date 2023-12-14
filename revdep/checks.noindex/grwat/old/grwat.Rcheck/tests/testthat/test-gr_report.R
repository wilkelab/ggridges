skip_on_cran()

data(spas) 
df = spas[spas$Date < as.Date('1960-01-01'), ]
sep = gr_separate(df, params = gr_get_params(reg = 'center'))
vars = suppressWarnings(gr_summarize(sep))

test_that("Report is correctly generated", {
  skip_on_os("windows")
  expect_message(suppressWarnings(gr_report(sep, vars, output = '~/Spas-Zagorye.html', locale = 'RU')))
})
  