path = system.file("extdata", "spas-zagorye.txt", 
                   package = "grwat")

hdata_raw = read.delim(path, header = FALSE, 
                       sep = ' ', na.strings = c('-999', '-999.0', '-'),
                       col.names = c('d', 'm', 'y', 'q'))

hdata = hdata_raw %>% 
  dplyr::transmute(Date = lubridate::make_date(y, m, d), 
                   Q = q)

test_that("Gaps are correctly filled", {
  
  # fill gaps
  fhdata = gr_fill_gaps(hdata, autocorr = 0.8)
  
  # check the results
  gaps = gr_get_gaps(fhdata)
  expect_equal(sum(gaps$type == 'gap'), 2)
  expect_equal(sum(gaps[gaps$type == 'gap', ]$duration), as.difftime(10, units = 'days'))
  
  # fill gaps
  fhdata = gr_fill_gaps(hdata, nobserv = 7)
  
  # check the results
  gaps = gr_get_gaps(fhdata)
  expect_equal(sum(gaps$type == 'gap'), 1)
  expect_equal(sum(gaps[gaps$type == 'gap', ]$duration), as.difftime(5, units = 'days'))
  
})