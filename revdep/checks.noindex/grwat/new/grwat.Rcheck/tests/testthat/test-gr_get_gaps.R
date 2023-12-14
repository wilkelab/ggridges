path = system.file("extdata", "spas-zagorye.txt", 
                   package = "grwat")

hdata_raw = read.delim(path, header = FALSE, 
                       sep = ' ', na.strings = c('-999', '-999.0', '-'),
                       col.names = c('d', 'm', 'y', 'q'))

hdata = hdata_raw %>% 
  dplyr::transmute(Date = lubridate::make_date(y, m, d), 
                   Q = q)

test_that("Gaps are correctly identified", {
  
  gaps = gr_get_gaps(hdata)
  
  expect_equal(sum(gaps$type == 'gap'), 4)
  expect_equal(sum(gaps[gaps$type == 'gap', ]$duration), as.difftime(15, units = 'days'))
  
})