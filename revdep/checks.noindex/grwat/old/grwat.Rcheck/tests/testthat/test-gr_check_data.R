data(spas)

test_that("Correct data is welcomed", {
  
  expect_message(gr_check_data(spas))

})

test_that("Incorrect column number is handled", {
  
  expect_error(gr_check_data(spas[-1]))

})


test_that("Duplicated dates are handled", {
  
  expect_error(gr_check_data(rbind(spas[1:10, ], spas[1:10, ])))
  
})


test_that("Incorrect column types are handled", {
  
  spas2 = data.frame(d = 1:3, m = 1:3, y = 2020:2022, q = 5:7)
    
  expect_error(gr_check_data(spas2))
  
  spas2 = spas
  spas2$Date = as.character(spas2$Date)
  expect_error(gr_check_data(spas2))
  
  spas2 = spas
  spas2$Q = as.character(spas2$Q)
  expect_error(gr_check_data(spas2))
  
  spas2 = spas
  spas2$Temp = as.character(spas2$Temp)
  expect_error(gr_check_data(spas2))
  
  spas2 = spas
  spas2$Prec = as.character(spas2$Prec)
  expect_error(gr_check_data(spas2))
  
})

test_that("Negative runoff and precipitation values are handled", {
  
  spas2 = spas
  spas2$Q = -spas2$Q
  expect_error(gr_check_data(spas2))
  
  spas2 = spas
  spas2$Prec = -spas2$Prec
  expect_error(gr_check_data(spas2))
  
})


