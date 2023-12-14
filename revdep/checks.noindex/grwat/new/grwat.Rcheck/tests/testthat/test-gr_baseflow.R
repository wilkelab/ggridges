data(spas)

test_that("Boughton filtering works", {
  Qbase = gr_baseflow(spas$Q, method = 'boughton')
  expect_type(Qbase, "double")
  expect_true(length(Qbase) == length(spas$Q))
  expect_true(all(spas$Q - Qbase >= 0))
})

test_that("Chapman filtering works", {
  Qbase = gr_baseflow(spas$Q, method = 'chapman')
  expect_type(Qbase, "double")
  expect_true(length(Qbase) == length(spas$Q))
  expect_true(all(spas$Q - Qbase >= 0))
})

test_that("Jakeman filtering works", {
  Qbase = gr_baseflow(spas$Q, method = 'jakeman')
  expect_type(Qbase, "double")
  expect_true(length(Qbase) == length(spas$Q))
  expect_true(all(spas$Q - Qbase >= 0))
})

test_that("Lynehollick filtering works", {
  Qbase = gr_baseflow(spas$Q, method = 'lynehollick')
  expect_type(Qbase, "double")
  expect_true(length(Qbase) == length(spas$Q))
  expect_true(all(spas$Q - Qbase >= 0))
})

test_that("Maxwell filtering works", {
  Qbase = gr_baseflow(spas$Q, method = 'maxwell')
  expect_type(Qbase, "double")
  expect_true(length(Qbase) == length(spas$Q))
  expect_true(all(spas$Q - Qbase >= 0))
})

