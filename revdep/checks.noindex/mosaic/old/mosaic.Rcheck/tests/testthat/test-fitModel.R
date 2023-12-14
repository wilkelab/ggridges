
# context('fitModel()')


test_that("A function is created", {
  expect_true(is.function(fitModel( width ~ A * length + B, data=mosaicData::KidsFeet)))
  expect_equal(ignore_attr = TRUE, names(formals(fitModel( width ~ A * length + B, data=mosaicData::KidsFeet))), 
                    c('length','...','transformation'))
})

test_that("Function gives correct results", {
  formula <- width ~ A * length + B
  f <- fitModel( formula, data=mosaicData::KidsFeet )
  model <- nls(formula, data=mosaicData::KidsFeet, start=list(A=1, B=1)) 
  expect_equal(ignore_attr = TRUE,  f(mosaicData::KidsFeet$length), fitted(model) )
})

