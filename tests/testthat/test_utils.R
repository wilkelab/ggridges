
context('utils')

test_that('reduce works', {
  expect_equal(reduce(1:10, function(x, y) x + y), sum(1:10))
  expect_equal(reduce(1:20, .init=1, function(x, y) x + y), sum(1:20)+1)
  expect_equal(reduce(as.list(1:20), function(x, y) x + y), sum(1:20))
  expect_equal(reduce(c(T, F, T), function(x, y) x & y), FALSE)
  expect_equal(reduce(c('the', 'fish', 'was', 'delish'), paste), 'the fish was delish')

  expect_equal(reduce(list(), .init=0), 0)
  expect_error(reduce(list()), "`.x` is empty, and no `.init` supplied")
})
