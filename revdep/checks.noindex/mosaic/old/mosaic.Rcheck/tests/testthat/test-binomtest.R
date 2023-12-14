
# context("binom.test()")

  TestData <- data.frame( a = factor(rep(letters[1:3], length.out = 100)),
                          b = rep(letters[1:3], length.out = 100), 
                          c = rep(c(TRUE, FALSE, FALSE), length.out = 100), 
                          d = rep(c(TRUE, FALSE, FALSE), length.out = 100), 
                          stringsAsFactors = FALSE
  )
  
test_that("formulas work", {
 
  X <- stats::binom.test(34, 100)
  A <- binom.test(~ a, data=TestData)
  B <- binom.test(~ b, data=TestData)
  C <- binom.test(~ c, data=TestData)
  
  expect_equal(ignore_attr = TRUE, confint(A), confint(X))
  expect_match(A$data.name, "TestData\\$a")
  
  expect_equal(ignore_attr = TRUE, confint(B), confint(X))
  expect_match(B$data.name, "TestData\\$b")
  
  expect_equal(ignore_attr = TRUE, confint(C), confint(X))
  expect_match(C$data.name, "TestData\\$c")
  
})

test_that("formula + unnamed second arg data frame throws an error", {
  expect_error(binom.test(~ a, TestData), "did you forget")
})

# No longer supporting this.
#
# test_that("formulas work with unnamed second arg", {
#  
#   X <- stats::binom.test(34, 100)
#   A <- binom.test(~ a, TestData)
#   B <- binom.test(~ b, TestData)
#   C <- binom.test(~ c, TestData)
#   
#   expect_equal(ignore_attr = TRUE, confint(A), confint(X))
#   expect_match(A$data.name, "TestData\\$a")
#   
#   expect_equal(ignore_attr = TRUE, confint(B), confint(X))
#   expect_match(B$data.name, "TestData\\$b")
#   
#   expect_equal(ignore_attr = TRUE, confint(C), confint(X))
#   expect_match(C$data.name, "TestData\\$c")
# })
  
test_that("success = works", {
  
  X <- stats::binom.test(33, 100)
  Y <- stats::binom.test(66, 100)
  A <- binom.test(~ a, data=TestData, success = "b")
  B <- binom.test(~ b, data=TestData, success = "b")
  C <- binom.test(~ c, data=TestData, success = FALSE)
  
  expect_equal(ignore_attr = TRUE, confint(A), confint(X))
  expect_match(A$data.name, "TestData\\$a")
  expect_match(A$data.name, "success = b")
  
  expect_equal(ignore_attr = TRUE, confint(B), confint(X))
  expect_match(B$data.name, "TestData\\$b")
  expect_match(B$data.name, "success = b")
  
  expect_equal(ignore_attr = TRUE, confint(C), confint(Y))
  expect_match(C$data.name, "TestData\\$c")
  expect_match(C$data.name, "success = FALSE")
})

test_that("bare vars throw error", {
  expect_error(binom.test(a, data = TestData), "first argument should be a formula")
  expect_error(binom.test(b, data = TestData), "first argument should be a formula")
  expect_error(binom.test(d, data = TestData), "first argument should be a formula")
})


# test_that("bare vars work", {
#   X <- stats::binom.test(34, 100)
#   A <- binom.test( a, data=TestData)
#   B <- binom.test( b, data=TestData)
#   C <- binom.test( c, data=TestData)
#   
#   expect_equal(ignore_attr = TRUE, confint(A), confint(X))
#   expect_match(A$data.name, "a")
#   expect_match(A$data.name, "success = a")
#   
#   expect_equal(ignore_attr = TRUE, confint(B), confint(X))
#   expect_match(B$data.name, "b")
#   expect_match(B$data.name, "success = a")
#   
#   expect_equal(ignore_attr = TRUE, confint(C), confint(X))
#   expect_match(C$data.name, "c")
#   expect_match(C$data.name, "success = TRUE")
# })

test_that("numbers work", {
  expect_equal(ignore_attr = TRUE,  
    confint(stats::binom.test(33, 100)),
    confint(binom.test(33, 100))
  )
  
})
  
  test_that("x treated as raw data when n is missing", {
    X <- resample(1:3, 100)
    x <- sum(X == min(X))
    expect_equal(ignore_attr = TRUE,   
      confint(binom.test(X)), 
      confint(binom.test(x, 100)) )
  })  
  
test_that("CI methods correct", {

  # Clopper-Pearson, the default but with 3 names
  
  expect_equal(ignore_attr = TRUE, 
    confint(stats::binom.test(26,200)),
    confint(binom.test(26,200)))
  
  expect_equal(ignore_attr = TRUE, 
    confint(stats::binom.test(26,200)),
    confint(binom.test(26,200, ci.method="clopper-pearson")))
  
  expect_equal(ignore_attr = TRUE, 
    confint(binom.test(26,200, ci.method="clopper-pearson")),
    confint(binom.test(26,200, ci.method="binom.test")))
 
  # Score/Wilson/prop.test  
  expect_equal(ignore_attr = TRUE, 
    confint(stats::prop.test(26,200)),
    confint(binom.test(26,200, ci.method="prop.test", correct = TRUE)))
  
  expect_equal(ignore_attr = TRUE, 
    confint(stats::prop.test(26,200, correct = FALSE)),
    confint(binom.test(26,200, ci.method = "wilson")))
  
  expect_equal(ignore_attr = TRUE, 
    confint(binom.test(26,200, ci.method = "score")),
    confint(binom.test(26,200, ci.method = "wilson")))
  
  # Clopper vs external reference 
  # NIST example from http://www.itl.nist.gov/div898/handbook/prc/section2/prc241.htm
  ci <- confint(binom.test(4, 20, ci.method="clopper", conf.level=.9))
  expect_equal(ci[1,2], 0.071354, tolerance = 1e-5)
  expect_equal(ci[1,3], 0.401029, tolerance = 1e-5)

  # Wald vs external reference 
  # from http://www.stat.wmich.edu/s160/book/node47.html 
  ci <- confint(binom.test(15, 59, ci.method="Wald"))
  expect_equal(ci[1,2], 0.143, tolerance = 1e-3)
  expect_equal(ci[1,3], 0.365, tolerance = 1e-3)
 
  # Plus4 
  ci <- confint(binom.test(0, 100, ci.method="Plus4"))
  expect_equal(ci[1,2], 0.0,    tolerance = 1e-3)
  expect_equal(ci[1,3], 0.0456, tolerance = 1e-3)
  
})

test_that("binom.test compatibile with dplyr", {
  expect_equal(ignore_attr = TRUE, 
    data.frame(x = rep(c('a', 'b'), c(5, 10))) |>
      summarise(pval = pval(binom.test( ~ x))),
    data.frame(x = rep(c('a', 'b'), c(5, 10))) |>
      summarise(pval = pval(stats::binom.test(5, 15)))
  )
})
