
# context("prop.test()")

TestData <- data.frame( a = factor(rep(letters[1:3], length.out=100)),
                        b = rep(letters[1:3], length.out=100), 
                        c = rep(c(TRUE, FALSE, FALSE), length.out=100), 
                        d = rep(c(TRUE, FALSE, FALSE), length.out=100), 
                        stringsAsFactors = FALSE
)

test_that("formulas work", {
  
  X <- stats::prop.test(34, 100)
  A <- prop.test(~ a, data=TestData)
  B <- prop.test(~ b, data=TestData)
  C <- prop.test(~ c, data=TestData)
  
  expect_equal(ignore_attr = TRUE, confint(A), confint(X))
  expect_match(A$data.name, "TestData\\$a")
  
  expect_equal(ignore_attr = TRUE, confint(B), confint(X))
  expect_match(B$data.name, "TestData\\$b")
  
  expect_equal(ignore_attr = TRUE, confint(C), confint(X))
  expect_match(C$data.name, "TestData\\$c")
  
})

test_that("formulas + unnamed second arg throws error", {
   expect_error( prop.test(~ a, TestData), "did you forget")
})

# test_that("formulas work with unnamed second arg", {
#   
#   X <- stats::prop.test(34, 100)
#   A <- prop.test(~ a, TestData)
#   B <- prop.test(~ b, TestData)
#   C <- prop.test(~ c, TestData)
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
  
  X <- stats::prop.test(33, 100)
  Y <- stats::prop.test(66, 100)
  A <- prop.test(~ a, data=TestData, success = "b")
  B <- prop.test(~ b, data=TestData, success = "b")
  C <- prop.test(~ c, data=TestData, success = FALSE)
  
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
  expect_error(prop.test(a, data = TestData), "first argument should be a formula")
  expect_error(prop.test(b, data = TestData), "first argument should be a formula")
  expect_error(prop.test(d, data = TestData), "first argument should be a formula")
})

# test_that("bare vars work", {
#   X <- stats::prop.test(34, 100)
#   A <- prop.test( a, data=TestData)
#   B <- prop.test( b, data=TestData)
#   C <- prop.test( c, data=TestData)
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
    confint(stats::prop.test(33, 100)),
    confint(prop.test(33, 100))
  )
  
})

test_that("x treated as raw data when n is missing", {
  X <- resample(1:3, 100)
  x <- sum(X == min(X))
  expect_equal(ignore_attr = TRUE,   
    confint(prop.test(X)), 
    confint(prop.test(x, 100)) )
})  


test_that("tests for multiple proportions", {
  smokers  <- c( 83, 90, 129, 70 )
  patients <- c( 86, 93, 136, 82 )
  expect_equal(ignore_attr = TRUE,  
    stats::prop.test(smokers, patients),
    prop.test(smokers, patients)
  )
})
  
test_that("x treated as raw data", {
  X <- resample(1:3, 100)
  x <- sum(X == min(X))
  expect_equal(ignore_attr = TRUE,   
    confint(prop.test(X)), 
    confint(prop.test(x, 100)) )
})
