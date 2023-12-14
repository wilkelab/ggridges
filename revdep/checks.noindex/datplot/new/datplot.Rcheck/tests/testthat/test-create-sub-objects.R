test <- c(1, 50, 70, 0, NA)
names(test) <- c("index", "datmin", "datmax", "calc", "step")
test <- list(test)

test_that("returns a matrix", {
  expect_true(inherits(create.sub.objects(test, stepsize = 10), "matrix"))
})

test_that("creates appropriate amount of sub-objects", {
  expect_equal(nrow(create.sub.objects(test, stepsize = 10)), 3)
})

test_that("warning (or not) when timespan is larger than stepsize", {
  expect_warning(create.sub.objects(test, stepsize = 50), "larger")
  expect_failure(expect_warning(create.sub.objects(test, stepsize = 2)))
})


test_that("attaches correct attribute", {
  check <- create.sub.objects(test, stepsize = 10, calc = "weight")
  expect_match(attributes(check)$calc, "weight")
  check <- create.sub.objects(test, stepsize = 10, calc = "probability")
  expect_match(attributes(check)$calc, "probability")
})


test_that("adds cumulative probability", {
  test <- c(1, 50, 51, 0.5, NA)
  names(test) <- c("index", "datmin", "datmax", "probability", "step")
  test <- list(test)
  check <- create.sub.objects(test, stepsize = 1,
                              calc = "probability",
                              cumulative = TRUE)
  expect_true("cumul_prob" %in% colnames(check))
})

test_that("cumulative probability adds correctly", {
  test <- c(1, 50, 61, 0.1, NA)
  names(test) <- c("index", "datmin", "datmax", "probability", "step")
  test <- list(test)
  check <- create.sub.objects(test, stepsize = 1,
                              calc = "probability",
                              cumulative = TRUE)
  expect_equal(check[1,"cumul_prob"] * nrow(check),
               check[nrow(check),"cumul_prob"])
})
