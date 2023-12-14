testthat::context("utils")
set.seed(20181220)
n <- 30

testthat::test_that(".validate_levels works as expected", {
  data("iris", package = "datasets")
  for (char in c("-", ".", "~", "#", "@")) {
    iris$Test <- paste0(iris$Species, ifelse(iris$Sepal.Length > mean(iris$Sepal.Length), char, ""))
    iris$Test <- factor(iris$Test)
    testthat::expect_error(object = .validate_levels(data = iris, cols = "Test", char = char))
  }
})

testthat::test_that(".dropbig detects values above 'max'", {
  df <- data.frame(
    theta = rnorm(n),
    se = rnorm(n)
  )
  df$theta[1] <- rnorm(1, mean = 100)
  out <- .dropbig(data = df, estvarname = "theta", se = "se", methodvar = NULL, by = NULL, max = 5, semax = 10, robust = FALSE)
  testthat::expect_equal(object = is.na(out$theta[1]), expected = TRUE)
  out <- .dropbig(data = df, estvarname = "theta", se = "se", methodvar = NULL, by = NULL, max = 5, semax = 10, robust = TRUE)
  testthat::expect_equal(object = is.na(out$theta[1]), expected = TRUE)
})

testthat::test_that(".na_pair works as expected", {
  df <- data.frame(
    theta = rnorm(n),
    se = rnorm(n)
  )
  df$theta[1:10] <- NA
  df.processed <- .na_pair(data = df, estvarname = "theta", se = "se")
  testthat::expect_equal(object = sum(is.na(df.processed$se)), expected = sum(is.na(df$theta)))
})

testthat::test_that(".order actually orders a dataset", {
  data("iris", package = "datasets")
  iris2 <- iris[order(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width), ]
  messyIris <- iris[sample(x = seq(nrow(iris)), size = nrow(iris), replace = FALSE), ]
  fixedIris <- .order(data = messyIris, by = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
  testthat::expect_equivalent(object = fixedIris, expected = iris2)
})

testthat::test_that(".dropbig works if splitting by 'by' or 'methodvar'", {
  df <- data.frame(
    theta = rnorm(n),
    se = rnorm(n),
    group = rep(x = 1:3, each = n / 3)
  )
  df$theta[1] <- rnorm(1, mean = 1000)
  ### Check standardised values:
  # library(tidyverse)
  # df %>%
  # 	group_by(group) %>%
  # 	mutate(std = (theta - mean(theta)) / sqrt(var(theta)),
  # 				 std.r = (theta - median(theta)) / (fivenum(theta)[4] - fivenum(theta)[2]))
  out <- .dropbig(data = df, estvarname = "theta", se = "se", methodvar = NULL, by = "group", max = 2, semax = 10, robust = FALSE)
  testthat::expect_equal(object = is.na(out$theta[1]), expected = TRUE)
  out <- .dropbig(data = df, estvarname = "theta", se = "se", methodvar = NULL, by = "group", max = 10, semax = 10, robust = TRUE)
  testthat::expect_equal(object = is.na(out$theta[1]), expected = TRUE)
})
