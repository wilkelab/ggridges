context("Epidemiological inference functions")

test_that("epidemiological summary is output if requested and appropriate", {
  mtcars.test <- mtcars
  mtcars.test$am <- factor(mtcars$am)
  mtcars.test$cyl <- factor(mtcars$cyl)

  summary.out <- capture.output(
    getPlotSummary(am, cyl, data = mtcars.test, summary.type = "inference", inference.type = "conf", epi.out = TRUE)
  )

  expect_match(summary.out, "### Odds Ratio estimates for am = 1", all = FALSE)
  expect_match(summary.out, "### Risk Ratio estimates for am = 1", all = FALSE)
  expect_match(summary.out, "### Risk Difference estimates for am = 1", all = FALSE)
})

## All `test.mat` are based on examples from the following
## Boston University School of Public Health CI online course:
## https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_Confidence_Intervals/index.html

test_that("odds ratios are calculated correctly", {
  test.mat <- matrix(c(57, 10, 6, 7), ncol = 2)
  test.or <- calculate_or(test.mat)

  expect_equivalent(round(test.or["estimate"], 2), 6.65)
  expect_equivalent(round(test.or["ci.lwr"], 2), 1.85)
  expect_equivalent(round(test.or["ci.upr"], 2), 23.94)

  test.mat <- matrix(c(10, 10, 0, 2), ncol = 2)
  test.or2 <- calculate_or(test.mat)
  expect_equivalent(test.or2, rep(NA, 4))
})

test_that("risk ratios are calculated correctly", {
  test.mat <- matrix(c(29, 41, 20, 9), ncol = 2)
  test.rr <- calculate_rr(test.mat)

  expect_equivalent(round(test.rr["estimate"], 2), 0.44)
  expect_equivalent(round(test.rr["ci.lwr"], 2), 0.22)
  expect_equivalent(round(test.rr["ci.upr"], 2), 0.87)

  test.mat <- matrix(c(10, 10, 0, 2), ncol = 2)
  test.rr2 <- calculate_rr(test.mat)
  expect_equivalent(test.rr2, rep(NA, 4))
})

test_that("risk differences are calculated correctly", {
  test.mat <- matrix(c(2757, 663, 298, 81), ncol = 2)
  test.rd <- calculate_rd(test.mat)

  expect_equivalent(round(test.rd["estimate"], 4), 0.0113)
  expect_equivalent(round(test.rd["ci.lwr"], 4), -0.0134)
  expect_equivalent(round(test.rd["ci.upr"], 4), 0.0361)
})

test_that("all 2x2 table functions throw appropriate errors", {
  test.mat <- matrix(sample(1:100, size = 3 * 2), ncol = 2, nrow = 3)

  expect_error(calculate_or(test.mat), "2x2")
  expect_error(calculate_rr(test.mat), "2x2")
  expect_error(calculate_rd(test.mat), "2x2")
})

test_that("epidemiological summary tables are formatted correctly", {
  test.mat <- matrix(
    c(
      0.28125000,  0.03804859,  2.07896160,  0.33220211,
      0.062500000, 0.008455243, 0.461991467, 0.005138646,
      NA,          NA,          NA,          NA
    ),
    nrow = 4,
    dimnames = list(c("estimate", "ci.lwr", "ci.upr", "p"))
  )

  formatted.tab <- epi.format(test.mat, label = "OR", names = letters[1:4], 1)

  expect_match(trimws(formatted.tab[1]), "^OR")
  expect_match(trimws(formatted.tab[2]), "^a")

  ## a row should have [name] [estimate] [CI] [p-value]
  expect_match(
    trimws(formatted.tab[3]),
    "^[A-z] +[0-9]+\\.[0-9]+ +\\(.+,.+\\) +[0-1]\\.[0-9]+$"
  )

  expect_match(formatted.tab[5], "OR cannot be estimated")

  ## baseline should have the value of first.val argument
  expect_match(trimws(formatted.tab[2]), "1\\.00")
})

test_that("Mantel-Haenszel tests work when requested", {
  set.seed(2020-09-08)
  test.df <- data.frame(
    x1 <- sample(c("a", "b"), size = 1000, replace = TRUE),
    x2 <- sample(c("o", "p"), size = 1000, replace = TRUE),
    g <- sample(c("x", "y", "z"), size = 1000, replace = TRUE)
  )

  summary.out <- capture.output(
    getPlotSummary(x1, x2, g1 = g, data = test.df, summary.type = "inference", inference.type = "conf", epi.out = TRUE)
  )

  expect_match(summary.out, "Mantel-Haenszel", all = FALSE)

  summary.out2 <- capture.output(
    getPlotSummary(x1, g, g1 = x2, data = test.df, summary.type = "inference", inference.type = "conf", epi.out = TRUE)
  )

  expect_match(summary.out2, "Cochran-Mantel-Haenszel", all = FALSE)


})
