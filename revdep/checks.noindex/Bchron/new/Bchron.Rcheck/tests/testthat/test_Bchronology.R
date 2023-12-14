library(Bchron)
co <- function(expr) capture.output(expr, file = "NUL")

data(Glendalough)

set.seed(100)

co(GlenOut <- with(
  Glendalough,
  Bchronology(
    ages = ages,
    ageSds = ageSds,
    calCurves = calCurves,
    positions = position,
    positionThicknesses = thickness,
    ids = id,
    predictPositions = seq(-10, 1500, by = 10),
    iterations = 100,
    burn = 20,
    thin = 1
  )
))

test_that("Data sets", {
  expect_output(print(Glendalough))
})

test_that("Bchronology", {
  expect_s3_class(GlenOut, "BchronologyRun")
})

test_that("summary.BchronologyRun", {
  expect_output(summary(GlenOut, type = "quantiles"))
  expect_output(summary(GlenOut, type = "convergence"))
  expect_output(summary(GlenOut, type = "outliers"))
  expect_output(summary(GlenOut, type = "max_var"))
})

test_that("plot.BchronologyRun", {
  p <- plot(GlenOut)
  expect_s3_class(p, "ggplot")
})

test_that("predict.BchronologyRun", {
  co(predictAges1 <- predict(GlenOut,
    newPositions = c(150, 725, 1500),
    newPositionThicknesses = c(5, 0, 20)
  ))
  co(predictAges2 <- predict(GlenOut,
    newPositions = seq(0, 1500, by = 10)
  ))
  expect_type(predictAges1, "double")
  expect_type(predictAges2, "double")
  expect_false(any(is.na(predictAges1)))
  expect_false(any(is.na(predictAges2)))
})

test_that("sedimentation and accumulation rates", {
  co(acc_rate <- summary(GlenOut,
    type = "acc_rate",
    probs = c(0.25, 0.5, 0.75)
  ))
  co(sed_rate <- summary(GlenOut,
    type = "sed_rate", useExisting = FALSE,
    probs = c(0.25, 0.5, 0.75)
  ))
  expect_type(acc_rate, "list")
  expect_type(sed_rate, "list")
})

test_that("choosePositions", {
  # Check choosing new positions
  co(newPositions <- choosePositions(GlenOut, N = 3))
  co(newPositions2 <- choosePositions(GlenOut,
    N = 2,
    positions = seq(500, 700, by = 10)
  ))
  expect_type(newPositions, "double")
  expect_type(newPositions2, "double")
  expect_false(any(is.na(newPositions)))
  expect_false(any(is.na(newPositions2)))
})

test_that("Bchronology prediction bug", {
  # New test due to weird bug in Bchronology prediction - 13/4/20
  set.seed(123)
  df <- structure(list(
    age = c(2975, 4270, 4480),
    error = c(60, 70, 60),
    depth = c(72.5, 117.5, 132.5),
    calCurves = c("intcal13", "intcal13", "intcal13"),
    thickness = c(5, 5, 5)
  ),
  row.names = c(NA, -3L),
  class = c("tbl_df", "tbl", "data.frame")
  )
  co(test_chron <- with(
    df,
    Bchronology(
      ages = age,
      ageSds = error,
      calCurves = calCurves,
      positions = depth,
      positionThicknesses = thickness,
      iterations = 100,
      burn = 20,
      thin = 1
    )
  ))
  co(summ <- summary(test_chron, type = "quantiles"))
  expect_true(all(summ < 6000))
})

test_that("Test with starting values", {
  co(GlenOut <- with(
    Glendalough,
    Bchronology(
      ages = ages,
      ageSds = ageSds,
      calCurves = calCurves,
      positions = position,
      positionThicknesses = thickness,
      ids = id,
      predictPositions = seq(-10, 1500, by = 10),
      thetaStart = ages,
      iterations = 100,
      burn = 20,
      thin = 1
    )
  ))
  expect_s3_class(GlenOut, "BchronologyRun")
})

test_that("Non-unique IDs fail", {
  expect_error(with(
    Glendalough,
    Bchronology(
      ages = ages,
      ageSds = ageSds,
      calCurves = calCurves,
      positions = position,
      positionThicknesses = thickness,
      ids = rep("a", nrow(Glendalough)),
      predictPositions = seq(-10, 1500, by = 10),
      thetaStart = ages,
      iterations = 100,
      burn = 20,
      thin = 1
    )
  ))
})
