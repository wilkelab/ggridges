set.seed(123)
library(Bchron)
co <- function(expr) capture.output(expr, file = "NUL")

ages1 <- BchronCalibrate(
  ages = 11553,
  ageSds = 230,
  calCurves = "intcal20",
  ids = "Ox-123456"
)
ages2 <- BchronCalibrate(
  ages = c(3445, 11553, 7456),
  ageSds = c(50, 230, 110),
  calCurves = c("intcal20", "intcal20", "shcal20")
)
ages3 <- BchronCalibrate(
  ages = c(1000, 11553),
  ageSds = c(50, 230),
  positions = c(100, 150),
  calCurves = c("intcal20", "normal")
)

# A dodgy one that was found by a GitHub user shheidgen
ages4 <- BchronCalibrate(
  ages = 6885,
  ageSds = 65,
  calCurve = "intcal13"
)

# A test of the calibration curve version
ages5 <- BchronCalibrate(
  ages = c(3445, 11553, 7456),
  ageSds = c(50, 230, 110),
  calCurves = c("intcal20", "intcal20", "intcal20")
)

test_that("BchronCalibrate works", {
  expect_s3_class(ages1, "BchronCalibratedDates")
  expect_s3_class(ages2, "BchronCalibratedDates")
  expect_s3_class(ages3, "BchronCalibratedDates")
})

test_that("Out of range dates produce an error unless turned off", {
  expect_error(BchronCalibrate(
    ages = 85,
    ageSds = 30,
    calCurve = "intcal20"
  ))
  expect_s3_class(
    BchronCalibrate(
      ages = 85,
      ageSds = 30,
      calCurve = "intcal20",
      allowOutside = TRUE
    ), "BchronCalibratedDates"
  )
})

test_that("Ages converted to integers gives a message", {
  expect_message(BchronCalibrate(
    ages = 1500.3,
    ageSds = 30,
    calCurve = "intcal20"
  ))
  expect_message(BchronCalibrate(
    ages = 1500,
    ageSds = 30.3,
    calCurve = "intcal20"
  ))
})

test_that("summary.BchronologyRun works", {
  expect_output(summary(ages1))
  expect_output(summary(ages2))
  expect_output(summary(ages3))
})

test_that("plot.BchronCalibrate works", {
  p <- plot(ages1)
  expect_s3_class(p, "ggplot")
  expect_type(plot(ages2), "list")
  p <- plot(ages2, date = 1)
  expect_s3_class(p, "ggplot")
  p <- plot(ages2, date = "Date1")
  expect_s3_class(p, "ggplot")
  expect_type(plot(ages2, date = c("Date1", "Date2")), "list")
  expect_error(plot(ages2, date = "5"))
  expect_error(plot(ages2, date = 7))
  p <- plot(ages3)
  expect_s3_class(p, "ggplot")
  p <- plot(ages4)
  expect_s3_class(p, "ggplot")
  p <- plot(ages1, includeCal = TRUE)
  expect_s3_class(p, "ggplot")
  expect_error(plot(ages3, includeCal = TRUE))
  p <- plot(ages5, includeCal = TRUE)
  expect_s3_class(p, "ggplot")
  expect_error(plot(ages2, includeCal = TRUE))
})
