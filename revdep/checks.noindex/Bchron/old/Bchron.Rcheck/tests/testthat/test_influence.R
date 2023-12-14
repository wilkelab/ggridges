set.seed(123)
library(Bchron)
co <- function(expr) capture.output(expr, file = "NUL")

test_that("dateInfluence", {
  data(Glendalough)
  co(GlenOut <- with(
    Glendalough,
    Bchronology(
      ages = ages,
      ageSds = ageSds,
      calCurves = calCurves,
      positions = position,
      positionThicknesses = thickness,
      ids = id,
      predictPositions = seq(0, 1500, by = 100),
      iterations = 100,
      burn = 20,
      thin = 1
    )
  ))

  # Remove two dates
  co(GlenOut_m2 <- Bchronology(
    ages = Glendalough$ages[-c(3:4)],
    ageSds = Glendalough$ageSds[-c(3:4)],
    calCurves = Glendalough$calCurves[-c(3:4)],
    positions = Glendalough$position[-c(3:4)],
    positionThicknesses = Glendalough$thickness[-c(3:4)],
    ids = Glendalough$id[-c(3:4)],
    predictPositions = seq(0, 1500, by = 100),
    iterations = 100,
    burn = 20,
    thin = 1
  ))

  co(di1 <- dateInfluence(GlenOut,
    whichDate = "Beta-100901",
    measure = "absMedianDiff"
  ))
  expect_type(
    di1,
    "list"
  )
  co(di2 <- dateInfluence(GlenOut,
    whichDate = "Beta-100901",
    measure = "KL"
  ))
  expect_type(
    di2,
    "list"
  )
  co(di3 <- dateInfluence(GlenOut,
    whichDate = "Beta-100901",
    measure = "absMeanDiff"
  ))
  expect_type(
    di3,
    "list"
  )
  co(di4 <- dateInfluence(GlenOut,
    whichDate = 4, measure = "absMeanDiff"
  ))
  expect_type(
    di4,
    "list"
  )
  co(di5 <- dateInfluence(GlenOut,
    whichDate = "all", measure = "KL"
  ))
  expect_type(
    di5,
    "list"
  )
  co(di6 <- dateInfluence(GlenOut,
    whichDate = "internal", measure = "KL"
  ))
  expect_type(
    di6,
    "list"
  )
  expect_output(coreInfluence(GlenOut_m2,
    GlenOut,
    type = c("max", "plot"),
    xlab = "Age (cal years BP)",
    ylab = "Depth (cm)",
    main = "Chronology difference at 95% for
              Glendalough removing two dates",
    las = 1
  ))
})
