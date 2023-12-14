test_that("failure for non-numeric values", {
  expect_error(get.step.sequence(datmin = "min",
                                 datmax = 2,
                                 stepsize = 1),
               "numeric")
  expect_error(get.step.sequence(datmin = 1,
                                 datmax = "max",
                                 stepsize = 1),
               "numeric")
  expect_error(get.step.sequence(datmin = 1,
                                 datmax = 2,
                                 stepsize = "size"),
               "numeric")
})


test_that("first and last values in sequence are correct", {
  min <- -100
  max <- -50
  test <- get.step.sequence(datmin = min,
                            datmax = max,
                            stepsize = 25)
  expect_equal(test[1], min)
  expect_equal(test[length(test)], max)
})

test_that("stepsize 1 builds simple sequence", {
  min <- 1
  max <- 11
  stepsize <- 1
  seq <- get.step.sequence(datmin = min,
                           datmax = max,
                           stepsize = stepsize)
  expect_equal(length(seq), length(seq(min, max, stepsize)))
})

test_that("timespan not divisible by stepsize,
          timespan exceeds 60% of stepsize,
          schould return min, max and mean", {
  min <- 1
  max <- 21
  stepsize <- 30
  seq <- get.step.sequence(datmin = min,
                           datmax = max,
                           stepsize = stepsize)
  expect_equal(length(seq), 3)
  expect_equal(seq[1], min)
  expect_equal(seq[2], mean(c(min, max)))
  expect_equal(seq[3], max)
})


test_that("timespan not divisible by stepsize,
          dated to only one year", {
            min <- 1
            max <- 1
            stepsize <- 30
            seq <- get.step.sequence(datmin = min,
                                     datmax = max,
                                     stepsize = stepsize)
            expect_equal(seq, min)
            expect_equal(seq, max)
            expect_equal(length(seq), 1)
})

test_that("timespan not divisible by stepsize,
          timespan less then 60% of stepsize,
          should return only min and max date", {
            min <- 1
            max <- 10
            stepsize <- 30
            seq <- get.step.sequence(datmin = min,
                                     datmax = max,
                                     stepsize = stepsize)
            expect_equal(seq[1], min)
            expect_equal(seq[2], max)
            expect_equal(length(seq), 2)
})

test_that("timespan divisible by stepsize,
          no residuals", {
            min <- 1
            max <- 11
            stepsize <- 2
            seq <- get.step.sequence(datmin = min,
                                     datmax = max,
                                     stepsize = stepsize)
            expect_equal(seq, seq(from = min, to = max, by = 2))
})

test_that("timespan divisible by stepsize,
          with residuals smaller than half the stepsize", {
            min <- 1
            max <- 17
            stepsize <- 3
            check <- seq(from = min, to = max, by = stepsize)
            seq <- get.step.sequence(datmin = min,
                                     datmax = max,
                                     stepsize = stepsize)
            expect_equal(length(seq), length(check))
            expect_failure(expect_equal(seq, check))
})


test_that("timespan divisible by stepsize,
          with residuals larger than half the stepsize,
          modifies stepsize", {
            min <- 1
            max <- 49
            stepsize <- 25
            seq <- get.step.sequence(datmin = min,
                                     datmax = max,
                                     stepsize = stepsize)
            expect_lt(diff(seq)[1], stepsize)
})

