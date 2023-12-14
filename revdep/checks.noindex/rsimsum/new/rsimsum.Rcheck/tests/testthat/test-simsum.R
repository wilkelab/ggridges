testthat::context("simsum")

testthat::test_that("simsum prints ok", {
  data("MIsim", package = "rsimsum")
  data("relhaz", package = "rsimsum")
  testthat::expect_output(print(rsimsum::simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")))
  testthat::expect_output(print(rsimsum::simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se")))
  testthat::expect_output(print(rsimsum::simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", control = list(mcse = FALSE))))
  testthat::expect_output(print(rsimsum::simsum(data = MIsim, estvarname = "b", se = "se")))
  testthat::expect_output(print(rsimsum::simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = c("n", "baseline"))))
  testthat::expect_output(print(rsimsum::simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", by = c("n", "baseline"))))
  testthat::expect_output(print(rsimsum::simsum(data = relhaz, estvarname = "theta", se = "se")))
})

testthat::test_that("simsum returns an object of class simsum", {
  data("MIsim", package = "rsimsum")
  s <- rsimsum::simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC")
  testthat::expect_s3_class(s, "simsum")
})

testthat::test_that("summ slot of a simsum object is a data.frame", {
  data("MIsim", package = "rsimsum")
  s <- rsimsum::simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC")
  testthat::expect_s3_class(s$summ, "data.frame")
})

testthat::test_that("not passing estvarname throws an error", {
  testthat::expect_error(
    {
      data("MIsim", package = "rsimsum")
      s <- rsimsum::simsum(data = MIsim, true = 0.5, se = "se", methodvar = "method", ref = "CC")
    },
    'argument "estvarname" is missing, with no default'
  )
})

testthat::test_that("specifying ref and not methodvar throws a warning", {
  testthat::expect_warning(
    {
      data("MIsim", package = "rsimsum")
      s <- rsimsum::simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", ref = "CC")
    },
    "'ref' method is specified while 'methodvar' is not: 'ref' will be ignored"
  )
})

testthat::test_that("specifying methodvar and not ref shows a message", {
  testthat::expect_message(
    {
      data("MIsim", package = "rsimsum")
      s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
    },
    "'ref' method was not specified, CC set as the reference"
  )
})

testthat::test_that("running simsum on MIsim return summaries of the correct dimension", {
  data("MIsim", package = "rsimsum")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  testthat::expect_equal(dim(s$summ), expected = c(42, 4))
})

testthat::test_that("simsum with mcse option returns mcse", {
  data("MIsim", package = "rsimsum")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  testthat::expect_true("mcse" %in% names(s$summ))
})

testthat::test_that("simsum without mcse option does not returns mcse", {
  data("MIsim", package = "rsimsum")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", control = list(mcse = FALSE))
  testthat::expect_false("mcse" %in% names(s$summ))
})

testthat::test_that("simsum with by factors returns error when 'by' name is not a variable in data", {
  testthat::expect_error({
    data("relhaz", package = "rsimsum")
    simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = "by")
  })
})

testthat::test_that("simsum with by factors works fine", {
  data("relhaz", package = "rsimsum")
  testthat::expect_output(object = print(simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = "n")))
  testthat::expect_output(object = print(simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = "baseline")))
  testthat::expect_output(object = print(simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = c("n", "baseline"))))
})

testthat::test_that("simsum with by factors returns a data.frame with results", {
  data("relhaz", package = "rsimsum")
  s <- simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = c("n", "baseline"))
  testthat::expect_s3_class(object = s$summ, class = "data.frame")
})

testthat::test_that("simsum works with missing data and default arguments", {
  data("MIsim")
  x <- MIsim
  set.seed(180123)
  x[which(rnorm(nrow(x)) > 2), "b"] <- NA
  x[which(rnorm(nrow(x)) > 2), "se"] <- NA
  testthat::expect_output(object = print(simsum(data = x, estvarname = "b", true = 0.5, se = "se", methodvar = "method")))
})

testthat::test_that("simsum with x = FALSE does not return data", {
  data("MIsim", package = "rsimsum")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", x = FALSE)
  testthat::expect_null(object = s$x)
})

testthat::test_that("simsum with custom ci.limits works as expected", {
  data("MIsim", package = "rsimsum")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ci.limits = c(-Inf, Inf))
  testthat::expect_true(object = all(s$summ$est[s$summ$stat == "cover"] == 1))
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ci.limits = c(Inf, -Inf))
  testthat::expect_true(object = all(s$summ$est[s$summ$stat == "cover"] == 0))
})

testthat::test_that("simsum with x = TRUE does return data", {
  data("MIsim", package = "rsimsum")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", x = TRUE)
  testthat::expect_s3_class(object = s$x, class = "data.frame")
  testthat::expect_equal(object = nrow(s$x), expected = nrow(MIsim))
})

testthat::test_that("simsum with dropbig = TRUE does drop all the big stuff", {
  data("MIsim", package = "rsimsum")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", x = TRUE, dropbig = TRUE, control = list(dropbig.max = 3, dropbig.semax = 10, dropbig.robust = FALSE))
  expected <- .dropbig(data = MIsim, estvarname = "b", se = "se", methodvar = "method", by = NULL, robust = FALSE, max = 3, semax = 10)
  expected <- .na_pair(data = expected, estvarname = "b", se = "se")
  expected$method <- factor(expected$method)
  testthat::expect_equivalent(object = s$x, expected = expected)
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, methodvar = "method", x = TRUE, dropbig = TRUE, control = list(dropbig.max = 3, dropbig.robust = FALSE))
  expected <- .dropbig(data = MIsim, estvarname = "b", methodvar = "method", by = NULL, robust = FALSE, max = 3)
  expected <- .na_pair(data = expected, estvarname = "b")
  expected$method <- factor(expected$method)
  testthat::expect_equivalent(object = s$x, expected = expected)
})

testthat::test_that("simsum without 'true' does not compute bias, cover, mse", {
  data("MIsim", package = "rsimsum")
  s <- simsum(data = MIsim, estvarname = "b", se = "se", methodvar = "method")
  testthat::expect_false(object = any(c("bias", "cover", "mse") %in% s$summ$stat))
})

testthat::test_that("simsum without 'se' does not compute se2mean, se2median, modelse, relerror, cover, becover, power", {
  data("MIsim", package = "rsimsum")
  s <- simsum(data = MIsim, estvarname = "b", true = 0.5, methodvar = "method")
  testthat::expect_false(object = any(c("se2mean", "se2median", "modelse", "relerror", "cover", "becover", "power") %in% s$summ$stat))
})

testthat::test_that("simsum without 'se' nor 'true' does not compute se2mean, se2median, modelse, relerror, cover, becover, power, bias, mse", {
  data("MIsim", package = "rsimsum")
  s <- simsum(data = MIsim, estvarname = "b", methodvar = "method")
  testthat::expect_false(object = any(c("se2mean", "se2median", "modelse", "relerror", "cover", "becover", "power", "bias", "mse") %in% s$summ$stat))
})

testthat::test_that("simsum with both ci.limits and df throws an error", {
  data("tt", package = "rsimsum")
  testthat::expect_error(object = simsum(data = tt, estvarname = "diff", se = "se", methodvar = "method", ci.limits = c("lower", "upper"), df = "df"))
})
