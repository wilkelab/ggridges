testthat::context("autoplot")

data("MIsim", package = "rsimsum")
data("relhaz", package = "rsimsum")
single <- rsimsum::simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", ref = "CC", x = T)
multi <- rsimsum::simsum(data = relhaz, estvarname = "theta", true = -0.5, se = "se", methodvar = "model", by = c("n", "baseline"), x = TRUE)
singlesum <- summary(single)
multisum <- summary(multi)
data("nlp", package = "rsimsum")
nlps <- rsimsum::simsum(data = nlp, estvarname = "b", true = 0, se = "se", methodvar = "model", by = c("baseline", "ss", "esigma"))
nlpssum <- summary(nlps)
data("MIsim2", package = "rsimsum")
multiplemethodvar <- rsimsum::simsum(data = MIsim2, estvarname = "b", true = 0.5, se = "se", methodvar = c("m1", "m2"), x = T)
multiplemethodvarsum <- summary(multiplemethodvar)

testthat::test_that("output from autoplot is of class gg, ggplot", {
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "est_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "se_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "est_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "se_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, type = "heat"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "est_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "se_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "est_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "se_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "heat"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "est_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "se_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "est_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "se_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "heat"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "est_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "se_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "est_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "se_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "heat"), class = c("gg", "ggplot"))
  # nested loop plot
  testthat::expect_s3_class(object = autoplot(nlps, type = "nlp"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(nlpssum, type = "nlp"), class = c("gg", "ggplot"))
  # multiple columns identifying methods
  testthat::expect_s3_class(object = autoplot(multiplemethodvar), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvar, type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvar, type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvar, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvar, type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvar, type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvar, type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvar, type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvar, type = "est_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvar, type = "se_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvar, type = "est_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvar, type = "se_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvar, type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvar, type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvar, type = "heat"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum, type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum, type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum, type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum, type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum, type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum, type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum, type = "est_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum, type = "se_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum, type = "est_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum, type = "se_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum, type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum, type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multiplemethodvarsum, type = "heat"), class = c("gg", "ggplot"))
})

testthat::test_that("argument checks works throws errors when appropriate", {
  # simsum object, no 'by'
  testthat::expect_error(object = print(autoplot(single, type = "megacool_plot")))
  testthat::expect_error(object = print(autoplot(single, type = 1)))
  testthat::expect_error(object = print(autoplot(single, type = TRUE)))
  testthat::expect_error(object = print(autoplot(single, stats = "summary")))
  testthat::expect_error(object = print(autoplot(single, stats = "BIAS")))
  testthat::expect_error(object = print(autoplot(single, stats = 1)))
  testthat::expect_error(object = print(autoplot(single, stats = TRUE)))
  testthat::expect_error(object = print(autoplot(single, target = "top")))
  testthat::expect_error(object = print(autoplot(single, target = TRUE)))
  testthat::expect_error(object = print(autoplot(single, fitted = NULL)))
  testthat::expect_error(object = print(autoplot(single, fitted = 1)))
  testthat::expect_error(object = print(autoplot(single, fitted = "yes")))
  testthat::expect_error(object = print(autoplot(single, scales = "both")))
  testthat::expect_error(object = print(autoplot(single, scales = "none")))
  testthat::expect_error(object = print(autoplot(single, scales = "either")))
  testthat::expect_error(object = print(autoplot(single, top = 0)))
  testthat::expect_error(object = print(autoplot(single, top = "Yes!")))
  testthat::expect_error(object = print(autoplot(single, density.legend = 0)))
  testthat::expect_error(object = print(autoplot(single, density.legend = "Bamboozled")))
  testthat::expect_error(object = print(autoplot(single, zoom = TRUE)))
  testthat::expect_error(object = print(autoplot(single, zoom = "Why not?")))
  # simsum object, with 'by'
  testthat::expect_error(object = print(autoplot(multi, type = "megacool_plot")))
  testthat::expect_error(object = print(autoplot(multi, type = 1)))
  testthat::expect_error(object = print(autoplot(multi, type = TRUE)))
  testthat::expect_error(object = print(autoplot(multi, stats = "summary")))
  testthat::expect_error(object = print(autoplot(multi, stats = "BIAS")))
  testthat::expect_error(object = print(autoplot(multi, stats = 1)))
  testthat::expect_error(object = print(autoplot(multi, stats = TRUE)))
  testthat::expect_error(object = print(autoplot(multi, target = "top")))
  testthat::expect_error(object = print(autoplot(multi, target = TRUE)))
  testthat::expect_error(object = print(autoplot(multi, fitted = NULL)))
  testthat::expect_error(object = print(autoplot(multi, fitted = 1)))
  testthat::expect_error(object = print(autoplot(multi, fitted = "yes")))
  testthat::expect_error(object = print(autoplot(multi, scales = "both")))
  testthat::expect_error(object = print(autoplot(multi, scales = "none")))
  testthat::expect_error(object = print(autoplot(multi, scales = "either")))
  testthat::expect_error(object = print(autoplot(multi, top = 0)))
  testthat::expect_error(object = print(autoplot(multi, top = "Yes!")))
  testthat::expect_error(object = print(autoplot(multi, density.legend = 0)))
  testthat::expect_error(object = print(autoplot(multi, density.legend = "Bamboozled")))
  testthat::expect_error(object = print(autoplot(multi, zoom = TRUE)))
  testthat::expect_error(object = print(autoplot(multi, zoom = "Why not?")))
  # summary.simsum object, no 'by'
  testthat::expect_error(object = print(autoplot(singlesum, type = "megacool_plot")))
  testthat::expect_error(object = print(autoplot(singlesum, type = 1)))
  testthat::expect_error(object = print(autoplot(singlesum, type = TRUE)))
  testthat::expect_error(object = print(autoplot(singlesum, stats = "summary")))
  testthat::expect_error(object = print(autoplot(singlesum, stats = "BIAS")))
  testthat::expect_error(object = print(autoplot(singlesum, stats = 1)))
  testthat::expect_error(object = print(autoplot(singlesum, stats = TRUE)))
  testthat::expect_error(object = print(autoplot(singlesum, target = "top")))
  testthat::expect_error(object = print(autoplot(singlesum, target = TRUE)))
  testthat::expect_error(object = print(autoplot(singlesum, fitted = NULL)))
  testthat::expect_error(object = print(autoplot(singlesum, fitted = 1)))
  testthat::expect_error(object = print(autoplot(singlesum, fitted = "yes")))
  testthat::expect_error(object = print(autoplot(singlesum, scales = "both")))
  testthat::expect_error(object = print(autoplot(singlesum, scales = "none")))
  testthat::expect_error(object = print(autoplot(singlesum, scales = "either")))
  testthat::expect_error(object = print(autoplot(singlesum, top = 0)))
  testthat::expect_error(object = print(autoplot(singlesum, top = "Yes!")))
  testthat::expect_error(object = print(autoplot(singlesum, density.legend = 0)))
  testthat::expect_error(object = print(autoplot(singlesum, density.legend = "Bamboozled")))
  testthat::expect_error(object = print(autoplot(singlesum, zoom = TRUE)))
  testthat::expect_error(object = print(autoplot(singlesum, zoom = "Why not?")))
  # summary.simsum object, with 'by'
  testthat::expect_error(object = print(autoplot(multisum, type = "megacool_plot")))
  testthat::expect_error(object = print(autoplot(multisum, type = 1)))
  testthat::expect_error(object = print(autoplot(multisum, type = TRUE)))
  testthat::expect_error(object = print(autoplot(multisum, stats = "summary")))
  testthat::expect_error(object = print(autoplot(multisum, stats = "BIAS")))
  testthat::expect_error(object = print(autoplot(multisum, stats = 1)))
  testthat::expect_error(object = print(autoplot(multisum, stats = TRUE)))
  testthat::expect_error(object = print(autoplot(multisum, target = "top")))
  testthat::expect_error(object = print(autoplot(multisum, target = TRUE)))
  testthat::expect_error(object = print(autoplot(multisum, fitted = NULL)))
  testthat::expect_error(object = print(autoplot(multisum, fitted = 1)))
  testthat::expect_error(object = print(autoplot(multisum, fitted = "yes")))
  testthat::expect_error(object = print(autoplot(multisum, scales = "both")))
  testthat::expect_error(object = print(autoplot(multisum, scales = "none")))
  testthat::expect_error(object = print(autoplot(multisum, scales = "either")))
  testthat::expect_error(object = print(autoplot(multisum, top = 0)))
  testthat::expect_error(object = print(autoplot(multisum, top = "Yes!")))
  testthat::expect_error(object = print(autoplot(multisum, density.legend = 0)))
  testthat::expect_error(object = print(autoplot(multisum, density.legend = "Bamboozled")))
  testthat::expect_error(object = print(autoplot(multisum, zoom = TRUE)))
  testthat::expect_error(object = print(autoplot(multisum, zoom = "Why not?")))
  # multiple columns identifying methods
  testthat::expect_error(object = print(autoplot(multiplemethodvar, type = "megacool_plot")))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, type = 1)))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, type = TRUE)))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, stats = "summary")))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, stats = "BIAS")))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, stats = 1)))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, stats = TRUE)))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, target = "top")))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, target = TRUE)))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, fitted = NULL)))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, fitted = 1)))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, fitted = "yes")))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, scales = "both")))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, scales = "none")))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, scales = "either")))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, top = 0)))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, top = "Yes!")))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, density.legend = 0)))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, density.legend = "Bamboozled")))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, zoom = TRUE)))
  testthat::expect_error(object = print(autoplot(multiplemethodvar, zoom = "Why not?")))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, type = "megacool_plot")))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, type = 1)))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, type = TRUE)))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, stats = "summary")))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, stats = "BIAS")))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, stats = 1)))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, stats = TRUE)))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, target = "top")))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, target = TRUE)))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, fitted = NULL)))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, fitted = 1)))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, fitted = "yes")))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, scales = "both")))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, scales = "none")))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, scales = "either")))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, top = 0)))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, top = "Yes!")))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, density.legend = 0)))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, density.legend = "Bamboozled")))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, zoom = TRUE)))
  testthat::expect_error(object = print(autoplot(multiplemethodvarsum, zoom = "Why not?")))
})

testthat::test_that("autoplot with target", {
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, target = 1), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, target = 1), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, target = 1), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, target = 1), class = c("gg", "ggplot"))
})

testthat::test_that("autoplot with stats", {
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, stats = "cover"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, stats = "cover"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, stats = "cover"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, stats = "cover"), class = c("gg", "ggplot"))
})

testthat::test_that("autoplot with target, stats", {
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, target = 0.50, stats = "cover"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, target = 0.50, stats = "cover"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, target = 0.50, stats = "cover"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, target = 0.50, stats = "cover"), class = c("gg", "ggplot"))
})

testthat::test_that("autoplot with fitted", {
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, fitted = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, fitted = FALSE), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, fitted = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, fitted = FALSE), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, fitted = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, fitted = FALSE), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, fitted = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, fitted = FALSE), class = c("gg", "ggplot"))
})

testthat::test_that("autoplot with scales", {
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, scales = "fixed"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, scales = "free"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, scales = "free_x"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(single, scales = "free_y"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, scales = "fixed"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, scales = "free"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, scales = "free_x"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, scales = "free_y"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, scales = "fixed"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, scales = "free"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, scales = "free_x"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, scales = "free_y"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, scales = "fixed"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, scales = "free"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, scales = "free_x"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, scales = "free_y"), class = c("gg", "ggplot"))
})

testthat::test_that("autoplot with top", {
  # nested loop plot
  testthat::expect_s3_class(object = autoplot(nlps, type = "nlp", top = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(nlps, type = "nlp", top = FALSE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(nlpssum, type = "nlp", top = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(nlpssum, type = "nlp", top = FALSE), class = c("gg", "ggplot"))
})

testthat::test_that("autoplot with density.legend", {
  # density plot
  testthat::expect_s3_class(object = autoplot(singlesum, type = "est_density", density.legend = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "se_density", density.legend = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "est_density", density.legend = FALSE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "se_density", density.legend = FALSE), class = c("gg", "ggplot"))
  # hex plot
  testthat::expect_s3_class(object = autoplot(singlesum, type = "est_hex", density.legend = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "se_hex", density.legend = TRUE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "est_hex", density.legend = FALSE), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "se_hex", density.legend = FALSE), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, scales = "fixed"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, scales = "free"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, scales = "free_x"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, scales = "free_y"), class = c("gg", "ggplot"))
})

testthat::test_that("autoplot with zoom", {
  # nested loop plot
  testthat::expect_s3_class(object = autoplot(single, type = "zip", zoom = 0.5), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "zip", zoom = 0.5), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, type = "zip", zoom = 0.5), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, type = "zip", zoom = 0.5), class = c("gg", "ggplot"))
})

testthat::test_that("inferring target", {
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, stats = "bias"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, stats = "bias"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, stats = "bias"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, stats = "bias"), class = c("gg", "ggplot"))
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, stats = "cover"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, stats = "cover"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, stats = "cover"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, stats = "cover"), class = c("gg", "ggplot"))
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, stats = "becover"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, stats = "becover"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, stats = "becover"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, stats = "becover"), class = c("gg", "ggplot"))
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, stats = "power"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, stats = "power"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, stats = "power"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, stats = "power"), class = c("gg", "ggplot"))
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, stats = "thetamean"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, stats = "thetamean"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, stats = "thetamean"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, stats = "thetamean"), class = c("gg", "ggplot"))
  # simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(single, stats = "thetamedian"), class = c("gg", "ggplot"))
  # simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multi, stats = "thetamedian"), class = c("gg", "ggplot"))
  # summary.simsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(singlesum, stats = "thetamedian"), class = c("gg", "ggplot"))
  # summary.simsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(multisum, stats = "thetamedian"), class = c("gg", "ggplot"))
})

testthat::test_that("zip with t critical values", {
  data("tt", package = "rsimsum")
  tt1 <- tt2 <- tt
  tt1$yo <- "One"
  tt2$yo <- "Two"
  doublett <- do.call(rbind.data.frame, list(tt1, tt2))

  single <- rsimsum::simsum(data = tt, estvarname = "diff", true = -1, se = "se", methodvar = "method", x = TRUE, df = "df")
  multi <- rsimsum::multisimsum(data = doublett, par = "yo", estvarname = "diff", true = c(One = -1, Two = -1), se = "se", methodvar = "method", x = TRUE, df = "df")
  singlesum <- summary(single)
  multisum <- summary(multi)

  testthat::expect_s3_class(object = autoplot(single, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, par = "One", type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multi, par = "Two", type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(singlesum, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, par = "One", type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(multisum, par = "Two", type = "zip"), class = c("gg", "ggplot"))
})

data("frailty", package = "rsimsum")
ms <- rsimsum::multisimsum(
  data = frailty,
  par = "par", true = c(trt = -0.50, fv = 0.75),
  estvarname = "b", se = "se", methodvar = "model",
  x = TRUE
)
sms <- summary(ms)
ms2 <- rsimsum::multisimsum(
  data = frailty,
  par = "par", true = c(trt = -0.50, fv = 0.75),
  estvarname = "b", se = "se", methodvar = "model",
  by = "fv_dist",
  x = TRUE
)
sms2 <- summary(ms2)

testthat::test_that("output from autoplot is of class gg, ggplot", {
  # multisimsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(ms, par = "trt"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "est_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "se_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "est_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "se_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms, par = "trt", type = "heat"), class = c("gg", "ggplot"))
  # multisimsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "est_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "se_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "est_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "se_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "heat"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(ms2, par = "trt", type = "nlp"), class = c("gg", "ggplot"))
  # summary.multisimsum object, no 'by'
  testthat::expect_s3_class(object = autoplot(sms, par = "trt"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "est_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "se_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "est_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "se_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms, par = "trt", type = "heat"), class = c("gg", "ggplot"))
  # summary.multisimsum object, with 'by'
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "forest"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "est"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "se"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "est_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "se_ba"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "est_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "se_density"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "est_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "se_hex"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "heat"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(sms2, par = "trt", type = "nlp"), class = c("gg", "ggplot"))
})

testthat::test_that("putting wrong 'par' (or no 'par' at all) throws an error", {
  # multisimsum object, no 'by'
  testthat::expect_error(object = autoplot(ms))
  testthat::expect_error(object = autoplot(ms, par = "42"))
  testthat::expect_error(object = autoplot(ms, par = 42))
  testthat::expect_error(object = autoplot(ms, par = TRUE))
  # multisimsum object, with 'by'
  testthat::expect_error(object = autoplot(ms2))
  testthat::expect_error(object = autoplot(ms2, par = "42"))
  testthat::expect_error(object = autoplot(ms2, par = 42))
  testthat::expect_error(object = autoplot(ms2, par = TRUE))
  # summary.multisimsum object, no 'by'
  testthat::expect_error(object = autoplot(sms))
  testthat::expect_error(object = autoplot(sms, par = "42"))
  testthat::expect_error(object = autoplot(sms, par = 42))
  testthat::expect_error(object = autoplot(sms, par = TRUE))
  # summary.multisimsum object, with 'by'
  testthat::expect_error(object = autoplot(sms2))
  testthat::expect_error(object = autoplot(sms2, par = "42"))
  testthat::expect_error(object = autoplot(sms2, par = 42))
  testthat::expect_error(object = autoplot(sms2, par = TRUE))
})


testthat::test_that("nlp with no 'by' factors throw an error", {
  testthat::expect_error(object = autoplot(ms, par = "trt", type = "nlp"), regexp = "Nested loop plot not meaningful")
})


### Test better handling of edge cases...
testthat::test_that("Edge cases for rsimsum >= 0.8.0", {
  data("tt", package = "rsimsum")
  tt$true <- -1
  s1 <- simsum(data = tt, estvarname = "diff", x = TRUE)
  s2 <- simsum(data = tt, estvarname = "diff", se = "se", x = TRUE)
  s3 <- simsum(data = tt, estvarname = "diff", true = -1, x = TRUE)
  s4 <- simsum(data = tt, estvarname = "diff", true = "true", x = TRUE)
  s5 <- simsum(data = tt, estvarname = "diff", se = "se", true = "true", x = TRUE)

  testthat::expect_error(object = autoplot(s1, type = "zip"))
  testthat::expect_error(object = autoplot(s2, type = "zip"))
  testthat::expect_error(object = autoplot(s3, type = "zip"))
  testthat::expect_error(object = autoplot(s4, type = "zip"))
  testthat::expect_error(object = autoplot(s5, type = "zip"))

  testthat::expect_error(object = autoplot(s1, type = "se"))
  testthat::expect_error(object = autoplot(s1, type = "se_ba"))
  testthat::expect_error(object = autoplot(s1, type = "se_ridge"))
  testthat::expect_error(object = autoplot(s1, type = "se_density"))
  testthat::expect_error(object = autoplot(s1, type = "se_hex"))

  testthat::expect_error(object = autoplot(s1, type = "lolly", stats = "thetamean"))
  testthat::expect_error(object = autoplot(s1, type = "lolly", stats = "thetamedian"))
  testthat::expect_error(object = autoplot(s4, type = "lolly", stats = "thetamean"))
  testthat::expect_error(object = autoplot(s4, type = "lolly", stats = "thetamedian"))
})

### More tests for zip plots...
testthat::test_that("Testing more zip plot cases", {
  data("tt", package = "rsimsum")

  s <- simsum(data = tt, estvarname = "diff", se = "se", true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))
  s <- simsum(data = tt, estvarname = "diff", se = "se", df = "df", true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))
  s <- simsum(data = tt, estvarname = "diff", se = "se", ci.limits = c("lower", "upper"), true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))

  s <- simsum(data = tt, estvarname = "diff", se = "se", methodvar = "method", true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))
  s <- simsum(data = tt, estvarname = "diff", se = "se", methodvar = "method", df = "df", true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))
  s <- simsum(data = tt, estvarname = "diff", se = "se", methodvar = "method", ci.limits = c("lower", "upper"), true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))

  s <- simsum(data = tt, estvarname = "diff", se = "se", by = "dgm", true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))
  s <- simsum(data = tt, estvarname = "diff", se = "se", by = "dgm", df = "df", true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))
  s <- simsum(data = tt, estvarname = "diff", se = "se", by = "dgm", ci.limits = c("lower", "upper"), true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))

  s <- simsum(data = tt, estvarname = "diff", se = "se", methodvar = "method", by = "dgm", true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))
  s <- simsum(data = tt, estvarname = "diff", se = "se", methodvar = "method", by = "dgm", df = "df", true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))
  s <- simsum(data = tt, estvarname = "diff", se = "se", methodvar = "method", by = "dgm", ci.limits = c("lower", "upper"), true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))

  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))
  s <- simsum(data = tt, estvarname = "diff", se = "se", ci.limits = c(-2, 0), true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))
  s <- simsum(data = tt, estvarname = "diff", se = "se", methodvar = "method", ci.limits = c(-2, 0), true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))
  s <- simsum(data = tt, estvarname = "diff", se = "se", by = "dgm", ci.limits = c(-2, 0), true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))
  s <- simsum(data = tt, estvarname = "diff", se = "se", methodvar = "method", by = "dgm", ci.limits = c(-2, 0), true = -1, x = TRUE)
  testthat::expect_s3_class(object = autoplot(s, type = "zip"), class = c("gg", "ggplot"))
})
