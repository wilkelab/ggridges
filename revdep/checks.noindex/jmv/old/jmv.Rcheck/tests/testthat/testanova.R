
testthat::context('anova')

data('ToothGrowth')

testthat::test_that('All options in the ANOVA work (sunny)', {
    r <- jmv::ANOVA(
        formula = len ~ supp + dose + supp:dose,
        data = ToothGrowth,
        effectSize = c("eta", "partEta", "omega"),
        modelTest = TRUE,
        homo = TRUE,
        norm = TRUE,
        contrasts = list(
            list(var="supp", type="deviation"),
            list(var="dose", type="simple")
        ),
        postHoc = ~ supp + dose + supp:dose,
        postHocCorr = c(
            "tukey",
            "scheffe",
            "bonf",
            "holm",
            "none"
        ),
        postHocES = "d",
        postHocEsCi = TRUE,
        emMeans = ~ supp:dose,
        emmPlots = FALSE,
        emmTables = TRUE
    )

    # Test main anova table
    main <- as.data.frame(r$main)

    testthat::expect_equal(
        c(2740.103, 205.35, 2426.434, 108.319, 712.106),
        main$ss,
        tolerance=1e-6
    )
    testthat::expect_equal(
        c(5, 1, 2, 2, 54),
        main$df
    )
    testthat::expect_equal(
        c(548.021, 205.35, 1213.217, 54.159, 13.187),
        main$ms,
        tolerance=1e-6
    )
    testthat::expect_equal(
        c(41.557, 15.572, 92, 4.107, NA),
        main$F,
        tolerance=1e-5
    )
    testthat::expect_equal(
        c(0, 0.00023, 0, 0.02186, NA),
        main$p,
        tolerance=1e-5
    )
    testthat::expect_equal(
        c(NA, 0.059, 0.703, 0.031, NA),
        main$etaSq,
        tolerance=1e-3
    )
    testthat::expect_equal(
        c(NA, 0.224, 0.773, 0.132, NA),
        main$etaSqP,
        tolerance=1e-3
    )
    testthat::expect_equal(
        c(NA, 0.055, 0.693, 0.024, NA),
        main$omegaSq,
        tolerance=1e-3
    )

    # Test homogeneity assumption test table
    homo <- r$assump$homo$asDF

    testthat::expect_equal(1.940, homo$F, tolerance=1e-4)
    testthat::expect_equal(5, homo$df1)
    testthat::expect_equal(54, homo$df2)
    testthat::expect_equal(0.1027, homo$p, tolerance=1e-4)

    # Test normality assumption test table
    norm <- r$assump$norm$asDF

    testthat::expect_equal(0.98499, norm$`s[sw]`, tolerance=1e-5)
    testthat::expect_equal(0.66942, norm$`p[sw]`, tolerance=1e-5)

    # Test contrast tables
    c1 <- r$contrasts[[1]]$asDF

    testthat::expect_equal("VC - OJ, VC", c1$contrast)
    testthat::expect_equal(-1.85, c1$est)
    testthat::expect_equal(0.469, c1$se, tolerance=1e-3)
    testthat::expect_equal(-3.946, c1$t, tolerance=1e-4)
    testthat::expect_equal(0.000231, c1$p, tolerance=1e-6)

    c2 <- r$contrasts[[2]]$asDF

    testthat::expect_equal(c("1000 - 500", "2000 - 500"), c2$contrast)
    testthat::expect_equal(c(9.13, 15.495), c2$est)
    testthat::expect_equal(c(1.148, 1.148), c2$se, tolerance=1e-3)
    testthat::expect_equal(c(7.951, 13.493), c2$t, tolerance=1e-4)
    testthat::expect_equal(c(0, 0), c2$p, tolerance=1e-6)

    # Test post-hoc tables
    ph1 <- r$postHoc[[1]]$asDF
    testthat::expect_equal("OJ", ph1$supp1)
    testthat::expect_equal("VC", ph1$supp2)
    testthat::expect_equal(3.7, ph1$md)
    testthat::expect_equal(0.938, ph1$se, tolerance=1e-3)
    testthat::expect_equal(54, ph1$df)
    testthat::expect_equal(3.946, ph1$t, tolerance=1e-4)
    testthat::expect_equal(0.000231, ph1$pnone, tolerance=1e-6)
    testthat::expect_equal(0.000231, ph1$ptukey, tolerance=1e-6)
    testthat::expect_equal(0.000231, ph1$pscheffe, tolerance=1e-6)
    testthat::expect_equal(0.000231, ph1$pbonferroni, tolerance=1e-6)
    testthat::expect_equal(0.000231, ph1$pholm, tolerance=1e-6)
    testthat::expect_equal(1.0189, ph1$d, tolerance=1e-4)
    testthat::expect_equal(0.4652, ph1$dlower, tolerance=1e-4)
    testthat::expect_equal(1.5726, ph1$dupper, tolerance=1e-5)

    ph2 <- r$postHoc[[2]]$asDF
    testthat::expect_equal(c("500", "500", "1000"), ph2$dose1)
    testthat::expect_equal(c("1000", "2000", "2000"), ph2$dose2)
    testthat::expect_equal(c(-9.13, -15.495, -6.365), ph2$md)
    testthat::expect_equal(c(1.148, 1.148, 1.148), ph2$se, tolerance=1e-3)
    testthat::expect_equal(c(54, 54, 54), ph2$df)
    testthat::expect_equal(c(-7.951, -13.493, -5.543), ph2$t, tolerance=1e-4)
    testthat::expect_equal(c(1e-10, 0, 9.121e-07), ph2$pnone, tolerance=1e-10)
    testthat::expect_equal(c(4e-10, 0, 2.7076e-06), ph2$ptukey, tolerance=1e-10)
    testthat::expect_equal(c(8e-10, 0, 5.2332e-06), ph2$pscheffe, tolerance=1e-10)
    testthat::expect_equal(c(4e-10, 0, 2.7363e-06), ph2$pbonferroni, tolerance=1e-10)
    testthat::expect_equal(c(2e-10, 0, 9.121e-07), ph2$pholm, tolerance=1e-10)
    testthat::expect_equal(c(-2.514, -4.267, -1.753), ph2$d, tolerance=1e-4)
    testthat::expect_equal(c(-3.312, -5.306, -2.471), ph2$dlower, tolerance=1e-4)
    testthat::expect_equal(c(-1.716, -3.228, -1.034), ph2$dupper, tolerance=1e-4)

    ph3 <- r$postHoc[[3]]$asDF
    testthat::expect_equal(
        c("OJ", "OJ", "OJ", "OJ", "OJ", "OJ", "OJ", "OJ", "OJ", "OJ", "OJ", "OJ", "VC", "VC", "VC"),
        ph3$supp1
    )
    testthat::expect_equal(
        c(
            "500", "500", "500", "500", "500", "1000", "1000", "1000", "1000", "2000", "2000",
            "2000", "500", "500", "1000"
        ),
        ph3$dose1
    )
    testthat::expect_equal(
        c("OJ", "OJ", "VC", "VC", "VC", "OJ", "VC", "VC", "VC", "VC", "VC", "VC", "VC", "VC", "VC"),
        ph3$supp2
    )
    testthat::expect_equal(
        c(
            "1000", "2000", "500", "1000", "2000", "2000", "500", "1000", "2000", "500", "1000",
            "2000", "1000", "2000", "2000"
        ),
        ph3$dose2
    )
    testthat::expect_equal(
        c(
            -9.47, -12.83, 5.25, -3.54, -12.91, -3.36, 14.72, 5.93, -3.44, 18.08, 9.29, -0.08,
            -8.79, -18.16, -9.37
        ),
        ph3$md
    )
    testthat::expect_equal(
        c(
            1.624, 1.624, 1.624, 1.624, 1.624, 1.624, 1.624, 1.624, 1.624, 1.624, 1.624, 1.624,
            1.624, 1.624, 1.624
        ),
        ph3$se,
        tolerance=1e-3
    )
    testthat::expect_equal(
        c(54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54),
        ph3$df
    )
    testthat::expect_equal(
        c(
            -5.831, -7.9, 3.233, -2.18, -7.949, -2.069, 9.064, 3.651, -2.118, 11.133, 5.72, -0.049,
            -5.413, -11.182, -5.77
        ),
        ph3$t,
        tolerance=1e-4
    )
    testthat::expect_equal(
        c(0, 0, 0.002, 0.034, 0, 0.043, 0, 0.001, 0.039, 0, 0, 0.961, 0, 0, 0),
        ph3$pnone,
        tolerance=1e-3
    )
    testthat::expect_equal(
        c(0, 0, 0.024, 0.264, 0, 0.319, 0, 0.007, 0.294, 0, 0, 1, 0, 0, 0),
        ph3$ptukey,
        tolerance=1e-3
    )
    testthat::expect_equal(
        c(0, 0, 0.081, 0.456, 0, 0.517, 0, 0.032, 0.49, 0, 0, 1, 0, 0, 0),
        ph3$pscheffe,
        tolerance=1e-3
    )
    testthat::expect_equal(
        c(0, 0, 0.031, 0.505, 0, 0.65, 0, 0.009, 0.582, 0, 0, 1, 0, 0, 0),
        ph3$pbonferroni,
        tolerance=1e-3
    )
    testthat::expect_equal(
        c(0, 0, 0.01, 0.135, 0, 0.135, 0, 0.004, 0.135, 0, 0, 0.961, 0, 0, 0),
        ph3$pholm,
        tolerance=1e-3
    )
    testthat::expect_equal(
        c(
            -2.608, -3.533, 1.446, -0.975, -3.555, -0.925, -4.054, 1.633, -0.947, -4.979, -2.558,
            -0.022, -2.421, -5.001, -2.58
        ),
        ph3$d,
        tolerance=1e-4
    )
    testthat::expect_equal(
        c(
            -3.636, -4.659, 0.507, -1.891, -4.684, -1.839, -5.243, 0.683, -1.862, -6.293, -3.582,
            -0.919, -3.431, -6.318, -3.606
        ),
        ph3$dlower,
        tolerance=1e-4
    )
    testthat::expect_equal(
        c(
            -1.58, -2.407, 2.385, -0.059, -2.426, -0.011, -2.864, 2.583, -0.032, -3.665, -1.535,
            0.875, -1.41, -3.684, -1.555
        ),
        ph3$dupper,
        tolerance=1e-3
    )

    # Test note in post-hoc tables
    testthat::expect_equal(
        "Comparisons are based on estimated marginal means",
        r$postHoc[[1]]$notes$note$note
    )

    # Test marginal means tables
    emm <- r$emm[[1]]$emmTable$asDF

    testthat::expect_equal(
        c("500", "500", "1000", "1000", "2000", "2000"),
        emm$dose
    )
    testthat::expect_equal(
        c("OJ", "VC", "OJ", "VC", "OJ", "VC"),
        emm$supp
    )
    testthat::expect_equal(
        c(13.23, 7.98, 22.7, 16.77, 26.06, 26.14),
        emm$mean,
    )
    testthat::expect_equal(
        c(1.148, 1.148, 1.148, 1.148, 1.148, 1.148),
        emm$se,
        tolerance=1e-3
    )
    testthat::expect_equal(
        c(10.928, 5.678, 20.398, 14.468, 23.758, 23.838),
        emm$lower,
        tolerance=1e-4
    )
    testthat::expect_equal(
        c(15.532, 10.282, 25.002, 19.072, 28.362, 28.442),
        emm$upper,
        tolerance=1e-4
    )
})

testthat::test_that('Analysis work with special characters in variable names', {
    data <- ToothGrowth
    data$dose <- factor(data$dose)
    levels(data$dose) <- paste(levels(data$dose), "ш")
    levels(data$supp) <- paste(levels(data$supp), "月")
    names(data) <- c("lㅈen", "su pp ツ", "do se ฐ")

    r <- jmv::ANOVA(
        formula = `lㅈen` ~ `su pp ツ` + `do se ฐ` + `su pp ツ`:`do se ฐ`,
        data = data,
        modelTest = TRUE,
    )

    main <- r$main$asDF
    testthat::expect_equal(
        c("Overall model", "su pp ツ", "do se ฐ", "su pp ツ:do se ฐ", "Residuals"),
        main$name,
    )
    testthat::expect_equal(
        c(2740.103, 205.35, 2426.434, 108.319, 712.106),
        main$ss,
        tolerance=1e-6
    )
    testthat::expect_equal(
        c(5, 1, 2, 2, 54),
        main$df
    )
    testthat::expect_equal(
        c(548.021, 205.35, 1213.217, 54.159, 13.187),
        main$ms,
        tolerance=1e-6
    )
    testthat::expect_equal(
        c(41.557, 15.572, 92, 4.107, NA),
        main$F,
        tolerance=1e-5
    )
    testthat::expect_equal(
        c(0, 0.00023, 0, 0.02186, NA),
        main$p,
        tolerance=1e-5
    )
})

testthat::test_that('Correct error message is displayed with perfect fit of the model', {
    df <- data.frame(
        dep = c(90, 87, 75, 60, 35, 50, 65, 70),
        var = factor(1:8)
    )

    testthat::expect_error(
        jmv::ANOVA(df, dep='dep', factors='var', qq=TRUE),
        "perfect fit"
    )
})

testthat::test_that('Provide error message when dep variable contains only one unique value', {
    df <- data.frame(
        dep = c(1, 1, 1, 1, 1, 1, 1),
        var = factor(c(1, 2, 1, 2, 1, 2, 1))
    )

    testthat::expect_error(
        jmv::ANOVA(formula=dep~var, data=df),
        "Dependent variable 'dep' contains only one unique value"
    )
})

testthat::test_that('Provide error message when a variable contains only missing values', {
    df <- data.frame(
        dep = 1:7,
        var = rep(NA, 7)
    )

    testthat::expect_error(
        jmv::ANOVA(formula=dep~var, data=df),
        "The dataset contains 0 rows"
    )
})

testthat::test_that('Provide error message when dep variable contains infinte values', {
    df <- data.frame(
        dep = c(1:6, Inf),
        var = rep(1:2, length.out = 7)
    )

    testthat::expect_error(
        jmv::ANOVA(formula=dep~var, data=df),
        "Dependent variable 'dep' contains infinite values"
    )
})

testthat::test_that("Contrasts work", {
    df <- data.frame(
        dep = c(1,6,3,2,6,2,3,1,9,2),
        var = factor(rep(1:2, length.out = 10))
    )

    contrasts <- list(
        list(var="var", type="deviation")
    )

    r <- jmv::ANOVA(data=df,dep="dep", factors=c("var"), contrasts=contrasts)

    contr <- r$contrasts[[1]]$asDF

    testthat::expect_equal(contr$est, -0.9)
    testthat::expect_equal(contr$se, 0.825, tolerance=1e-3)
    testthat::expect_equal(contr$t, -1.091, tolerance=1e-3)
    testthat::expect_equal(contr$p, 0.307, tolerance=1e-3)
})

testthat::test_that("Contrasts work with special characters in variable name", {
    df <- data.frame(
        dep = c(1,6,3,2,6,2,3,1,9,2),
        var = factor(rep(1:2, length.out = 10))
    )
    names(df) <- c("dep~A", "var A")

    contrasts <- list(
        list(var="var A", type="deviation")
    )

    r <- jmv::ANOVA(data=df,dep="dep~A", factors=c("var A"), contrasts=contrasts)

    contr <- r$contrasts[[1]]$asDF

    testthat::expect_equal(contr$est, -0.9)
    testthat::expect_equal(contr$se, 0.825, tolerance=1e-3)
    testthat::expect_equal(contr$t, -1.091, tolerance=1e-3)
    testthat::expect_equal(contr$p, 0.307, tolerance=1e-3)
})
