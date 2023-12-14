testthat::context('loglinear')

testthat::test_that('All options in the linreg work (sunny)', {
    data <- data.frame(
        "factor 1" = c("Group 1", "Group 1", "Group 2", "Group 2"),
        "factor 2" = c("Treatment 1", "Treatment 2", "Treatment 1", "Treatment 2"),
        "count s" = c(44, 86, 56, 78),
        check.names = FALSE,
        stringsAsFactors = TRUE
    )

    factors <- c("factor 1", "factor 2")
    counts <- "count s"
    blocks <- list(
        list("factor 1", "factor 2", c("factor 1", "factor 2"))
    )
    refLevels <- list(
        list(var="factor 1", ref="Group 1"),
        list(var="factor 2", ref="Treatment 1")
    )

    r <- jmv::logLinear(
        data,
        factors = !!factors,
        counts = !!counts,
        blocks = blocks,
        refLevels = refLevels,
        modelTest = TRUE,
        bic = TRUE,
        pseudoR2 = c("r2mf", "r2cs", "r2n"),
        omni = TRUE,
        ci = TRUE,
        RR = TRUE,
        ciRR = TRUE,
        emMeans = ~ `factor 1` + `factor 2` + `factor 1`:`factor 2`,
        emmPlots = FALSE,
        emmTables = TRUE
    )

    # Test model fit table
    modelFitTable <- r$modelFit$asDF
    testthat::expect_equal(1, modelFitTable[['model']])
    testthat::expect_equal(0, modelFitTable[['dev']], tolerance = 1e-3)
    testthat::expect_equal(31.983, modelFitTable[['aic']], tolerance = 1e-3)
    testthat::expect_equal(29.528, modelFitTable[['bic']], tolerance = 1e-3)
    testthat::expect_equal(1, modelFitTable[['r2mf']], tolerance = 1e-3)
    testthat::expect_equal(0.987, modelFitTable[['r2cs']], tolerance = 1e-3)
    testthat::expect_equal(1, modelFitTable[['r2n']], tolerance = 1e-3)
    testthat::expect_equal(17.505, modelFitTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(3, modelFitTable[['df']])
    testthat::expect_equal(0.001, modelFitTable[['p']], tolerance = 1e-3)

    # Test omnibus likelihood ratio tests table
    lrtTable <- r$models[[1]]$lrt$asDF
    testthat::expect_equal(
        c('factor 1', 'factor 2', 'factor 1:factor 2'), lrtTable[['term']]
    )
    testthat::expect_equal(c(1.443, 13.816, 1.773), lrtTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(c(1, 1, 1), lrtTable[['df']])
    testthat::expect_equal(c(0.23, 0, 0.183), lrtTable[['p']], tolerance = 1e-3)

    # Test model coefficients table
    coefTable <- r$models[[1]]$coef$asDF
    testthat::expect_equal(
        c(
            'Intercept',
            'factor 1:',
            'Group 2 – Group 1',
            'factor 2:',
            'Treatment 2 – Treatment 1',
            'factor 1:factor 2:',
            '(Group 2 – Group 1):(Treatment 2 – Treatment 1)'
        ),
        coefTable[['term']]
    )
    testthat::expect_equal(
        c(3.784, NA, 0.241, NA, 0.67, NA, -0.339), coefTable[['est']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(3.489, NA, -0.154, NA, 0.307, NA, -0.839), coefTable[['lower']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(4.08, NA, 0.636, NA, 1.033, NA, 0.161), coefTable[['upper']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.151, NA, 0.201, NA, 0.185, NA, 0.255), coefTable[['se']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(25.102, NA, 1.197, NA, 3.616, NA, -1.329), coefTable[['z']], tolerance = 1e-3
    )
    testthat::expect_equal(c(0, NA, 0.231, NA, 0, NA, 0.184), coefTable[['p']], tolerance = 1e-3)
    testthat::expect_equal(
        c(44, NA, 1.273, NA, 1.955, NA, 0.713), coefTable[['rate']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(32.744, NA, 0.858, NA, 1.359, NA, 0.432), coefTable[['rateLower']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(59.126, NA, 1.889, NA, 2.811, NA, 1.175), coefTable[['rateUpper']], tolerance = 1e-3
    )

    # Test estimated marginal means
    emmFactor1Table <- r$models[[1]]$emm[[1]]$emmTable$asDF
    testthat::expect_equal(c('Group 1', 'Group 2'), emmFactor1Table[['factor 1']])
    testthat::expect_equal(c(61.514, 66.091), emmFactor1Table[['counts']], tolerance = 1e-3)
    testthat::expect_equal(c(5.701, 5.788), emmFactor1Table[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(51.297, 55.667), emmFactor1Table[['lower']], tolerance = 1e-3)
    testthat::expect_equal(c(73.767, 78.467), emmFactor1Table[['upper']], tolerance = 1e-3)

    emmFactor2Table <- r$models[[1]]$emm[[2]]$emmTable$asDF
    testthat::expect_equal(c('Treatment 1', 'Treatment 2'), emmFactor2Table[['factor 2']])
    testthat::expect_equal(c(49.639, 81.902), emmFactor2Table[['counts']], tolerance = 1e-3)
    testthat::expect_equal(c(5, 6.403), emmFactor2Table[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(40.746, 70.267), emmFactor2Table[['lower']], tolerance = 1e-3)
    testthat::expect_equal(c(60.473, 95.465), emmFactor2Table[['upper']], tolerance = 1e-3)

    emmInteractionTable <- r$models[[1]]$emm[[3]]$emmTable$asDF
    testthat::expect_equal(
        c('Treatment 1', 'Treatment 1', 'Treatment 2', 'Treatment 2'),
        emmInteractionTable[['factor 2']]
    )
    testthat::expect_equal(
        c('Group 1', 'Group 2', 'Group 1', 'Group 2'), emmInteractionTable[['factor 1']]
    )
    testthat::expect_equal(c(44, 56, 86, 78), emmInteractionTable[['counts']], tolerance = 1e-3)
    testthat::expect_equal(
        c(6.633, 7.483, 9.274, 8.832), emmInteractionTable[['se']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(32.744, 43.096, 69.616, 62.476), emmInteractionTable[['lower']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(59.126, 72.767, 106.239, 97.381), emmInteractionTable[['upper']], tolerance = 1e-3
    )
})

testthat::test_that('loglinear works old scenario', {
    data('mtcars')

    tab <- table('gear'=mtcars$gear, 'cyl'=mtcars$cyl)
    dat <- as.data.frame(tab)

    logLin <- jmv::logLinear(
        data = dat, factors = c("gear", "cyl"),  counts = "Freq",
        blocks = list(list("gear", "cyl", c("gear", "cyl"))),
        refLevels = list(
            list(var="gear", ref="3"),
            list(var="cyl", ref="4")
        )
    )

    modelFit <- logLin$modelFit$asDF
    coef <- logLin$models[[1]]$coef$asDF

    # Test model fit table
    testthat::expect_equal(1, modelFit$r2mf[1], tolerance = 1e-3)
    testthat::expect_equal(41.382, modelFit$aic[1], tolerance = 1e-3)

    # Test coefficients table
    testthat::expect_equal(2.079, coef$est[3], tolerance = 1e-3)
    testthat::expect_equal(1.225, coef$se[6], tolerance = 1e-3)
    testthat::expect_equal(0.571, coef$p[4], tolerance = 1e-3)
})

testthat::test_that('Provide error message when factor contains fewer than two levels', {
    df <- data.frame(x = rep(1, 7))

    testthat::expect_error(
        jmv::logLinear(
            data=df,
            factors="x",
            blocks=list(list("x")),
            refLevels = list(list(var="x", ref="1"))
        ),
        "Factors must have at least two levels"
    )
})

testthat::test_that('Provide error message when data contains only missing values', {
    df <- data.frame(x = factor(rep(NA, 7)))

    testthat::expect_error(
        jmv::logLinear(
            data=df,
            factors="x",
            blocks=list(list("x")),
            refLevels = list(list(var="x", ref=NULL))
        ),
        "The dataset contains 0 rows"
    )
})
