testthat::context('linreg')

testthat::test_that('All options in the linreg work (sunny)', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    data <- data.frame(
        `dep var` = rnorm(100),
        `cov 1` = rnorm(100),
        `cov 2` = rnorm(100),
        `facto r` = sample(LETTERS[1:3], 100, replace=TRUE),
        stringsAsFactors = TRUE,
        check.names = FALSE
    )

    dep <- "dep var"
    covs <- c("cov 1", "cov 2")
    factors <- "facto r"
    blocks = list(list("cov 1", "cov 2", "facto r"))
    refLevels = list(list(var="facto r", ref="A"))

    r <- jmv::linReg(
        data,
        dep = !!dep,
        covs = !!covs,
        factors = !!factors,
        blocks = blocks,
        refLevels = refLevels,
        r2Adj = TRUE,
        aic = TRUE,
        bic = TRUE,
        rmse = TRUE,
        modelTest = TRUE,
        anova = TRUE,
        ci = TRUE,
        stdEst = TRUE,
        ciStdEst = TRUE,
        norm = TRUE,
        durbin = TRUE,
        collin = TRUE,
        cooks = TRUE,
        emMeans = ~ `cov 1` + `cov 2` + `facto r`,
        emmPlots = FALSE,
        emmTables = TRUE
    )

    # Test model fit table
    modelFitTable <- r$modelFit$asDF
    testthat::expect_equal(1, modelFitTable[['model']], tolerance = 1e-3)
    testthat::expect_equal(0.158, modelFitTable[['r']], tolerance = 1e-3)
    testthat::expect_equal(0.025, modelFitTable[['r2']], tolerance = 1e-3)
    testthat::expect_equal(-0.016, modelFitTable[['r2Adj']], tolerance = 1e-3)
    testthat::expect_equal(304.925, modelFitTable[['aic']], tolerance = 1e-5)
    testthat::expect_equal(320.556, modelFitTable[['bic']], tolerance = 1e-5)
    testthat::expect_equal(1.047, modelFitTable[['rmse']], tolerance = 1e-3)
    testthat::expect_equal(0.609, modelFitTable[['f']], tolerance = 1e-3)
    testthat::expect_equal(4, modelFitTable[['df1']])
    testthat::expect_equal(95, modelFitTable[['df2']])
    testthat::expect_equal(0.657, modelFitTable[['p']], tolerance = 1e-3)

    # Test omnibus ANOVA test table
    anovaTable <- r$models[[1]]$anova$asDF
    testthat::expect_equal(c('cov 1', 'cov 2', 'facto r', 'Residuals'), anovaTable[['term']])
    testthat::expect_equal(c(1.093, 0.183, 1.4, 109.567), anovaTable[['ss']], tolerance = 1e-3)
    testthat::expect_equal(c(1, 1, 2, 95), anovaTable[['df']])
    testthat::expect_equal(c(1.093, 0.183, 0.7, 1.153), anovaTable[['ms']], tolerance = 1e-3)
    testthat::expect_equal(c(0.947, 0.159, 0.607, NA), anovaTable[['F']], tolerance = 1e-3)
    testthat::expect_equal(c(0.333, 0.691, 0.547, NA), anovaTable[['p']], tolerance = 1e-3)

    # Test model coefficients table
    coefTable <- r$models[[1]]$coef$asDF
    testthat::expect_equal(
        c('Intercept', 'cov 1', 'cov 2', 'facto r:', 'B – A', 'C – A'), coefTable[['term']]
    )
    testthat::expect_equal(
        c(0.241, -0.105, -0.043, NA, 0.155, -0.137), coefTable[['est']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.2, 0.108, 0.107, NA, 0.286, 0.261), coefTable[['se']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(-0.155, -0.32, -0.255, NA, -0.412, -0.655), coefTable[['lower']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.638, 0.109, 0.17, NA, 0.723, 0.382), coefTable[['upper']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1.208, -0.973, -0.398, NA, 0.543, -0.524), coefTable[['t']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.23, 0.333, 0.691, NA, 0.589, 0.601), coefTable[['p']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(NA, -0.099, -0.042, NA, 0.146, -0.128), coefTable[['stdEst']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(NA, -0.302, -0.252, NA, -0.387, -0.615), coefTable[['stdEstLower']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(NA, 0.103, 0.168, NA, 0.679, 0.358), coefTable[['stdEstUpper']], tolerance = 1e-3
    )

    # Test cook's distance summary table
    cooksTable <- r$models[[1]]$dataSummary$cooks$asDF
    testthat::expect_equal(0.01, cooksTable[['mean']], tolerance = 1e-3)
    testthat::expect_equal(0.005, cooksTable[['median']], tolerance = 1e-3)
    testthat::expect_equal(0.014, cooksTable[['sd']], tolerance = 1e-3)
    testthat::expect_equal(0, cooksTable[['min']], tolerance = 1e-3)
    testthat::expect_equal(0.107, cooksTable[['max']], tolerance = 1e-3)

    # Test Durbin-Watson autocorrelation test table
    durbinTable <- r$models[[1]]$assump$durbin$asDF
    testthat::expect_equal(-0.103, durbinTable[['autoCor']], tolerance = 1e-3)
    testthat::expect_equal(2.194, durbinTable[['dw']], tolerance = 1e-3)
    testthat::expect_equal(0.326, durbinTable[['p']], tolerance = 1e-3)

    # Test collinearity statistics table
    collinTable <- r$models[[1]]$assump$collin$asDF
    testthat::expect_equal(c('cov 1', 'cov 2', 'facto r'), collinTable[['term']])
    testthat::expect_equal(c(1.008, 1.044, 1.022), collinTable[['vif']], tolerance = 1e-3)
    testthat::expect_equal(c(0.992, 0.958, 0.978), collinTable[['tol']], tolerance = 1e-3)

    # Test normality test table
    normTable <- r$models[[1]]$assump$norm$asDF
    testthat::expect_equal(0.994, normTable[['s[sw]']], tolerance = 1e-3)
    testthat::expect_equal(0.941, normTable[['p[sw]']], tolerance = 1e-3)

    # Test estimated marginal means
    emmeansCov1Table <- r$models[[1]]$emm[[1]]$emmTable$asDF
    testthat::expect_equal(c(-0.965, 0.041, 1.048), emmeansCov1Table[['cov 1']], tolerance = 1e-3)
    testthat::expect_equal(c(0.356, 0.25, 0.144), emmeansCov1Table[['emmean']], tolerance = 1e-3)
    testthat::expect_equal(c(0.153, 0.108, 0.154), emmeansCov1Table[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(0.053, 0.035, -0.162), emmeansCov1Table[['lower']], tolerance = 1e-3)
    testthat::expect_equal(c(0.659, 0.465, 0.45), emmeansCov1Table[['upper']], tolerance = 1e-3)

    emmeansCov2Table <- r$models[[1]]$emm[[2]]$emmTable$asDF
    testthat::expect_equal(c(-1.213, -0.159, 0.896), emmeansCov2Table[['cov 2']], tolerance = 1e-3)
    testthat::expect_equal(c(0.295, 0.25, 0.205), emmeansCov2Table[['emmean']], tolerance = 1e-3)
    testthat::expect_equal(c(0.157, 0.108, 0.155), emmeansCov2Table[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.017, 0.035, -0.104), emmeansCov2Table[['lower']], tolerance = 1e-3)
    testthat::expect_equal(c(0.606, 0.465, 0.514), emmeansCov2Table[['upper']], tolerance = 1e-3)

    emmeansFactorTable <- r$models[[1]]$emm[[3]]$emmTable$asDF
    testthat::expect_equal(c('A', 'B', 'C'), emmeansFactorTable[['facto r']])
    testthat::expect_equal(c(0.244, 0.399, 0.107), emmeansFactorTable[['emmean']], tolerance = 1e-3)
    testthat::expect_equal(c(0.196, 0.201, 0.172), emmeansFactorTable[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(-0.146, 0, -0.235), emmeansFactorTable[['lower']], tolerance = 1e-3)
    testthat::expect_equal(c(0.634, 0.798, 0.449), emmeansFactorTable[['upper']], tolerance = 1e-3)
})

testthat::test_that('Test that basic linear regression works', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(100)
    intercept <- rnorm(100) + 1
    a <- rnorm(100) * 2.5
    b <- rnorm(100) * .5
    c <- rnorm(100) * .1

    y <- intercept + a + b + c

    data <- list()
    data[["dep"]] <- y
    data[["var1"]] <- a
    data[["var 2"]] <- b
    data[["var3"]] <- c

    attr(data, 'row.names') <- seq_len(length(data[[1]]))
    attr(data, 'class') <- 'data.frame'

    dep <- "dep"
    covs <- c("var1", "var 2", "var3")
    blocks = list(list("var1", "var 2", "var3"))

    linreg <- jmv::linReg(data, dep=!!dep, covs=!!covs, blocks=blocks, stdEst = TRUE)

    modelFit <- linreg$modelFit$asDF
    coef <- linreg$models[[1]]$coef$asDF

    # Test model fit table
    testthat::expect_equal(0.875, modelFit$r[1], tolerance = 1e-3)
    testthat::expect_equal(0.766, modelFit$r2[1], tolerance = 1e-3)

    # Test coefficients table
    testthat::expect_equal(1.008, coef$est[1], tolerance = 1e-3)
    testthat::expect_equal(0.958, coef$se[4], tolerance = 1e-3)
    testthat::expect_true(is.na(coef$stdEst[1]))
})

testthat::test_that('Test that basic linear regression with blocks works', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(100)
    intercept <- rnorm(100) + 1
    a <- rnorm(100) * 2.5
    b <- rnorm(100) * .5
    c <- rnorm(100) * .1

    y <- intercept + a + b + c

    data <- list()
    data[["dep"]] <- y
    data[["var1"]] <- a
    data[["var 2"]] <- b
    data[["var3"]] <- c

    attr(data, 'row.names') <- seq_len(length(data[[1]]))
    attr(data, 'class') <- 'data.frame'

    dep <- "dep"
    covs <- c("var1", "var 2", "var3")
    blocks = list(list("var1", "var 2", "var3", c("var1", "var 2")))

    linreg <- jmv::linReg(
        data, dep=!!dep, covs=!!covs, blocks=blocks, stdEst = TRUE, ciStdEst = TRUE
    )

    coef <- linreg$models[[1]]$coef$asDF

    # Test coefficients table
    testthat::expect_equal(0.903, coef$stdEst[2], tolerance = 1e-3)
    testthat::expect_equal(0.189, coef$stdEstLower[3], tolerance = 1e-3)
    testthat::expect_equal(0.394, coef$stdEstUpper[3], tolerance = 1e-3)
})

testthat::test_that('Test different intercept codings', {
    data('ToothGrowth', package='datasets')
    data <- ToothGrowth
    data$dose <- factor(data$dose)

    dep <- "len"
    factors <- c("dose", "supp")
    blocks = list(list("dose", "supp"))
    refLevels = list(
        list(var="supp", ref="OJ"),
        list(var="dose", ref="0.5")
    )

    linreg <- jmv::linReg(
        data, dep=!!dep, factors=!!factors, blocks=blocks, refLevels=refLevels,
        emMeans=~ supp, emmTables=TRUE, emmPlots=FALSE
    )
    coef <- linreg$models[[1]]$coef$asDF
    testthat::expect_equal(12.455, coef$est[1], tolerance = 1e-3)

    emmeans <- linreg$models[[1]]$emm[[1]]$emmTable$asDF
    testthat::expect_equal(20.663, emmeans$emmean[1], tolerance = 1e-3)
})

testthat::test_that('Test that grand mean intercept works', {
    data('ToothGrowth', package='datasets')
    data <- ToothGrowth
    data$dose <- factor(data$dose)

    dep <- "len"
    factors <- c("dose", "supp")
    blocks = list(list("dose", "supp"))
    refLevels = list(
        list(var="supp", ref="OJ"),
        list(var="dose", ref="0.5")
    )

    linreg <- jmv::linReg(
        data, dep=!!dep, factors=!!factors, blocks=blocks, refLevels=refLevels, intercept='grandMean'
    )

    coef <- linreg$models[[1]]$coef
    testthat::expect_equal(coef$footnotes, "Represents grand mean")

    coefDf <- coef$asDF
    testthat::expect_equal(18.813, coefDf$est[1], tolerance = 1e-3)
})

testthat::test_that('Cooks summary in linreg works', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(100)
    intercept <- rnorm(100) + 1
    a <- rnorm(100) * 2.5
    b <- rnorm(100) * .5
    c <- rnorm(100) * .1

    y <- intercept + a + b + c

    data <- list()
    data[["dep"]] <- y
    data[["var1"]] <- a
    data[["var 2"]] <- b
    data[["var3"]] <- c

    attr(data, 'row.names') <- seq_len(length(data[[1]]))
    attr(data, 'class') <- 'data.frame'

    dep <- "dep"
    covs <- c("var1", "var 2", "var3")
    blocks = list(list("var1", "var 2", "var3"))

    linreg <- jmv::linReg(
        data,
        dep=!!dep,
        covs=!!covs,
        blocks=blocks,
        cooks=TRUE
    )

    cooksTable <- linreg$models[[1]]$dataSummary$cooks$asDF

    testthat::expect_equal(0.0109, cooksTable$mean, tolerance = 1e-4)
    testthat::expect_equal(0.0031, cooksTable$median, tolerance = 1e-4)
    testthat::expect_equal(0.0188, cooksTable$sd, tolerance = 1e-4)
    testthat::expect_equal(0.0000, cooksTable$min, tolerance = 1e-4)
    testthat::expect_equal(0.0966, cooksTable$max, tolerance = 1e-4)
})

testthat::test_that('emmeans table in linreg works with covariate with only two unique values', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(100)
    data <- data.frame(
        dep = rnorm(100),
        var1 = rnorm(100),
        var2 = sample(0:1, 100, replace = TRUE)
    )

    dep <- "dep"
    covs <- c("var1", "var2")
    blocks = list(list("var1", "var2"))

    linreg <- jmv::linReg(
        data,
        dep=!!dep,
        covs=!!covs,
        blocks=blocks,
        emMeans = ~var1:var2,
        emmTables = TRUE
    )

    emmeansTable <- linreg$models[[1]]$emm[[1]]$emmTable$asDF

    testthat::expect_equal(0.0077, emmeansTable$emmean[1], tolerance = 1e-4)
    testthat::expect_equal(0.1441, emmeansTable$se[2], tolerance = 1e-4)
    testthat::expect_equal(-0.1564, emmeansTable$lower[4], tolerance = 1e-4)
    testthat::expect_equal(0.1623, emmeansTable$upper[6], tolerance = 1e-4)
    testthat::expect_equal(0.2515, emmeansTable$emmean[7], tolerance = 1e-4)
    testthat::expect_equal(0.1821, emmeansTable$se[9], tolerance = 1e-4)
})

testthat::test_that("Analysis shows warning note on singular fit", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    N <- 20
    data <- data.frame(
        x1 = sample(letters[1:4], N, replace = TRUE),
        x2 = sample(letters[1:4], N, replace = TRUE),
        y = rnorm(N)
    )

    dep <- "y"
    factors <- c("x1", "x2")
    blocks = list(list("x1", "x2", c("x1", "x2")))
    refLevels = list(list(var="x1", ref="a"),
                     list(var="x2", ref="a"))

    r <- jmv::linReg(
        data,
        dep=!!dep,
        factors=!!factors,
        blocks=blocks,
        refLevels=refLevels,
        anova = TRUE,
        collin = TRUE
    )

    noteAnova <- r$models[[1]]$anova$notes$alias$note
    noteCoef <- r$models[[1]]$coef$notes$alias$note
    noteVIF <- r$models[[1]]$assump$collin$notes$alias$note

    testthat::expect_equal(noteAnova, "Linear model contains aliased coefficients (singular fit)")
    testthat::expect_equal(noteCoef, "Linear model contains aliased coefficients (singular fit)")
    testthat::expect_equal(noteVIF, "Linear model contains aliased coefficients (singular fit)")
})

testthat::test_that("Analysis works for covariate with one unique value", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    df <- data.frame(
        x = rep(1, 100),
        y = rnorm(100)
    )

    r <- jmv::linReg(
        df,
        dep="y",
        covs="x",
        blocks=list(list("x")),
    )

    coef <- r$models[[1]]$coef$asDF
    testthat::expect_equal(0.237, coef[1, "est"], tolerance = 1e-4)
    testthat::expect_equal(NaN, coef[2, "est"])
})

testthat::test_that("Analysis throws error for factor with one level", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    df <- data.frame(
        x = factor(rep(1, 100)),
        y = rnorm(100)
    )

    testthat::expect_error(
        {
            jmv::linReg(
                df,
                dep="y",
                factors="x",
                blocks=list(list("x")),
                refLevels = list(list(var="x", ref="1"))
            )
        },
        regexp = "needs to have at least 2 levels"
    )
})

testthat::test_that("Analysis works with global weights", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    weights <- abs(rnorm(100))

    df <- data.frame(
        dep = rnorm(100),
        cov = rnorm(100),
        factor = factor(sample(LETTERS[1:3], 100, replace=TRUE))
    )
    attr(df, "jmv-weights") <- weights

    refLevels = list(list(var="factor", ref="A"))

    r <- jmv::linReg(
        df,
        dep="dep",
        covs="cov",
        factors="factor",
        blocks=list(list("cov", "factor")),
        refLevels=refLevels,
    )

    coef <- r$models[[1]]$coef$asDF
    testthat::expect_equal(coef$est[1], -0.100, tolerance = 1e-3)
    testthat::expect_equal(coef$se[2], 0.089, tolerance = 1e-3)
    testthat::expect_equal(coef$t[4], 1.004, tolerance = 1e-3)
    testthat::expect_equal(coef$p[5], 0.247, tolerance = 1e-3)
})

testthat::test_that("Analysis works with legacy weights", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    df <- data.frame(
        weights = abs(rnorm(100)),
        dep = rnorm(100),
        cov = rnorm(100),
        factor = factor(sample(LETTERS[1:3], 100, replace=TRUE))
    )

    refLevels = list(list(var="factor", ref="A"))

    r <- jmv::linReg(
        df,
        dep="dep",
        covs="cov",
        factors="factor",
        weights="weights",
        blocks=list(list("cov", "factor")),
        refLevels=refLevels,
    )

    coef <- r$models[[1]]$coef
    coefDf <- coef$asDF

    testthat::expect_equal("Weighted by 'weights'", coef$notes$weights$note)
    testthat::expect_equal(coefDf$est[1], -0.100, tolerance = 1e-3)
    testthat::expect_equal(coefDf$se[2], 0.089, tolerance = 1e-3)
    testthat::expect_equal(coefDf$t[4], 1.004, tolerance = 1e-3)
    testthat::expect_equal(coefDf$p[5], 0.247, tolerance = 1e-3)
})

testthat::test_that("Analysis throws error with negative weights", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    df <- data.frame(
        weights = rnorm(100),
        dep = rnorm(100),
        cov = rnorm(100)
    )

    testthat::expect_error(
        {
            jmv::linReg(
                df,
                dep="dep",
                covs="cov",
                weights="weights",
                blocks=list(list("cov")),
            )
        },
        regexp = "Negative weights are not permitted"
    )
})

testthat::test_that('Emmeans work with nuisance parameters (no interactions)', {
    #' Test that nuisance factors are handled correctly in the estimated marginal means
    #' See: https://cran.r-project.org/web/packages/emmeans/vignettes/messy-data.html
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    df <- data.frame(
        dep = rnorm(100),
        cov1 = rnorm(100),
        cov2 = rnorm(100),
        factor1 = sample(letters[1:3], 100, replace=TRUE),
        factor2 = sample(LETTERS[1:2], 100, replace=TRUE),
        stringsAsFactors = TRUE
    )

    dep <- "dep"
    covs <- paste0("cov", 1:2)
    factors <- paste0("factor", 1:2)
    blocks = list(as.list(c(covs, factors)))
    refLevels = list(
        list(var=factors[1], ref="a"),
        list(var=factors[2], ref="A")
    )

    r <- jmv::linReg(
        df,
        dep = !!dep,
        covs = !!covs,
        factors = !!factors,
        blocks = blocks,
        refLevels = refLevels,
        emMeans = ~ cov1:cov2,
        emmPlots = FALSE,
        emmTables = TRUE
    )

    # Test estimated marginal means
    emmeansTable <- r$models[[1]]$emm[[1]]$emmTable$asDF
    testthat::expect_equal(
        c(-1.213, -1.213, -1.213, -0.159, -0.159, -0.159, 0.896, 0.896, 0.896),
        emmeansTable[['cov2']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(-0.965, 0.041, 1.048, -0.965, 0.041, 1.048, -0.965, 0.041, 1.048),
        emmeansTable[['cov1']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.389, 0.292, 0.196, 0.341, 0.245, 0.148, 0.293, 0.197, 0.101),
        emmeansTable[['emmean']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.185, 0.157, 0.197, 0.153, 0.108, 0.154, 0.195, 0.156, 0.185),
        emmeansTable[['se']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.022, -0.019, -0.195, 0.037, 0.03, -0.158, -0.095, -0.112, -0.266),
        emmeansTable[['lower']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.756, 0.604, 0.588, 0.645, 0.46, 0.455, 0.681, 0.506, 0.467),
        emmeansTable[['upper']],
        tolerance = 1e-3
    )
})


testthat::test_that('Emmeans work with nuisance parameters (with interactions)', {
    #' Test that nuisance factors are handled correctly in the estimated marginal means
    #' When a nuisance factor is included in an interaction it should still be included
    #' in the reference grid.
    #' See: https://cran.r-project.org/web/packages/emmeans/vignettes/messy-data.html
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    df <- data.frame(
        dep = rnorm(100),
        cov1 = rnorm(100),
        cov2 = rnorm(100),
        factor1 = sample(letters[1:3], 100, replace=TRUE),
        factor2 = sample(LETTERS[1:2], 100, replace=TRUE),
        stringsAsFactors = TRUE
    )

    dep <- "dep"
    covs <- c("cov1", "cov2")
    factors <- c("factor1", "factor2")
    blocks = list(list("cov1", "cov2", "factor1", "factor2", c("cov1", "factor1")))
    refLevels = list(
        list(var=factors[1], ref="a"),
        list(var=factors[2], ref="A")
    )

    r <- jmv::linReg(
        df,
        dep = !!dep,
        covs = !!covs,
        factors = !!factors,
        blocks = blocks,
        refLevels = refLevels,
        emMeans = ~ cov1:cov2,
        emmPlots = FALSE,
        emmTables = TRUE
    )

    # Test estimated marginal means
    emmeansTable <- r$models[[1]]$emm[[1]]$emmTable$asDF
    testthat::expect_equal(
        c(-1.213, -1.213, -1.213, -0.159, -0.159, -0.159, 0.896, 0.896, 0.896),
        emmeansTable[['cov2']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(-0.965, 0.041, 1.048, -0.965, 0.041, 1.048, -0.965, 0.041, 1.048),
        emmeansTable[['cov1']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.403, 0.29, 0.177, 0.35, 0.237, 0.124, 0.297, 0.184, 0.071),
        emmeansTable[['emmean']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.193, 0.159, 0.2, 0.156, 0.109, 0.16, 0.197, 0.161, 0.198),
        emmeansTable[['se']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.02, -0.027, -0.22, 0.041, 0.02, -0.195, -0.094, -0.135, -0.321),
        emmeansTable[['lower']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.786, 0.607, 0.573, 0.66, 0.454, 0.443, 0.688, 0.504, 0.464),
        emmeansTable[['upper']],
        tolerance = 1e-3
    )
})



