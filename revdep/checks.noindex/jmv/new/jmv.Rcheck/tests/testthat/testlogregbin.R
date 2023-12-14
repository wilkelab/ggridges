testthat::context('logregbin')

testthat::test_that('All options in the logRegBin work (sunny)', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    N <- 100
    cov1 <- rnorm(N)
    cov2 <- rnorm(N)
    z <- 1 + 2*cov1 + 3*cov2
    pr <- 1 / ( 1 + exp(-z))
    dep <- factor(rbinom(N, 1, pr))

    df <- data.frame(
        `dep 1`=dep, `cov 1`=cov1, `cov 2`=cov2, check.names = FALSE
    )

    r <- jmv::logRegBin(
        data = df,
        dep = "dep 1",
        covs = c("cov 1", "cov 2"),
        blocks = list(list("cov 1", "cov 2")),
        refLevels = list(list(var="dep 1", ref="0")),
        modelTest = TRUE,
        bic = TRUE,
        pseudoR2 = c("r2mf", "r2cs", "r2n", "r2t"),
        omni = TRUE,
        ci = TRUE,
        OR = TRUE,
        ciOR = TRUE,
        class = TRUE,
        acc = TRUE,
        spec = TRUE,
        sens = TRUE,
        auc = TRUE,
        collin = TRUE,
        emMeans = ~`cov 1` + `cov 2`,
        emmPlots = FALSE,
        emmTables = TRUE
    )

    # Test model fit table
    modelFitTable <- r$modelFit$asDF
    testthat::expect_equal(1, modelFitTable[['model']])
    testthat::expect_equal(39.039, modelFitTable[['dev']], tolerance = 1e-3)
    testthat::expect_equal(45.039, modelFitTable[['aic']], tolerance = 1e-3)
    testthat::expect_equal(52.854, modelFitTable[['bic']], tolerance = 1e-3)
    testthat::expect_equal(0.701, modelFitTable[['r2mf']], tolerance = 1e-3)
    testthat::expect_equal(0.6, modelFitTable[['r2cs']], tolerance = 1e-3)
    testthat::expect_equal(0.823, modelFitTable[['r2n']], tolerance = 1e-3)
    testthat::expect_equal(0.733, modelFitTable[['r2t']], tolerance = 1e-3)
    testthat::expect_equal(91.645, modelFitTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(2, modelFitTable[['df']])
    testthat::expect_equal(0, modelFitTable[['p']])

    #  Test omnibus likelihood ratio tests table
    lrtTable <- r$models[[1]]$lrt$asDF
    testthat::expect_equal(c('cov 1', 'cov 2'), lrtTable[['term']])
    testthat::expect_equal(c(43.325, 72.433), lrtTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(c(1, 1), lrtTable[['df']])
    testthat::expect_equal(c(0, 0), lrtTable[['p']], tolerance = 1e-3)

    # Test model coefficients table
    coefTable <- r$models[[1]]$coef$asDF
    testthat::expect_equal(c('Intercept', 'cov 1', 'cov 2'), coefTable[['term']])
    testthat::expect_equal(c(0.926, 3.042, 4.929), coefTable[['est']], tolerance = 1e-3)
    testthat::expect_equal(c(0.022, 1.445, 2.643), coefTable[['lower']], tolerance = 1e-3)
    testthat::expect_equal(c(1.829, 4.639, 7.215), coefTable[['upper']], tolerance = 1e-3)
    testthat::expect_equal(c(0.461, 0.815, 1.166), coefTable[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(2.008, 3.734, 4.226), coefTable[['z']], tolerance = 1e-3)
    testthat::expect_equal(c(0.045, 0, 0), coefTable[['p']], tolerance = 1e-3)
    testthat::expect_equal(c(2.524, 20.944, 138.297), coefTable[['odds']], tolerance = 1e-3)
    testthat::expect_equal(c(1.023, 4.242, 14.06), coefTable[['oddsLower']], tolerance = 1e-3)
    testthat::expect_equal(c(6.229, 103.41, 1360.308), coefTable[['oddsUpper']], tolerance = 1e-3)

    # Test collinearity table
    collinTable <- r$models[[1]]$assump$collin$asDF
    testthat::expect_equal(c('cov 1', 'cov 2'), collinTable[['term']])
    testthat::expect_equal(c(2.331, 2.331), collinTable[['vif']], tolerance = 1e-3)
    testthat::expect_equal(c(0.429, 0.429), collinTable[['tol']], tolerance = 1e-3)

    # Test estimated marginal means table
    emmeansTable1 <- r$models[[1]]$emm[[1]]$emmTable$asDF
    testthat::expect_equal(c(-0.828, 0.237, 1.302), emmeansTable1[['cov 1']], tolerance = 1e-3)
    testthat::expect_equal(c(0.199, 0.864, 0.994), emmeansTable1[['prob']], tolerance = 1e-3)
    testthat::expect_equal(c(0.126, 0.062, 0.007), emmeansTable1[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(0.05, 0.691, 0.939), emmeansTable1[['lower']], tolerance = 1e-3)
    testthat::expect_equal(c(0.539, 0.947, 0.999), emmeansTable1[['upper']], tolerance = 1e-3)

    emmeansTable2 <- r$models[[1]]$emm[[2]]$emmTable$asDF
    testthat::expect_equal(c(-0.965, 0.041, 1.048), emmeansTable2[['cov 2']], tolerance = 1e-3)
    testthat::expect_equal(c(0.043, 0.864, 0.999), emmeansTable2[['prob']], tolerance = 1e-3)
    testthat::expect_equal(c(0.038, 0.062, 0.002), emmeansTable2[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(0.007, 0.691, 0.977), emmeansTable2[['lower']], tolerance = 1e-3)
    testthat::expect_equal(c(0.218, 0.947, 1), emmeansTable2[['upper']], tolerance = 1e-3)

    # Test classification table
    classTable <- r$models[[1]]$pred$class$asDF
    testthat::expect_equal('0', classTable[['name[0]']])
    testthat::expect_equal(28, classTable[['neg[0]']])
    testthat::expect_equal(8, classTable[['pos[0]']])
    testthat::expect_equal(77.778, classTable[['perc[0]']], tolerance = 1e-3)
    testthat::expect_equal('1', classTable[['name[1]']])
    testthat::expect_equal(5, classTable[['neg[1]']])
    testthat::expect_equal(59, classTable[['pos[1]']])
    testthat::expect_equal(92.188, classTable[['perc[1]']], tolerance = 1e-3)

    # Test predictive measures table
    measuresTable <- r$models[[1]]$pred$measures$asDF
    testthat::expect_equal(0.87, measuresTable[['accuracy']], tolerance = 1e-3)
    testthat::expect_equal(0.778, measuresTable[['spec']], tolerance = 1e-3)
    testthat::expect_equal(0.922, measuresTable[['sens']], tolerance = 1e-3)
    testthat::expect_equal(0.976, measuresTable[['auc']], tolerance = 1e-3)
})

testthat::test_that('logregbin works with factors', {
    set.seed(1337)

    N <- 100
    x <- sample(LETTERS[1:3], N, replace=TRUE)
    y <- sample(0:1, N, replace=TRUE)
    df <- data.frame(y=y, x=x)

    refLevels <- list(list(var="y", ref="0"),
                      list(var="x", ref="A"))

    logReg <- jmv::logRegBin(data = df, dep = "y", factors = "x",
                             blocks = list("x"), refLevels = refLevels)

    # Test coefficients table
    coef <- logReg$models[[1]]$coef$asDF
    testthat::expect_equal("x:", coef$term[2])
    testthat::expect_equal("B – A", coef$term[3])
    testthat::expect_equal("C – A", coef$term[4])
    testthat::expect_equal(-0.0606, coef$est[1], tolerance = 1e-3)
    testthat::expect_equal(-0.0824, coef$est[3], tolerance = 1e-3)
    testthat::expect_equal(0.112, coef$est[4], tolerance = 1e-3)
    testthat::expect_equal(0.348, coef$se[1], tolerance = 1e-3)
    testthat::expect_equal(0.515, coef$se[3], tolerance = 1e-3)
    testthat::expect_equal(0.473, coef$se[4], tolerance = 1e-3)
    testthat::expect_equal(-0.174, coef$z[1], tolerance = 1e-3)
    testthat::expect_equal(-0.160, coef$z[3], tolerance = 1e-3)
    testthat::expect_equal(0.236, coef$z[4], tolerance = 1e-3)
    testthat::expect_equal(0.862, coef$p[1], tolerance = 1e-3)
    testthat::expect_equal(0.873, coef$p[3], tolerance = 1e-3)
    testthat::expect_equal(0.813, coef$p[4], tolerance = 1e-3)
})

testthat::test_that('logregbin works with ordered factors', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    N <- 100
    x <- factor(sample(LETTERS[1:3], N, replace=TRUE), ordered = TRUE)
    y <- factor(sample(0:1, N, replace=TRUE), ordered = TRUE)
    df <- data.frame(y=y, x=x)

    refLevels <- list(list(var="y", ref="0"),
                      list(var="x", ref="A"))

    logReg <- jmv::logRegBin(data = df, dep = "y", factors = "x",
                             blocks = list("x"), refLevels = refLevels)

    # Test coefficients table
    coef <- logReg$models[[1]]$coef$asDF
    testthat::expect_equal("x:", coef$term[2])
    testthat::expect_equal("B – A", coef$term[3])
    testthat::expect_equal("C – A", coef$term[4])
    testthat::expect_equal(-0.0606, coef$est[1], tolerance = 1e-3)
    testthat::expect_equal(-0.0824, coef$est[3], tolerance = 1e-3)
    testthat::expect_equal(0.112, coef$est[4], tolerance = 1e-3)
    testthat::expect_equal(0.348, coef$se[1], tolerance = 1e-3)
    testthat::expect_equal(0.515, coef$se[3], tolerance = 1e-3)
    testthat::expect_equal(0.473, coef$se[4], tolerance = 1e-3)
    testthat::expect_equal(-0.174, coef$z[1], tolerance = 1e-3)
    testthat::expect_equal(-0.160, coef$z[3], tolerance = 1e-3)
    testthat::expect_equal(0.236, coef$z[4], tolerance = 1e-3)
    testthat::expect_equal(0.862, coef$p[1], tolerance = 1e-3)
    testthat::expect_equal(0.873, coef$p[3], tolerance = 1e-3)
    testthat::expect_equal(0.813, coef$p[4], tolerance = 1e-3)
})

testthat::test_that('Emmeans work with nuisance parameters (no interactions)', {
    #' Test that nuisance factors are handled correctly in the estimated marginal means
    #' See: https://cran.r-project.org/web/packages/emmeans/vignettes/messy-data.html
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    df <- data.frame(
        dep = sample(0:1, 100, replace=TRUE),
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
        list(var="dep", ref="0"),
        list(var=factors[1], ref="a"),
        list(var=factors[2], ref="A")
    )

    r <- jmv::logRegBin(
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
        c(-1.149, -1.149, -1.149, -0.08, -0.08, -0.08, 0.988, 0.988, 0.988),
        emmeansTable[['cov2']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(-0.852, 0.116, 1.083, -0.852, 0.116, 1.083, -0.852, 0.116, 1.083),
        emmeansTable[['cov1']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.593, 0.59, 0.587, 0.549, 0.546, 0.543, 0.505, 0.502, 0.499),
        emmeansTable[['prob']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.089, 0.072, 0.088, 0.074, 0.051, 0.073, 0.09, 0.073, 0.091),
        emmeansTable[['se']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.414, 0.445, 0.411, 0.405, 0.446, 0.4, 0.335, 0.362, 0.328),
        emmeansTable[['lower']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.75, 0.721, 0.743, 0.686, 0.643, 0.68, 0.674, 0.641, 0.67),
        emmeansTable[['upper']]
        , tolerance = 1e-3
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
        dep = sample(0:1, 100, replace=TRUE),
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
        list(var="dep", ref="0"),
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
        c(-1.149, -1.149, -1.149, -0.08, -0.08, -0.08, 0.988, 0.988, 0.988),
        emmeansTable[['cov2']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(-0.852, 0.116, 1.083, -0.852, 0.116, 1.083, -0.852, 0.116, 1.083),
        emmeansTable[['cov1']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.613, 0.589, 0.564, 0.567, 0.542, 0.518, 0.521, 0.496, 0.472),
        emmeansTable[['emmean']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.089, 0.071, 0.088, 0.073, 0.05, 0.074, 0.088, 0.072, 0.091),
        emmeansTable[['se']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.436, 0.447, 0.389, 0.423, 0.442, 0.372, 0.346, 0.352, 0.291),
        emmeansTable[['lower']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.791, 0.731, 0.74, 0.711, 0.643, 0.664, 0.696, 0.64, 0.652),
        emmeansTable[['upper']],
        tolerance = 1e-3
    )
})

testthat::test_that("Analysis works with global weights", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    weights <- sample(1:10, 100, replace=TRUE)

    df <- data.frame(
        dep = factor(sample(0:1, 100, replace=TRUE)),
        cov = rnorm(100),
        factor = factor(sample(LETTERS[1:3], 100, replace=TRUE))
    )
    attr(df, "jmv-weights") <- weights

    refLevels = list(list(var="dep", ref="0"), list(var="factor", ref="A"))

    r <- jmv::logRegBin(
        df,
        dep="dep",
        covs="cov",
        factors="factor",
        blocks=list(list("cov", "factor")),
        refLevels=refLevels,
    )

    # Test model fit table
    modelFitTable <- r$modelFit$asDF
    testthat::expect_equal(1, modelFitTable[['model']], tolerance = 1e-3)
    testthat::expect_equal(793.107, modelFitTable[['dev']], tolerance = 1e-3)
    testthat::expect_equal(801.107, modelFitTable[['aic']], tolerance = 1e-3)
    testthat::expect_equal(0.02, modelFitTable[['r2mf']], tolerance = 1e-3)

    # Test model coefficients table
    coefTable <- r$models[[1]]$coef$asDF
    testthat::expect_equal(c(-0.199, -0.251, NA, 0.457, 0.027), coefTable[['est']], tolerance = 1e-3)
    testthat::expect_equal(c(0.14, 0.085, NA, 0.201, 0.21), coefTable[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(-1.418, -2.949, NA, 2.276, 0.128), coefTable[['z']], tolerance = 1e-3)
    testthat::expect_equal(c(0.156, 0.003, NA, 0.023, 0.899), coefTable[['p']], tolerance = 1e-3)
})


testthat::test_that("Analysis adds note when design matrix is singular", {
    # GIVEN a singular data set
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    df <- data.frame(
        dep = rep(0:1, times=50),
        var1 = c(sample(letters[2:3], replace=TRUE, 50), rep(letters[1], 50)),
        var2 = c(sample(LETTERS[2:3], replace=TRUE, 50), rep(LETTERS[1], 50))
    )
    refLevels = list(
        list(var="dep", ref="0"), list(var="var1", ref=letters[1]), list(var="var2", ref=LETTERS[1])
    )

    # WHEN a binomial logistic regression is run on this data set
    r <- jmv::logRegBin(
        df,
        dep="dep",
        factors=c("var1", "var2"),
        blocks=list(list("var1", "var2")),
        refLevels=refLevels
    )

    # THEN the coefficients table contains a note informing the user on the singularity of the data
    notes <- r$models[[1]]$coef$notes
    testthat::expect_true("singular" %in% names(notes))
})

