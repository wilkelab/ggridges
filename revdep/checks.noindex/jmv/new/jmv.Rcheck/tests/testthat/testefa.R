testthat::context('efa')

testthat::test_that('All options in the efa work (sunny)', {
    data <- lavaan::HolzingerSwineford1939

    r <- jmv::efa(
        data = data,
        vars = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9"),
        nFactorMethod = "fixed",
        nFactors = 3,
        hideLoadings = 0,
        sortLoadings = TRUE,
        eigen = TRUE,
        factorCor = TRUE,
        factorSummary = TRUE,
        modelFit = TRUE,
        kmo = TRUE,
        bartlett = TRUE
    )

    # Test factor loadings table
    loadingsTable <- r$loadings$asDF
    testthat::expect_equal(
        c('x5', 'x4', 'x6', 'x3', 'x1', 'x2', 'x7', 'x8', 'x9'), loadingsTable[['name']]
    )
    testthat::expect_equal(
        c(0.886, 0.846, 0.805, -0.062, 0.196, 0.043, 0.044, -0.034, 0.032),
        loadingsTable[['pc1']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(-0.065, 0.016, 0.08, 0.686, 0.592, 0.509, -0.152, 0.125, 0.382),
        loadingsTable[['pc2']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.007, 0.008, -0.013, 0.019, 0.031, -0.122, 0.737, 0.686, 0.456),
        loadingsTable[['pc3']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.246, 0.272, 0.309, 0.547, 0.523, 0.745, 0.481, 0.48, 0.54),
        loadingsTable[['uniq']], tolerance = 1e-3
    )

    # Test factor summary table
    summaryTable <- r$factorStats$factorSummary$asDF
    testthat::expect_equal(c(2.24, 1.343, 1.274), summaryTable[['loadings']], tolerance = 1e-3)
    testthat::expect_equal(c(24.89, 14.926, 14.157), summaryTable[['varProp']], tolerance = 1e-4)
    testthat::expect_equal(c(24.89, 39.816, 53.974), summaryTable[['varCum']], tolerance = 1e-5)


    # Test inter-factor correlations table
    factorCorTable <- r$factorStats$factorCor
    testthat::expect_equal(0.322, factorCorTable$getCell(rowNo=1, "pc2")$value, tolerance = 1e-3)
    testthat::expect_equal(0.213, factorCorTable$getCell(rowNo=1, "pc3")$value, tolerance = 1e-3)
    testthat::expect_equal(0.261, factorCorTable$getCell(rowNo=2, "pc3")$value, tolerance = 1e-3)

    # Test model fit table
    modelFitTable <- r$modelFit$fit$asDF
    testthat::expect_equal(0.054, modelFitTable[['rmsea']], tolerance = 1e-3)
    testthat::expect_equal(0.016, modelFitTable[['rmseaLower']], tolerance = 1e-3)
    testthat::expect_equal(0.088, modelFitTable[['rmseaUpper']], tolerance = 1e-3)
    testthat::expect_equal(0.963, modelFitTable[['tli']], tolerance = 1e-3)
    testthat::expect_equal(-45.93, modelFitTable[['bic']], tolerance = 1e-3)
    testthat::expect_equal(22.555, modelFitTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(12, modelFitTable[['df']])
    testthat::expect_equal(0.032, modelFitTable[['p']], tolerance = 1e-3)

    # Test sphericity test table
    spherTable <- r$assump$bartlett$asDF
    testthat::expect_equal(904.097, spherTable[['chi']], tolerance = 1e-6)
    testthat::expect_equal(36, spherTable[['df']])
    testthat::expect_equal(0, spherTable[['p']], tolerance = 1e-3)

    # Test KMO table
    kmoTable <- r$assump$kmo$asDF
    testthat::expect_equal(
        c('Overall', 'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9'), kmoTable[['name']]
    )
    testthat::expect_equal(
        c(0.752, 0.805, 0.778, 0.734, 0.763, 0.739, 0.808, 0.593, 0.683, 0.788),
        kmoTable[['msa']], tolerance = 1e-3
    )

    # Test eigenvalues table
    eigenTable <- r$eigen$initEigen$asDF
    testthat::expect_equal(
        c(2.632, 0.934, 0.504, -0.092, -0.143, -0.169, -0.288, -0.302, -0.445),
        eigenTable[['eigen']], tolerance = 1e-3
    )
})

testthat::test_that('efa works old scenario', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(100)

    y <- rnorm(100)
    z <- rnorm(100)

    data <- list()
    data[["y1"]] <- y + .2 * rnorm(100)
    data[["y2"]] <- y + .2 * rnorm(100)
    data[["y3 plus"]] <- y + .2 * rnorm(100)
    data[["z1"]] <- z + .2 * rnorm(100)
    data[["z2"]] <- z + .2 * rnorm(100)

    attr(data, 'row.names') <- seq_len(length(data[[1]]))
    attr(data, 'class') <- 'data.frame'

    efa <- jmv::efa(
        data = data,
        vars = c("y1","y2","y3 plus","z1","z2"),
        nFactorMethod = 'fixed',
        nFactors = 2,
        hideLoadings = .3,
        rotation = 'varimax'
    )

    # Test loadings table
    testthat::expect_equal("", efa$loadings$getCell(rowNo=2, "pc2")$value)
    testthat::expect_equal(0.969985600224343, efa$loadings$getCell(rowNo=2, "pc1")$value)
    testthat::expect_equal(0.00443466058941522, efa$loadings$getCell(rowNo=4, "uniq")$value)
    testthat::expect_equal("y3 plus", efa$loadings$getCell(rowNo=3, "name")$value)

    testthat::expect_error(
        jmv::efa(data = data, vars = c("y1","y2","y3 plus","z1","z2"), nFactorMethod = "fixed", nFactors = 6),
        'Number of factors cannot be bigger than number of variables',
        fixed=TRUE
    )
})
