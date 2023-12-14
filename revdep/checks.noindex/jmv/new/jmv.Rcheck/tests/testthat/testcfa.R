testthat::context('cfa')

testthat::test_that('All options in the cfa work (sunny)', {
    data <- lavaan::HolzingerSwineford1939

    factors <- list(
        list(label='visual 1', vars=c("x1", "x2", "x3")),
        list(label='textual', vars=c("x4", "x5", "x6")),
        list(label='speed', vars=c("x7", "x8", "x9"))
    )
    resCov <- list(list(i1="x1",i2="x4"))

    r <- jmv::cfa(
        data = data,
        factors = factors,
        resCov = resCov,
        ci = TRUE,
        stdEst = TRUE,
        factInterceptEst = TRUE,
        resCovEst = TRUE,
        resInterceptEst = TRUE,
        fitMeasures = c("cfi", "tli", "rmsea", "srmr", "aic", "bic"),
        corRes = TRUE,
        mi = TRUE,
    )

    # Test factor loadings table
    loadingsTable <- r$factorLoadings$asDF
    testthat::expect_equal(
        c(
            'visual 1', 'visual 1', 'visual 1', 'textual', 'textual', 'textual', 'speed', 'speed',
            'speed'
        ),
        loadingsTable[['factor']]
    )
    testthat::expect_equal(
        c('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9'),
        loadingsTable[['indicator']]
    )
    testthat::expect_equal(
        c(0.879, 0.509, 0.67, 0.977, 1.106, 0.917, 0.616, 0.731, 0.672),
        loadingsTable[['est']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.083, 0.081, 0.078, 0.057, 0.063, 0.054, 0.075, 0.076, 0.078),
        loadingsTable[['se']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.716, 0.351, 0.516, 0.866, 0.983, 0.812, 0.47, 0.581, 0.518),
        loadingsTable[['lower']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1.043, 0.668, 0.823, 1.089, 1.229, 1.023, 0.763, 0.88, 0.825),
        loadingsTable[['upper']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(10.54, 6.289, 8.55, 17.148, 17.653, 17.035, 8.242, 9.58, 8.575),
        loadingsTable[['z']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0, 0, 0, 0, 0, 0, 0, 0, 0), loadingsTable[['p']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.755, 0.433, 0.593, 0.845, 0.858, 0.839, 0.567, 0.723, 0.667),
        loadingsTable[['stdEst']], tolerance = 1e-3
    )

    # Test factor covariances table
    facCovTable <- r$factorEst$factorCov$asDF
    testthat::expect_equal(
        c('visual 1', 'visual 1', 'visual 1', 'textual', 'textual', 'speed'),
        facCovTable[['factor1']]
    )
    testthat::expect_equal(
        c('visual 1', 'textual', 'speed', 'textual', 'speed', 'speed'),
        facCovTable[['factor2']]
    )
    testthat::expect_equal(c(1, 0.438, 0.476, 1, 0.284, 1), facCovTable[['est']], tolerance = 1e-3)
    testthat::expect_equal(
        c(NA, 0.066, 0.087, NA, 0.072, NA), facCovTable[['se']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(NA, 0.309, 0.306, NA, 0.143, NA), facCovTable[['lower']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(NA, 0.566, 0.646, NA, 0.424, NA), facCovTable[['upper']], tolerance = 1e-3
    )
    testthat::expect_equal(c(NA, 6.683, 5.491, NA, 3.964, NA), facCovTable[['z']], tolerance = 1e-3)
    testthat::expect_equal(c(NA, 0, 0, NA, 0, NA), facCovTable[['p']], tolerance = 1e-3)
    testthat::expect_equal(
        c(NA, 0.438, 0.476, NA, 0.284, NA), facCovTable[['stdEst']], tolerance = 1e-3
    )

    # Test factor intercepts table
    facInterceptTable <- r$factorEst$factorIntercept$asDF
    testthat::expect_equal(c('visual 1', 'textual', 'speed'), facInterceptTable[['factor']])
    testthat::expect_equal(c(1, 1, 1), facInterceptTable[['est']])
    testthat::expect_equal(c(NA, NA, NA), facInterceptTable[['se']])

    # Test residual covariances table
    resCovTable <- r$resEst$resCov$asDF
    testthat::expect_equal(
        c('x1', 'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9'), resCovTable[['var1']]
    )
    testthat::expect_equal(
        c('x1', 'x4', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9'), resCovTable[['var2']]
    )
    testthat::expect_equal(
        c(0.584, 0.08, 1.122, 0.827, 0.383, 0.437, 0.355, 0.803, 0.488, 0.564),
        resCovTable[['est']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.118, 0.043, 0.104, 0.096, 0.048, 0.059, 0.044, 0.088, 0.093, 0.092),
        resCovTable[['se']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.353, -0.004, 0.918, 0.638, 0.288, 0.322, 0.269, 0.63, 0.305, 0.383),
        resCovTable[['lower']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.816, 0.165, 1.327, 1.015, 0.478, 0.552, 0.441, 0.976, 0.67, 0.744),
        resCovTable[['upper']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(4.95, 1.863, 10.772, 8.583, 7.914, 7.472, 8.095, 9.111, 5.242, 6.129),
        resCovTable[['z']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0, 0.062, 0, 0, 0, 0, 0, 0, 0, 0), resCovTable[['p']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.43, 0.17, 0.812, 0.648, 0.286, 0.263, 0.297, 0.679, 0.477, 0.555),
        resCovTable[['stdEst']], tolerance = 1e-3
    )

    # Test residual intercepts table
    resInterceptTable <- r$resEst$resIntercept$asDF
    testthat::expect_equal(
        c('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9'), resInterceptTable[['var']]
    )
    testthat::expect_equal(
        c(4.936, 6.088, 2.25, 3.061, 4.341, 2.186, 4.186, 5.527, 5.374),
        resInterceptTable[['est']], tolerance = 1e-4
    )
    testthat::expect_equal(
        c(0.067, 0.068, 0.065, 0.067, 0.074, 0.063, 0.063, 0.058, 0.058),
        resInterceptTable[['se']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(4.804, 5.955, 2.123, 2.93, 4.195, 2.062, 4.063, 5.413, 5.26),
        resInterceptTable[['lower']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(5.067, 6.221, 2.378, 3.192, 4.486, 2.309, 4.309, 5.641, 5.488),
        resInterceptTable[['upper']],
        tolerance = 1e-4
    )
    testthat::expect_equal(
        c(73.494, 89.855, 34.579, 45.912, 58.452, 34.667, 66.766, 94.854, 92.546),
        resInterceptTable[['z']],
        tolerance = 1e-5
    )
    testthat::expect_equal(c(0, 0, 0, 0, 0, 0, 0, 0, 0), resInterceptTable[['p']], tolerance = 1e-3)
    testthat::expect_equal(
        c(4.236, 5.179, 1.993, 2.646, 3.369, 1.998, 3.848, 5.467, 5.334),
        resInterceptTable[['stdEst']],
        tolerance = 1e-3
    )

    # Test exact model fit table
    exactFitTable <- r$modelFit$test$asDF
    testthat::expect_equal(81.69, exactFitTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(23, exactFitTable[['df']], tolerance = 1e-3)
    testthat::expect_equal(0, exactFitTable[['p']], tolerance = 1e-3)

    # Test fit measures table
    fitMeasuresTable <- r$modelFit$fitMeasures$asDF
    testthat::expect_equal(0.934, fitMeasuresTable[['cfi']], tolerance = 1e-3)
    testthat::expect_equal(0.896, fitMeasuresTable[['tli']], tolerance = 1e-3)
    testthat::expect_equal(0.059, fitMeasuresTable[['srmr']], tolerance = 1e-3)
    testthat::expect_equal(0.092, fitMeasuresTable[['rmsea']], tolerance = 1e-3)
    testthat::expect_equal(0.071, fitMeasuresTable[['rmseaLower']], tolerance = 1e-3)
    testthat::expect_equal(0.114, fitMeasuresTable[['rmseaUpper']], tolerance = 1e-3)
    testthat::expect_equal(7533.875, fitMeasuresTable[['aic']], tolerance = 1e-7)
    testthat::expect_equal(7648.795, fitMeasuresTable[['bic']], tolerance = 1e-7)

    # Test residuals observed correlation matrix table
    corResTable <- r$modelPerformance$corRes$asDF
    testthat::expect_equal(
        c('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9'), corResTable[['var']],
    )
    testthat::expect_equal(c(NA, NA, NA, NA, NA, NA, NA, NA, NA), corResTable[['XeDE']])
    testthat::expect_equal(
        c(-0.03, NA, NA, NA, NA, NA, NA, NA, NA), corResTable[['XeDI']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(-0.007, 0.083, NA, NA, NA, NA, NA, NA, NA), corResTable[['XeDM']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.034, -0.007, -0.061, NA, NA, NA, NA, NA, NA), corResTable[['XeDQ']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.01, -0.023, -0.146, 0.008, NA, NA, NA, NA, NA), corResTable[['XeDU']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.08, 0.033, -0.02, -0.004, 0, NA, NA, NA, NA),
        corResTable[['XeDY']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(-0.137, -0.193, -0.088, 0.038, -0.036, -0.014, NA, NA, NA),
        corResTable[['XeDc']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(-0.036, -0.057, -0.018, -0.066, -0.037, -0.022, 0.077, NA, NA),
        corResTable[['XeDg']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.151, 0.068, 0.14, 0.048, 0.065, 0.055, -0.037, -0.033, NA),
        corResTable[['XeDk']],
        tolerance = 1e-3
    )

    # Test factor loadings modifcation indices table
    modIndFactor <- r$modelPerformance$modIndices$factorLoadingsMod$asDF
    testthat::expect_equal(
        c('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9'), modIndFactor[['var']]
    )
    testthat::expect_equal(
        c(NA, NA, NA, 0.015, 5.582, 5.391, 20.642, 3.846, 37.237),
        modIndFactor[['XdmlzdWFsIDE']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(7.347, 0, 7.904, NA, NA, NA, 0.06, 3.236, 4.372),
        modIndFactor[['XdGV4dHVhbA']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.169, 1.938, 0.436, 0.018, 0.194, 0.35, NA, NA, NA),
        modIndFactor[['Xc3BlZWQ']], tolerance = 1e-3
    )

    # Test residual covariances modifcation indices table
    modIndResCov <- r$modelPerformance$modIndices$resCovMod$asDF
    testthat::expect_equal(
        c('x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9'), modIndResCov[['var']]
    )
    testthat::expect_equal(c(NA, NA, NA, NA, NA, NA, NA, NA, NA), modIndResCov[['XeDE']])
    testthat::expect_equal(
        c(3.024, NA, NA, NA, NA, NA, NA, NA, NA), modIndResCov[['XeDI']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.945, 7.531, NA, NA, NA, NA, NA, NA, NA), modIndResCov[['XeDM']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(NA, 0.03, 0.144, NA, NA, NA, NA, NA, NA), modIndResCov[['XeDQ']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.032, 0.129, 9.958, 4.484, NA, NA, NA, NA, NA), modIndResCov[['XeDU']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1.567, 0.416, 1.213, 4.462, 0.004, NA, NA, NA, NA),
        modIndResCov[['XeDY']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(6.489, 8.721, 0.509, 7.518, 1.593, 0.405, NA, NA, NA),
        modIndResCov[['XeDc']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.285, 0.119, 0.162, 2.989, 0.22, 0.148, 34.951, NA, NA),
        modIndResCov[['XeDg']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(7.887, 1.68, 3.811, 0.302, 1.16, 0.075, 4.881, 16.305, NA),
        modIndResCov[['XeDk']], tolerance = 1e-3
    )
})

testthat::test_that('Human readable error message is thrown when model does not converge', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    nVars <- 20
    nCases <- 10

    data <- list()
    for (i in 1:nVars)
        data[[paste0("w",i)]] <- rnorm(nCases)
    data <- as.data.frame(data)

    factors <- list(list(label='factor_1', vars=paste0("w", 1:nVars)))

    testthat::expect_error(
        jmv::cfa(data = data, factors = factors, resCov = NULL, stdEst = TRUE, corRes = TRUE),
        class = exceptions$modelError
    )
})
