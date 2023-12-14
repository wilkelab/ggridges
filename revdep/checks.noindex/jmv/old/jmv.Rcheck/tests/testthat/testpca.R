testthat::context('pca')

testthat::test_that('All options in the pca work (sunny)', {
    data <- lavaan::HolzingerSwineford1939
    names(data)[7] <- "x 1"

    r <- jmv::pca(
        data = data,
        vars = c("x 1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9"),
        nFactorMethod = "fixed",
        nFactors = 3,
        hideLoadings = 0,
        sortLoadings = TRUE,
        eigen = TRUE,
        factorCor = TRUE,
        factorSummary = TRUE,
        kmo = TRUE,
        bartlett = TRUE
    )

    # Test factor loadings table
    loadingsTable <- r$loadings$asDF
    testthat::expect_equal(
        c('x5', 'x4', 'x6', 'x3', 'x2', 'x 1', 'x7', 'x8', 'x9'), loadingsTable[['name']]
    )
    testthat::expect_equal(
        c(0.903, 0.889, 0.869, 0.018, 0.083, 0.321, 0.098, 0.042, 0.13),
        loadingsTable[['pc1']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.06, 0.124, 0.178, 0.779, 0.727, 0.673, -0.153, 0.145, 0.435),
        loadingsTable[['pc2']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.077, 0.091, 0.076, 0.155, -0.102, 0.175, 0.83, 0.818, 0.636),
        loadingsTable[['pc3']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.175, 0.186, 0.207, 0.369, 0.455, 0.413, 0.278, 0.308, 0.389),
        loadingsTable[['uniq']],
        tolerance = 1e-3
    )

    # Test factor summary table
    summaryTable <- r$factorStats$factorSummary$asDF
    testthat::expect_equal(c('1', '2', '3'), summaryTable[['comp']])
    testthat::expect_equal(c(2.501, 1.872, 1.847), summaryTable[['loadings']], tolerance = 1e-3)
    testthat::expect_equal(c(27.784, 20.802, 20.527), summaryTable[['varProp']], tolerance = 1e-3)
    testthat::expect_equal(c(27.784, 48.587, 69.114), summaryTable[['varCum']], tolerance = 1e-3)

    # Test inter-factor correlations table
    factorCorTable <- r$factorStats$factorCor#a
    testthat::expect_equal(0.000, factorCorTable$getCell(rowNo=1, "pc2")$value, tolerance = 1e-3)
    testthat::expect_equal(0.000, factorCorTable$getCell(rowNo=1, "pc3")$value, tolerance = 1e-3)
    testthat::expect_equal(0.000, factorCorTable$getCell(rowNo=2, "pc3")$value, tolerance = 1e-3)

    # Test sphericity test table
    spherTable <- r$assump$bartlett$asDF
    testthat::expect_equal(904.097, spherTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(36, spherTable[['df']])
    testthat::expect_equal(0, spherTable[['p']])

    # Test KMO table
    kmoTable <- r$assump$kmo$asDF
    testthat::expect_equal(
        c('Overall', 'x 1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9'), kmoTable[['name']]
    )
    testthat::expect_equal(
        c(0.752, 0.805, 0.778, 0.734, 0.763, 0.739, 0.808, 0.593, 0.683, 0.788),
        kmoTable[['msa']],
        tolerance = 1e-3
    )

    # Test eigenvalues table
    eigenTable <- r$eigen$initEigen$asDF
    testthat::expect_equal(
        c('1', '2', '3', '4', '5', '6', '7', '8', '9'), eigenTable[['comp']])
    testthat::expect_equal(
        c(3.216, 1.639, 1.365, 0.699, 0.584, 0.5, 0.473, 0.286, 0.238),
        eigenTable[['eigen']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(35.737, 18.208, 15.168, 7.766, 6.493, 5.552, 5.257, 3.178, 2.641),
        eigenTable[['varProp']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(35.737, 53.945, 69.114, 76.879, 83.372, 88.924, 94.181, 97.359, 100),
        eigenTable[['varCum']],
        tolerance = 1e-3
    )
})

testthat::test_that('Error is thrown when n components > n variables', {
    df <- data.frame(
        "x1" = rnorm(10),
        "x2" = rnorm(10)
    )

    testthat::expect_error(
        jmv::pca(
            data = df,
            vars = c("x1", "x2"),
            nFactorMethod = "fixed",
            nFactors = 3
        ),
        'Number of components cannot be bigger than number of variables',
        fixed=TRUE
    )
})

testthat::test_that('pca works when simulated loadings in parallel analysis > model loadings', {
    data <- data.frame(
        a = c(2, 1, 5, 2, 3, 4, 4, 1, 3, 5),
        b = c(4, 5, 3, 2, 5, 3, 4, 2, 3, 5),
        c = c(5, 1, 4, 3, 5, 2, 3, 1, 2, 3)
    )

    r <- jmv::pca(data = data, vars = vars(a, b, c))

    testthat::expect_equal(r$loadings$asDF[1, 2], 0.7167, tolerance = 1e-4)
})
