testthat::context('anovarmnp')

testthat::test_that('All options in the anovaRMNP work (sunny)', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    data <- data.frame(
        x1 = sample(1:10, 20, replace=TRUE),
        x2 = sample(1:10, 20, replace=TRUE),
        x3 = sample(1:10, 20, replace=TRUE),
        x4 = sample(1:10, 20, replace=TRUE)
    )

    r <- jmv::anovaRMNP(
        data,
        measures = c('x1', 'x2', 'x3', 'x4'),
        desc = TRUE,
        pairs = TRUE
    )

    # Test main table
    mainTable <- r$table$asDF
    testthat::expect_equal(2.198, mainTable[['stat']], tolerance = 1e-3)
    testthat::expect_equal(3, mainTable[['df']])
    testthat::expect_equal(0.532, mainTable[['p']], tolerance = 1e-3)


    # Test pairwise comparisons
    pairsTable <- r$comp$asDF
    testthat::expect_equal(c('x1', 'x1', 'x1', 'x2', 'x2', 'x3'), pairsTable[['i1']])
    testthat::expect_equal(c('x2', 'x3', 'x4', 'x3', 'x4', 'x4'), pairsTable[['i2']])
    testthat::expect_equal(
        c(0.88, 0.063, 0.566, 0.943, 1.446, 0.503), pairsTable[['stat']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.382, 0.95, 0.574, 0.349, 0.154, 0.617), pairsTable[['p']], tolerance = 1e-3
    )

    # Test descriptives table
    descTable <-r$desc$asDF
    testthat::expect_equal(c('x1', 'x2', 'x3', 'x4'), descTable[['level']])
    testthat::expect_equal(as.vector(sapply(data, mean)), descTable[['mean']], tolerance = 1e-3)
    testthat::expect_equal(as.vector(sapply(data, median)), descTable[['median']], tolerance = 1e-3)
})
