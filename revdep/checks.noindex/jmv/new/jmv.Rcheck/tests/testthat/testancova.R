testthat::context('ancova')

testthat::test_that('All basic options in the ancova work (sunny)', {
    df <- data.frame(
        `dep 1` = c(0, 4, 19, 5, 9, 15, 1, 4, 19, 10, 13, 7, 5, 12, 2, 23, 6, 13),
        `factor 1` = rep(letters[1:3], each=6),
        `factor 2` = rep(LETTERS[1:2], length.out=18),
        `cov 1` = c(8, 51, 2, 74, 1, 91, 5, 25, 1, 59, 5, 32, 7, 9, 2, 54, 21, 12),
        check.names = FALSE,
        stringsAsFactors = TRUE
    )

    r <- jmv::ancova(
        df,
        dep='dep 1',
        factors=c('factor 1', 'factor 2'),
        covs='cov 1',
        modelTest = TRUE,
        effectSize = c('eta', 'partEta', 'omega')
    )

    # Test main anova table
    mainTable <- r$main$asDF
    testthat::expect_equal(
        c('Overall model', 'factor 1', 'factor 2', 'cov 1', 'factor 1:factor 2', 'Residuals'),
        mainTable[['name']]
    )
    testthat::expect_equal(
        c(527.821, 62.06, 39.788, 108.723, 317.25, 414.61), mainTable[['ss']], tolerance = 1e-3
    )
    testthat::expect_equal(c(6, 2, 1, 1, 2, 11), mainTable[['df']])
    testthat::expect_equal(
        c(87.97, 31.03, 39.788, 108.723, 158.625, 37.692), mainTable[['ms']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1.534, 0.823, 1.056, 2.885, 4.208, NA), mainTable[['F']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.255, 0.464, 0.326, 0.118, 0.044, NA), mainTable[['p']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(NA, 0.066, 0.042, 0.115, 0.337, NA), mainTable[['etaSq']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(NA, 0.13, 0.088, 0.208, 0.433, NA), mainTable[['etaSqP']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(NA, -0.014, 0.002, 0.072, 0.247, NA), mainTable[['omegaSq']], tolerance = 1e-3
    )
})
