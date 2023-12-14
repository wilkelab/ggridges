testthat::context('proptestn')

data('HairEyeColor')

testthat::test_that('All options in the propTestN work (sunny)', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    df <- data.frame(
        `x 1` = sample(letters[1:4], size = 100, replace = TRUE, prob = c(3, 1, 1, 1)),
        check.names = FALSE
    )

    r <- jmv::propTestN(
        df,
        var = "x 1",
        expected = TRUE,
        ratio = c(3, 1, 1, 1)
    )

    # Test proportions table
    propsTable <- r$props$asDF
    testthat::expect_equal(c('a', 'b', 'c', 'd'), propsTable[['level']])
    testthat::expect_equal(c(45, 23, 16, 16), propsTable[['count[obs]']])
    testthat::expect_equal(c(0.45, 0.23, 0.16, 0.16), propsTable[['prop[obs]']], tolerance = 1e-3)
    testthat::expect_equal(
        c(50, 16.667, 16.667, 16.667), propsTable[['count[exp]']], tolerance = 1e-3
    )
    testthat::expect_equal(c(0.5, 0.167, 0.167, 0.167), propsTable[['prop[exp]']], tolerance = 1e-3)

    # Test test statistics table
    testTable <- r$tests$asDF
    testthat::expect_equal(2.96, testTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(3, testTable[['df']])
    testthat::expect_equal(0.398, testTable[['p']], tolerance = 1e-3)
})

testthat::test_that('All options in the propTestN work with counts', {
    df <- as.data.frame(HairEyeColor)

    r <- jmv::propTestN(
        df,
        var = 'Eye',
        counts = 'Freq',
        expected = TRUE,
        ratio = c(2,1,1,1)
    )

    # Test proportions table
    propsTable <- r$props$asDF
    testthat::expect_equal(c('Brown', 'Blue', 'Hazel', 'Green'), propsTable[['level']])
    testthat::expect_equal(c(220, 215, 93, 64), propsTable[['count[obs]']])
    testthat::expect_equal(
        c(0.372, 0.363, 0.157, 0.108), propsTable[['prop[obs]']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(236.8, 118.4, 118.4, 118.4), propsTable[['count[exp]']], tolerance = 1e-3
    )
    testthat::expect_equal(c(0.4, 0.2, 0.2, 0.2), propsTable[['prop[exp]']], tolerance = 1e-3)

    # Test test statistics table
    testTable <- r$tests$asDF
    testthat::expect_equal(110.449, testTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(3, testTable[['df']])
    testthat::expect_equal(0, testTable[['p']], tolerance = 1e-3)
})
