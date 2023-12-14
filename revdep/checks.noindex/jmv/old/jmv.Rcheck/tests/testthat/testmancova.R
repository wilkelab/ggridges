testthat::context('mancova')

testthat::test_that('All options in the mancova work (sunny)', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(100)

    df <- data.frame(
        `dep 1` = rnorm(100, 0, 1),
        `dep 2` = rnorm(100, 0, 0.1),
        `dep 3` = rnorm(100, 1, 1),
        `dep 4` = rnorm(100, 1, 0.1),
        `cov 1` = rnorm(100),
        `factor 1` = sample(LETTERS[20:21], 100, replace = TRUE),
        `factor 2` = sample(letters[1:3], 100, replace = TRUE),
        check.names = FALSE,
        stringsAsFactors = TRUE
    )

    r <- jmv::mancova(
        data = df,
        deps = c("dep 1","dep 2","dep 3","dep 4"),
        factors = c("factor 1","factor 2"),
        covs ="cov 1",
        boxM = TRUE,
        shapiro = TRUE
    )

    # Test multivariate table
    multivarTable <- r$multivar$asDF
    testthat::expect_equal(
        c('factor 1', 'factor 2', 'factor 1:factor 2', 'cov 1'), multivarTable[['term[pillai]']]
    )
    testthat::expect_equal(
        c('Pillai\'s Trace', 'Pillai\'s Trace', 'Pillai\'s Trace', 'Pillai\'s Trace'),
        multivarTable[['test[pillai]']]
    )
    testthat::expect_equal(
        c(0.069, 0.067, 0.106, 0.041), multivarTable[['stat[pillai]']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1.677, 0.794, 1.276, 0.973), multivarTable[['f[pillai]']], tolerance = 1e-3
    )
    testthat::expect_equal(c(4, 8, 8, 4), multivarTable[['df1[pillai]']])
    testthat::expect_equal(c(90, 182, 182, 90), multivarTable[['df2[pillai]']])
    testthat::expect_equal(
        c(0.162, 0.609, 0.259, 0.426), multivarTable[['p[pillai]']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c('factor 1', 'factor 2', 'factor 1:factor 2', 'cov 1'), multivarTable[['term[wilks]']]
    )
    testthat::expect_equal(
        c('Wilks\' Lambda', 'Wilks\' Lambda', 'Wilks\' Lambda', 'Wilks\' Lambda'),
        multivarTable[['test[wilks]']]
    )
    testthat::expect_equal(
        c(0.931, 0.934, 0.896, 0.959), multivarTable[['stat[wilks]']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1.677, 0.785, 1.27, 0.973), multivarTable[['f[wilks]']], tolerance = 1e-3
    )
    testthat::expect_equal(c(4, 8, 8, 4), multivarTable[['df1[wilks]']])
    testthat::expect_equal(c(90, 180, 180, 90), multivarTable[['df2[wilks]']])
    testthat::expect_equal(
        c(0.162, 0.616, 0.261, 0.426), multivarTable[['p[wilks]']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c('factor 1', 'factor 2', 'factor 1:factor 2', 'cov 1'), multivarTable[['term[hotel]']]
    )
    testthat::expect_equal(
        c('Hotelling\'s Trace', 'Hotelling\'s Trace', 'Hotelling\'s Trace', 'Hotelling\'s Trace'),
        multivarTable[['test[hotel]']]
    )
    testthat::expect_equal(
        c(0.075, 0.07, 0.114, 0.043), multivarTable[['stat[hotel]']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1.677, 0.776, 1.265, 0.973), multivarTable[['f[hotel]']], tolerance = 1e-3
    )
    testthat::expect_equal(c(4, 8, 8, 4), multivarTable[['df1[hotel]']])
    testthat::expect_equal(c(90, 178, 178, 90), multivarTable[['df2[hotel]']])
    testthat::expect_equal(
        c(0.162, 0.624, 0.264, 0.426), multivarTable[['p[hotel]']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c('factor 1', 'factor 2', 'factor 1:factor 2', 'cov 1'), multivarTable[['term[roy]']]
    )
    testthat::expect_equal(
        c('Roy\'s Largest Root', 'Roy\'s Largest Root', 'Roy\'s Largest Root', 'Roy\'s Largest Root'),
        multivarTable[['test[roy]']]
    )
    testthat::expect_equal(
        c(0.075, 0.038, 0.086, 0.043), multivarTable[['stat[roy]']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1.677, 0.874, 1.947, 0.973), multivarTable[['f[roy]']], tolerance = 1e-3
    )
    testthat::expect_equal(c(4, 4, 4, 4), multivarTable[['df1[roy]']])
    testthat::expect_equal(c(90, 91, 91, 90), multivarTable[['df2[roy]']])
    testthat::expect_equal(
        c(0.162, 0.483, 0.109, 0.426), multivarTable[['p[roy]']], tolerance = 1e-3
    )

    # Test univariate table
    univarTable <- r$univar$asDF
    testthat::expect_equal(
        c(
            4.64, 0.011, 0.165, 0, 3.101, 0.008, 0.274, 0.015, 0.767, 0.045, 1.156, 0.013, 0.404,
            0.004, 2.752, 0.012, 94.232, 0.559, 102.583, 1.116
        ),
        univarTable[['ss']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 93, 93, 93, 93), univarTable[['df']]
    )
    testthat::expect_equal(
        c(
            4.64, 0.011, 0.165, 0, 1.55, 0.004, 0.137, 0.008, 0.383, 0.023, 0.578, 0.007, 0.404,
            0.004, 2.752, 0.012, 1.013, 0.006, 1.103, 0.012
        ),
        univarTable[['ms']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(
            4.579, 1.799, 0.15, 0.001, 1.53, 0.694, 0.124, 0.644, 0.378, 3.774, 0.524, 0.547, 0.399,
            0.603, 2.495, 1.01, NA, NA, NA, NA
        ),
        univarTable[['F']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(
            0.035, 0.183, 0.7, 0.977, 0.222, 0.502, 0.883, 0.528, 0.686, 0.027, 0.594, 0.58, 0.529,
            0.439, 0.118, 0.318, NA, NA, NA, NA
        ),
        univarTable[['p']],
        tolerance = 1e-3
    )

    # Test Box's M test table
    boxMTable <- r$assump$boxM$asDF
    testthat::expect_equal(38.74, boxMTable[['chi']], tolerance = 1e-3)
    testthat::expect_equal(50, boxMTable[['df']], tolerance = 1e-3)
    testthat::expect_equal(0.876, boxMTable[['p']], tolerance = 1e-3)

    # Test Shapiro-Wilk test table
    shapiro <- r$assump$shapiro$asDF
    testthat::expect_equal(0.978, shapiro[['w']], tolerance = 1e-3)
    testthat::expect_equal(0.097, shapiro[['p']], tolerance = 1e-3)
})

testthat::test_that('Provide error message when dependent variables are highly correlated', {
    df <- data.frame(
        dep1 = 1:10,
        dep2 = 1:10,
        factor = rep(letters[1:2], length.out=10),
        stringsAsFactors = TRUE
    )

    testthat::expect_error(
        jmv::mancova(data = df, deps = c("dep1","dep2"), factors = c("factor")),
        "Dependent variables are very highly correlated"
    )
})

testthat::test_that('Provide error message if residual degrees of freedom are equal to 0', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    df <- data.frame(
        dep1 = rnorm(22),
        dep2 = rnorm(22),
        factor1 = rep(letters[1:10], length.out=22),
        factor2 = rep(LETTERS[1:3], length.out=22),
        stringsAsFactors = TRUE
    )

    testthat::expect_error(
        jmv::mancova(data = df, deps = c("dep1","dep2"), factors = c("factor1", "factor2")),
        "Not enough degrees of freedom to estimate all the model effects"
    )
})
