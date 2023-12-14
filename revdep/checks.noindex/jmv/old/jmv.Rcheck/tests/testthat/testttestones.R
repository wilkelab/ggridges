testthat::context('ttestOneS')

testthat::test_that('All options in the ttestOneS work (sunny)', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    df <- data.frame(
        `dep 1` = rnorm(100, 0, 1),
        `dep 2` = rnorm(100, 2, 0.1),
        `dep 3` = rnorm(100, 10, 5),
        check.names = FALSE
    )

    r <- jmv::ttestOneS(
        df,
        vars = c("dep 1", "dep 2", "dep 3"),
        bf = TRUE,
        wilcoxon = TRUE,
        norm = TRUE,
        meanDiff = TRUE,
        ci = TRUE,
        effectSize = TRUE,
        ciES = TRUE,
        desc = TRUE
    )

    # Test main t-test table
    ttestTable <- r$ttest$asDF
    testthat::expect_equal(c('dep 1', 'dep 2', 'dep 3'), ttestTable[['var[stud]']])
    testthat::expect_equal(c(2.225, 199.13, 17.46), ttestTable[['stat[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(99, 99, 99), ttestTable[['df[stud]']])
    testthat::expect_equal(c(0.028, 0, 0), ttestTable[['p[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.237, 2.004, 9.207), ttestTable[['md[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.026, 1.984, 8.16), ttestTable[['cil[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.448, 2.024, 10.253), ttestTable[['ciu[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.223, 19.913, 1.746), ttestTable[['es[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.024, 17.082, 1.432), ttestTable[['ciles[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.42, 22.643, 2.056), ttestTable[['ciues[stud]']], tolerance = 1e-3)
    testthat::expect_equal(
        c(1.163, 2.203e+126, 5.448e+28), ttestTable[['stat[bf]']], tolerance = 1e-3
    )
    testthat::expect_equal(c(0, 0, 0), ttestTable[['err[bf]']], tolerance = 1e-3)
    testthat::expect_equal(c(3117, 5050, 5016), ttestTable[['stat[wilc]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.042, 0, 0), ttestTable[['p[wilc]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.233, 1.998, 9.281), ttestTable[['md[wilc]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.006, 1.979, 8.226), ttestTable[['cil[wilc]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.459, 2.019, 10.307), ttestTable[['ciu[wilc]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.234, 1, 0.987), ttestTable[['es[wilc]']], tolerance = 1e-3)

    # Test normality tests table
    normTable <- r$normality$asDF
    testthat::expect_equal(c('dep 1', 'dep 2', 'dep 3'), normTable[['name']])
    testthat::expect_equal(c(0.992, 0.973, 0.982), normTable[['w']], tolerance = 1e-3)
    testthat::expect_equal(c(0.849, 0.039, 0.19), normTable[['p']], tolerance = 1e-3)

    # Test descriptives table
    descTable <- r$descriptives$asDF
    testthat::expect_equal(c('dep 1', 'dep 2', 'dep 3'), descTable[['name']])
    testthat::expect_equal(c(100, 100, 100), descTable[['num']])
    testthat::expect_equal(c(0.237, 2.004, 9.207), descTable[['mean']], tolerance = 1e-3)
    testthat::expect_equal(c(0.19, 1.987, 9.819), descTable[['median']], tolerance = 1e-3)
    testthat::expect_equal(c(1.065, 0.101, 5.273), descTable[['sd']], tolerance = 1e-3)
    testthat::expect_equal(c(0.107, 0.01, 0.527), descTable[['se']], tolerance = 1e-3)
})

testthat::test_that('Matched rank biserial correlation is correct', {
    df <- data.frame(
        before = c(20, 22, 19, 20, 22, 18, 24, 20, 25),
        after = c(38, 37, 33, 29, 14, 12, 20, 22, 25)
    )
    df$dif <- df$after - df$before

    r <- jmv::ttestOneS(df, vars = "dif", wilcoxon=TRUE, students=FALSE, effectSize=TRUE)

    # Test rank biserial correlation
    ttestTable <- r$ttest$asDF
    testthat::expect_equal('dif', ttestTable[['var[wilc]']])
    testthat::expect_equal(27, ttestTable[['stat[wilc]']])
    testthat::expect_equal(0.234, ttestTable[['p[wilc]']], tolerance = 1e-3)
    testthat::expect_equal(0.5, ttestTable[['es[wilc]']])
})

testthat::test_that('Matched rank biserial correlation works with non zero test value', {
    df <- data.frame(x = c(1, 5, 3, 4, 4, 2, 3, 2, 1, 4, 5, 4, 3, 1))

    r <- jmv::ttestOneS(
        df, testValue=3, vars="x", hypothesis="gt", wilcoxon=TRUE, students=FALSE, effectSize=TRUE
    )

    # Test rank biserial correlation
    ttestTable <- r$ttest$asDF
    testthat::expect_equal(-0.0303, ttestTable[['es[wilc]']], tolerance = 1e-4)
})
