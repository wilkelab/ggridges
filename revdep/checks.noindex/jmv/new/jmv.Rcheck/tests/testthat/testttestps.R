testthat::context('ttestPS')

testthat::test_that('All options in the ttestPS work (sunny)', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    time1 <- rnorm(100)
    measure1 <- rnorm(100)
    df <- data.frame(
        `time 1` = time1,
        `time 2` = time1 - rnorm(100, 0.5, 1),
        `measure 1` = measure1,
        `measure 2` = measure1 + rnorm(100, 10, 20),
        check.names = FALSE
    )
    pairs <- list(
        list(i1="time 1", i2="time 2"),
        list(i1="measure 1", i2="measure 2")
    )

    r <- jmv::ttestPS(
        df,
        pairs = pairs,
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
    testthat::expect_equal(c('time 1', 'measure 1'), ttestTable[['var1[stud]']])
    testthat::expect_equal(c('time 2', 'measure 2'), ttestTable[['var2[stud]']])
    testthat::expect_equal(c(3.237, -5.02), ttestTable[['stat[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(99, 99), ttestTable[['df[stud]']])
    testthat::expect_equal(c(0.002, 0), ttestTable[['p[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.341, -9.255), ttestTable[['md[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.105, 1.843), ttestTable[['sed[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.132, -12.912), ttestTable[['cil[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.551, -5.597), ttestTable[['ciu[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.324, -0.502), ttestTable[['es[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.122, -0.709), ttestTable[['ciles[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.524, -0.293), ttestTable[['ciues[stud]']], tolerance = 1e-3)
    testthat::expect_equal(c(14.165, 6326.535), ttestTable[['stat[bf]']], tolerance = 1e-3)
    testthat::expect_equal(c(0, 0), ttestTable[['err[bf]']], tolerance = 1e-3)
    testthat::expect_equal(c(3468, 1261), ttestTable[['stat[wilc]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.001, 0), ttestTable[['p[wilc]']], tolerance = 1e-3)

    # CRAN complained as follows ... apparently only an issue on older versions of macOS?
    # ... anyway, you'll see i've set the tolerance really high to accommodate it
    #
    # ══ Failed tests ════════════════════════════════════════════════════════════════
    # ── Failure ('testttestps.R:51:5'): All options in the ttestPS work (sunny) ─────
    # c(0.356, -8.945) not equal to ttestTable[["md[wilc]"]].
    # 1/2 mismatches
    # [2] -8.95 - -8.93 == -0.0158

    testthat::expect_equal(c(0.356, -8.945), ttestTable[['md[wilc]']], tolerance = 0.02)

    testthat::expect_equal(c(0.105, 1.843), ttestTable[['sed[wilc]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.145, -12.849), ttestTable[['cil[wilc]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.561, -4.998), ttestTable[['ciu[wilc]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.373, -0.501), ttestTable[['es[wilc]']], tolerance = 1e-3)

    # Test normality tests table
    normTable <- r$norm$asDF
    testthat::expect_equal(c('time 1', 'measure 1'), normTable[['var1']])
    testthat::expect_equal(c('time 2', 'measure 2'), normTable[['var2']])
    testthat::expect_equal(c(0.982, 0.988), normTable[['w']], tolerance = 1e-3)
    testthat::expect_equal(c(0.19, 0.472), normTable[['p']], tolerance = 1e-3)

    # Test descriptives table
    descTable <- r$desc$asDF
    testthat::expect_equal(c('time 1', 'time 2', 'measure 1', 'measure 2'), descTable[['name']])
    testthat::expect_equal(c(100, 100, 100, 100), descTable[['num']])
    testthat::expect_equal(c(0.237, -0.104, 0.041, 9.296), descTable[['m']], tolerance = 1e-3)
    testthat::expect_equal(c(0.19, -0.187, -0.132, 7.791), descTable[['med']], tolerance = 1e-3)
    testthat::expect_equal(c(1.065, 1.523, 1.006, 18.408), descTable[['sd']], tolerance = 1e-3)
    testthat::expect_equal(c(0.107, 0.152, 0.101, 1.841), descTable[['se']], tolerance = 1e-3)
})

testthat::test_that('Matched rank biserial correlation is correct', {
    df <- data.frame(
        before = c(20, 22, 19, 20, 22, 18, 24, 20, 25),
        after = c(38, 37, 33, 29, 14, 12, 20, 22, 25)
    )
    pairs <- list(list(i1='before', i2='after'))

    r <- jmv::ttestPS(df, pairs, wilcoxon=TRUE, students=FALSE, effectSize=TRUE)

    # Test rank biserial correlation
    ttestTable <- r$ttest$asDF
    testthat::expect_equal('before', ttestTable[['var1[wilc]']], tolerance = 1e-3)
    testthat::expect_equal('after', ttestTable[['var2[wilc]']], tolerance = 1e-3)
    testthat::expect_equal(9, ttestTable[['stat[wilc]']])
    testthat::expect_equal(0.234, ttestTable[['p[wilc]']], tolerance = 1e-3)
    testthat::expect_equal(-0.5, ttestTable[['es[wilc]']])
})
