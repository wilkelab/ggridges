testthat::context('proptest2')

testthat::test_that('All options in the propTest2 work (sunny)', {
    df <- data.frame(
        `x 1` = c(rep("A", 8), rep("B", 14)),
        `x 2` = c(rep(0, 12), rep(1, 10)),
        check.names = FALSE
    )

    r <- jmv::propTest2(
        df,
        vars = c("x 1", "x 2"),
        testValue = 0.6,
        ci = TRUE,
        bf = TRUE,
        ciBayes = TRUE
    )

    # Test main table
    mainTable <- r$table$asDF
    testthat::expect_equal(c('x 1', 'x 1', 'x 2', 'x 2'), mainTable[['var']])
    testthat::expect_equal(c('A', 'B', '0', '1'), mainTable[['level']])
    testthat::expect_equal(c(8, 14, 12, 10), mainTable[['count']])
    testthat::expect_equal(c(22, 22, 22, 22), mainTable[['total']])
    testthat::expect_equal(c(0.364, 0.636, 0.545, 0.455), mainTable[['prop']], tolerance = 1e-3)
    testthat::expect_equal(c(0.029, 0.83, 0.665, 0.193), mainTable[['p']], tolerance = 1e-3)
    testthat::expect_equal(c(0.172, 0.407, 0.322, 0.244), mainTable[['cil']], tolerance = 1e-3)
    testthat::expect_equal(c(0.593, 0.828, 0.756, 0.678), mainTable[['ciu']], tolerance = 1e-3)
    testthat::expect_equal(c(3.016, 0.265, 0.295, 0.663), mainTable[['bf']], tolerance = 1e-3)
    testthat::expect_equal(c(0.197, 0.427, 0.345, 0.268), mainTable[['cilBayes']], tolerance = 1e-3)
    testthat::expect_equal(c(0.573, 0.803, 0.732, 0.655), mainTable[['ciuBayes']], tolerance = 1e-3)
})

testthat::test_that('All options in the propTest2 work with counts', {
    df <- data.frame(
        `x 1` = c(8, 14),
        `x 2` = c(12, 10),
        check.names = FALSE
    )

    r <- jmv::propTest2(
        df,
        vars = c("x 1", "x 2"),
        areCounts = TRUE,
        testValue = 0.6,
        ci = TRUE,
        bf = TRUE,
        ciBayes = TRUE
    )

    # Test main table
    mainTable <- r$table$asDF
    testthat::expect_equal(c('x 1', 'x 1', 'x 2', 'x 2'), mainTable[['var']])
    testthat::expect_equal(c('1', '2', '1', '2'), mainTable[['level']])
    testthat::expect_equal(c(8, 14, 12, 10), mainTable[['count']])
    testthat::expect_equal(c(22, 22, 22, 22), mainTable[['total']])
    testthat::expect_equal(c(0.364, 0.636, 0.545, 0.455), mainTable[['prop']], tolerance = 1e-3)
    testthat::expect_equal(c(0.029, 0.83, 0.665, 0.193), mainTable[['p']], tolerance = 1e-3)
    testthat::expect_equal(c(0.172, 0.407, 0.322, 0.244), mainTable[['cil']], tolerance = 1e-3)
    testthat::expect_equal(c(0.593, 0.828, 0.756, 0.678), mainTable[['ciu']], tolerance = 1e-3)
    testthat::expect_equal(c(3.016, 0.265, 0.295, 0.663), mainTable[['bf']], tolerance = 1e-3)
    testthat::expect_equal(c(0.197, 0.427, 0.345, 0.268), mainTable[['cilBayes']], tolerance = 1e-3)
    testthat::expect_equal(c(0.573, 0.803, 0.732, 0.655), mainTable[['ciuBayes']], tolerance = 1e-3)
})
