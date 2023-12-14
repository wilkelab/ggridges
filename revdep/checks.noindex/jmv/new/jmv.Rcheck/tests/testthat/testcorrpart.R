testthat::context('corrpart')

testthat::test_that('All options in the corrPart work for partial correlation (sunny)', {
    df <- data.frame(
        `var 1` = c(8, 51, 2, 74, 1, 91, 5, 25, 1, 59, 5, 32, 7),
        `var 2` = c(2, NA, NaN, 3, -1, -2, 1, 1, -2, 2, -2, -3, 3),
        `var 3` = c(0, 4, 19, 5, 9, 15, 1, 4, 19, 10, 13, 7, 5),
        check.names = FALSE
    )

    r <- jmv::corrPart(
        data = df,
        vars = c("var 1", "var 2"),
        controls = "var 3",
        type = "part",
        spearman = TRUE,
        kendall = TRUE,
        n = TRUE
    )

    # Test correlation table
    corTable <- r$matrix

    # Test Pearson's r
    testthat::expect_equal(
        0.254, as.numeric(corTable$getCell(rowKey="var 2", "var 1[r]")$value), tolerance = 1e-3
    )

    # Test Pearson's r p-value
    testthat::expect_equal(
        0.478, as.numeric(corTable$getCell(rowKey="var 2", "var 1[rp]")$value), tolerance = 1e-3
    )

    # Test Spearman's rho
    testthat::expect_equal(
        0.197, as.numeric(corTable$getCell(rowKey="var 2", "var 1[rho]")$value), tolerance = 1e-3
    )

    # Test Spearman's rho p-value
    testthat::expect_equal(
        0.586, as.numeric(corTable$getCell(rowKey="var 2", "var 1[rhop]")$value), tolerance = 1e-3
    )

    # Test Kendall's tau
    testthat::expect_equal(
        0.193, as.numeric(corTable$getCell(rowKey="var 2", "var 1[tau]")$value), tolerance = 1e-3
    )

    # Test Kendall's tau p-value
    testthat::expect_equal(
        0.436, as.numeric(corTable$getCell(rowKey="var 2", "var 1[taup]")$value), tolerance = 1e-3
    )

    # Test N
    testthat::expect_equal(11, as.numeric(corTable$getCell(rowKey="var 2", "var 1[n]")$value))
})

testthat::test_that('All options in the corrPart work for semipartial correlation (sunny)', {
    df <- data.frame(
        `var 1` = c(8, 51, 2, 74, 1, 91, 5, 25, 1, 59, 5, 32, 7),
        `var 2` = c(2, NA, NaN, 3, -1, -2, 1, 1, -2, 2, -2, -3, 3),
        `var 3` = c(0, 4, 19, 5, 9, 15, 1, 4, 19, 10, 13, 7, 5),
        check.names = FALSE
    )

    r <- jmv::corrPart(
        data = df,
        vars = c("var 1", "var 2"),
        controls = "var 3",
        type = "semi",
        spearman = TRUE,
        kendall = TRUE,
        n = TRUE
    )

    # Test correlation table
    corTable <- r$matrix

    # Test Pearson's r
    testthat::expect_equal(
        0.194, as.numeric(corTable$getCell(rowKey="var 2", "var 1[r]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        0.251, as.numeric(corTable$getCell(rowKey="var 1", "var 2[r]")$value), tolerance = 1e-3
    )

    # Test Pearson's r p-value
    testthat::expect_equal(
        0.591, as.numeric(corTable$getCell(rowKey="var 2", "var 1[rp]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        0.484, as.numeric(corTable$getCell(rowKey="var 1", "var 2[rp]")$value), tolerance = 1e-3
    )

    # Test Spearman's rho
    testthat::expect_equal(
        0.162, as.numeric(corTable$getCell(rowKey="var 2", "var 1[rho]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        0.196, as.numeric(corTable$getCell(rowKey="var 1", "var 2[rho]")$value), tolerance = 1e-3
    )

    # Test Spearman's rho p-value
    testthat::expect_equal(
        0.656, as.numeric(corTable$getCell(rowKey="var 2", "var 1[rhop]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        0.587, as.numeric(corTable$getCell(rowKey="var 1", "var 2[rhop]")$value), tolerance = 1e-3
    )

    # Test Kendall's tau
    testthat::expect_equal(
        0.177, as.numeric(corTable$getCell(rowKey="var 2", "var 1[tau]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        0.193, as.numeric(corTable$getCell(rowKey="var 1", "var 2[tau]")$value), tolerance = 1e-3
    )

    # Test Kendall's tau p-value
    testthat::expect_equal(
        0.477, as.numeric(corTable$getCell(rowKey="var 2", "var 1[taup]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        0.436, as.numeric(corTable$getCell(rowKey="var 1", "var 2[taup]")$value), tolerance = 1e-3
    )

    # Test N
    testthat::expect_equal(11, as.numeric(corTable$getCell(rowKey="var 2", "var 1[n]")$value))
    testthat::expect_equal(11, as.numeric(corTable$getCell(rowKey="var 1", "var 2[n]")$value))
})
