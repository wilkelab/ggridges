testthat::context('corrmatrix')

testthat::test_that('All options in the corrMatrix work (sunny)', {
    df <- data.frame(
        `var 1` = c(8, 51, 2, 74, 1, 91, 5, 25, 1, 59, 5, 32, 7),
        `var 2` = c(2, NA, NaN, 3, -1, -2, 1, 1, -2, 2, -2, -3, 3),
        `var 3` = c(0, 4, 19, 5, 9, 15, 1, 4, 19, 10, 13, 7, 5),
        check.names = FALSE
    )

    r <- jmv::corrMatrix(
        data = df,
        vars = c("var 1", "var 2","var 3"),
        spearman = TRUE,
        kendall = TRUE,
        n = TRUE,
        ci = TRUE
    )

    # Test correlation table
    corTable <- r$matrix

    # Test Pearson's r
    testthat::expect_equal(
        0.0834, as.numeric(corTable$getCell(rowKey="var 2", "var 1[r]")$value), tolerance = 1e-5
    )
    testthat::expect_equal(
        -0.0247, as.numeric(corTable$getCell(rowKey="var 3", "var 1[r]")$value), tolerance = 1e-4
    )
    testthat::expect_equal(
        -0.647, as.numeric(corTable$getCell(rowKey="var 3", "var 2[r]")$value), tolerance = 1e-3
    )

    # Test Pearson's r degrees of freedom
    testthat::expect_equal(9, as.numeric(corTable$getCell(rowKey="var 2", "var 1[rdf]")$value))
    testthat::expect_equal(11, as.numeric(corTable$getCell(rowKey="var 3", "var 1[rdf]")$value))
    testthat::expect_equal(9, as.numeric(corTable$getCell(rowKey="var 3", "var 2[rdf]")$value))

    # Test Pearson's r p-value
    testthat::expect_equal(
        0.807, as.numeric(corTable$getCell(rowKey="var 2", "var 1[rp]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        0.936, as.numeric(corTable$getCell(rowKey="var 3", "var 1[rp]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        0.0315, as.numeric(corTable$getCell(rowKey="var 3", "var 2[rp]")$value), tolerance = 1e-4
    )

    # Test Pearson's r confidence interval upper limit
    testthat::expect_equal(
        0.651, as.numeric(corTable$getCell(rowKey="var 2", "var 1[rciu]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        0.534, as.numeric(corTable$getCell(rowKey="var 3", "var 1[rciu]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        -0.0766, as.numeric(corTable$getCell(rowKey="var 3", "var 2[rciu]")$value), tolerance = 1e-4
    )

    # Test Pearson's r confidence interval lower limit
    testthat::expect_equal(
        -0.544, as.numeric(corTable$getCell(rowKey="var 2", "var 1[rcil]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        -0.568, as.numeric(corTable$getCell(rowKey="var 3", "var 1[rcil]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        -0.898, as.numeric(corTable$getCell(rowKey="var 3", "var 2[rcil]")$value), tolerance = 1e-3
    )

    # Test Spearman's rho
    testthat::expect_equal(
        0.193, as.numeric(corTable$getCell(rowKey="var 2", "var 1[rho]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        -0.225, as.numeric(corTable$getCell(rowKey="var 3", "var 1[rho]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        -0.569, as.numeric(corTable$getCell(rowKey="var 3", "var 2[rho]")$value), tolerance = 1e-3
    )

    # Test Spearman's rho degrees of freedom
    testthat::expect_equal(9, as.numeric(corTable$getCell(rowKey="var 2", "var 1[rhodf]")$value))
    testthat::expect_equal(11, as.numeric(corTable$getCell(rowKey="var 3", "var 1[rhodf]")$value))
    testthat::expect_equal(9, as.numeric(corTable$getCell(rowKey="var 3", "var 2[rhodf]")$value))

    # Test Spearman's rho p-value
    testthat::expect_equal(
        0.570, as.numeric(corTable$getCell(rowKey="var 2", "var 1[rhop]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        0.459, as.numeric(corTable$getCell(rowKey="var 3", "var 1[rhop]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        0.0674, as.numeric(corTable$getCell(rowKey="var 3", "var 2[rhop]")$value), tolerance = 1e-4
    )

    # Test Kendall's tau
    testthat::expect_equal(
        0.177, as.numeric(corTable$getCell(rowKey="var 2", "var 1[tau]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        -0.119, as.numeric(corTable$getCell(rowKey="var 3", "var 1[tau]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        -0.408, as.numeric(corTable$getCell(rowKey="var 3", "var 2[tau]")$value), tolerance = 1e-3
    )

    # Test Kendall's tau p-value
    testthat::expect_equal(
        0.472, as.numeric(corTable$getCell(rowKey="var 2", "var 1[taup]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        0.579, as.numeric(corTable$getCell(rowKey="var 3", "var 1[taup]")$value), tolerance = 1e-3
    )
    testthat::expect_equal(
        0.0942, as.numeric(corTable$getCell(rowKey="var 3", "var 2[taup]")$value), tolerance = 1e-4
    )

    # Test N
    testthat::expect_equal(11, as.numeric(corTable$getCell(rowKey="var 2", "var 1[n]")$value))
    testthat::expect_equal(13, as.numeric(corTable$getCell(rowKey="var 3", "var 1[n]")$value))
    testthat::expect_equal(11, as.numeric(corTable$getCell(rowKey="var 3", "var 2[n]")$value))
})
