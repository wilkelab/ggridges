testthat::context('anovaonew')

data('ToothGrowth', package='datasets')

testthat::test_that('All options in the anovaOneW work (sunny)', {
    dat <- ToothGrowth
    dat$dose <- factor(dat$dose)

    r <- jmv::anovaOneW(
        dat,
        deps = "len",
        group = "dose",
        fishers = TRUE,
        desc = TRUE,
        norm = TRUE,
        eqv = TRUE,
        phMethod = "gamesHowell",
        phTest = TRUE
    )

    # Test anova table
    main <-r$anova$asDF
    testthat::expect_equal(67.416, main$`F[fisher]`, tolerance = 1e-5)
    testthat::expect_equal(68.401, main$`F[welch]`, tolerance = 1e-5)
    testthat::expect_equal(2, main$`df1[fisher]`)
    testthat::expect_equal(2, main$`df1[welch]`)
    testthat::expect_equal(57, main$`df2[fisher]`)
    testthat::expect_equal(37.743, main$`df2[welch]`, tolerance = 1e-5)
    testthat::expect_equal(9.5333e-16, main$`p[fisher]`, tolerance = 1e-5)
    testthat::expect_equal(2.8124e-13, main$`p[welch]`, tolerance = 1e-5)

    # Test descriptives table
    desc <- r$desc$asDF
    ns <- as.numeric(tapply(dat$len, dat$dose, length))
    means <- as.numeric(tapply(dat$len, dat$dose, mean))
    sds <- as.numeric(tapply(dat$len, dat$dose, sd))
    ses <- sds / sqrt(ns)

    testthat::expect_equal(ns, desc$num, tolerance = 1e-5)
    testthat::expect_equal(means, desc$mean, tolerance = 1e-5)
    testthat::expect_equal(sds, desc$sd, tolerance = 1e-5)
    testthat::expect_equal(ses, desc$se, tolerance = 1e-5)

    # Test normality table
    norm <- r$assump$norm$asDF
    testthat::expect_equal(0.967, norm$w, tolerance = 1e-3)
    testthat::expect_equal(0.108, norm$p, tolerance = 1e-3)

    # Test levene's table
    levene <- r$assump$eqv$asDF
    testthat::expect_equal(0.73276, levene$F, tolerance = 1e-5)
    testthat::expect_equal(2, levene$df1, tolerance = 1e-5)
    testthat::expect_equal(57, levene$df2, tolerance = 1e-5)
    testthat::expect_equal(0.48505, levene$p, tolerance = 1e-5)

    # Test post-hoc table
    postHoc <- r$postHoc[[1]]
    testthat::expect_equal(-9.13, postHoc$getCell(rowKey="0.5", "1[md]")$value, tolerance = 1e-5)
    testthat::expect_equal(-6.4766, postHoc$getCell(rowKey="0.5", "1[t]")$value, tolerance = 1e-5)
    testthat::expect_equal(37.9864, postHoc$getCell(rowKey="0.5", "1[df]")$value, tolerance = 1e-5)
    testthat::expect_equal(3.76225e-07, postHoc$getCell(rowKey="0.5", "1[p]")$value, tolerance = 1e-10)

    testthat::expect_equal(-15.495, postHoc$getCell(rowKey="0.5", "2[md]")$value, tolerance = 1e-5)
    testthat::expect_equal(-11.799, postHoc$getCell(rowKey="0.5", "2[t]")$value, tolerance = 1e-5)
    testthat::expect_equal(36.8826, postHoc$getCell(rowKey="0.5", "2[df]")$value, tolerance = 1e-5)
    testthat::expect_equal(0, postHoc$getCell(rowKey="0.5", "2[p]")$value, tolerance = 1e-10)

    testthat::expect_equal(-6.365, postHoc$getCell(rowKey="1", "2[md]")$value, tolerance = 1e-5)
    testthat::expect_equal(-4.9005, postHoc$getCell(rowKey="1", "2[t]")$value, tolerance = 1e-5)
    testthat::expect_equal(37.101, postHoc$getCell(rowKey="1", "2[df]")$value, tolerance = 1e-5)
    testthat::expect_equal(5.5686e-05, postHoc$getCell(rowKey="1", "2[p]")$value, tolerance = 1e-9)
})
