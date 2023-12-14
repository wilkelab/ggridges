testthat::context('anovanp')

testthat::test_that('All options in the anovaNP work (sunny)', {
    factor <- as.factor(c(rep(c("a", "b", "c"), each=6)))
    dep <-  c(0,4,19,5,9,15,1,4,19,10,13,7,5,12,2,23,6,13)

    data <- data.frame(f = factor, dep = dep)

    r <- jmv::anovaNP(data, deps='dep', group='f', es=TRUE, pairs=TRUE)

    # Test anova table
    main <- r$table$asDF
    testthat::expect_equal(0.167, main$chiSq, tolerance = 1e-3)
    testthat::expect_equal(2, main$df, tolerance = 1e-3)
    testthat::expect_equal(0.920, main$p, tolerance = 1e-3)
    testthat::expect_equal(0.0098, main$es, tolerance = 1e-4)

    # Test comparisons table
    comp <- r$comparisons[[1]]$asDF
    testthat::expect_equal(c(0.227, 0.567, 0.340), comp$W, tolerance = 1e-3)
    testthat::expect_equal(c(0.986, 0.915, 0.969), comp$p, tolerance = 1e-3)
})
