testthat::context('conttablespaired')

testthat::test_that('All options in the contTablesPaired work (sunny)', {
    dat <- data.frame(
        `1st survey` = c('Approve', 'Approve', 'Disapprove', 'Disapprove'),
        `2nd survey` = c('Approve', 'Disapprove', 'Approve', 'Disapprove'),
        `Counts` = c(794, 150, 86, 570),
        check.names=FALSE
    )

    r <- jmv::contTablesPaired(
        dat,
        rows = '1st survey',
        cols = '2nd survey',
        counts = 'Counts',
        chiSqCorr = TRUE,
        exact = TRUE,
        pcRow = TRUE,
        pcCol = TRUE
    )

    # Test the contingency tables table
    contTable <- r$freqs$asDF
    testthat::expect_equal(c('Approve', 'Disapprove', 'Total'), contTable[['1st survey']])
    testthat::expect_equal(c('Count', 'Count', 'Count'), contTable[['type[count]']])
    testthat::expect_equal(
        c('% within row', '% within row', '% within row'), contTable[['type[pcRow]']]
    )
    testthat::expect_equal(
        c('% within column', '% within column', '% within column'), contTable[['type[pcCol]']]
    )
    testthat::expect_equal(c(794, 86, 880), contTable[['1[count]']])
    testthat::expect_equal(c(0.841, 0.131, 0.55), contTable[['1[pcRow]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.902, 0.098, 1), contTable[['1[pcCol]']], tolerance = 1e-3)
    testthat::expect_equal(c(150, 570, 720), contTable[['2[count]']])
    testthat::expect_equal(c(0.159, 0.869, 0.45), contTable[['2[pcRow]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.208, 0.792, 1), contTable[['2[pcCol]']], tolerance = 1e-3)
    testthat::expect_equal(c(944, 656, 1600), contTable[['.total[count]']])


    # Test the McNemar test table
    testTable <- r$test$asDF
    testthat::expect_equal(17.356, testTable[['value[mcn]']], tolerance = 1e-3)
    testthat::expect_equal(1, testTable[['df[mcn]']])
    testthat::expect_equal(0, testTable[['p[mcn]']], tolerance = 1e-3)
    testthat::expect_equal(16.818, testTable[['value[cor]']], tolerance = 1e-3)
    testthat::expect_equal(1, testTable[['df[cor]']])
    testthat::expect_equal(0, testTable[['p[cor]']], tolerance = 1e-3)
    testthat::expect_equal(0.556, testTable[['value[exa]']], tolerance = 1e-3)
    testthat::expect_equal(0, testTable[['p[exa]']], tolerance = 1e-3)
    testthat::expect_equal(1600, testTable[['value[n]']])
})
