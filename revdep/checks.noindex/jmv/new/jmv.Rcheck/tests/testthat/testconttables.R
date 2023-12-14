testthat::context('conttables')

testthat::test_that('All options in the contTables work (sunny)', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    df <- data.frame(
        `x 1` = sample(letters[1:2], 100, replace = TRUE),
        y = sample(LETTERS[1:2], 100, replace = TRUE),
        stringsAsFactors = TRUE,
        check.names = FALSE
    )

    r <- jmv::contTables(
        data=df,
        rows="x 1",
        cols="y",
        chiSqCorr = TRUE,
        zProp = TRUE,
        likeRat = TRUE,
        fisher = TRUE,
        contCoef = TRUE,
        phiCra = TRUE,
        diffProp = TRUE,
        logOdds = TRUE,
        odds = TRUE,
        relRisk = TRUE,
        gamma = TRUE,
        taub = TRUE,
        mh = TRUE,
        exp = TRUE,
        pcRow = TRUE,
        pcCol = TRUE,
        pcTot = TRUE
    )

    # Test main contingency tables
    mainTable <- r$freqs$asDF
    testthat::expect_equal(c('a', 'b', 'Total'), mainTable[['x 1']])
    testthat::expect_equal(c('Observed', 'Observed', 'Observed'), mainTable[['type[count]']])
    testthat::expect_equal(c('Expected', 'Expected', 'Expected'), mainTable[['type[expected]']])
    testthat::expect_equal(
        c('% within row', '% within row', '% within row'), mainTable[['type[pcRow]']]
    )
    testthat::expect_equal(
        c('% within column', '% within column', '% within column'), mainTable[['type[pcCol]']]
    )
    testthat::expect_equal(c('% of total', '% of total', '% of total'), mainTable[['type[pcTot]']])
    testthat::expect_equal(c(23, 28, 51), mainTable[['1[count]']])
    testthat::expect_equal(c(22.95, 28.05, 51), mainTable[['1[expected]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.511, 0.509, 0.51), mainTable[['1[pcRow]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.451, 0.549, 1), mainTable[['1[pcCol]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.23, 0.28, 0.51), mainTable[['1[pcTot]']], tolerance = 1e-3)
    testthat::expect_equal(c(22, 27, 49), mainTable[['2[count]']])
    testthat::expect_equal(c(22.05, 26.95, 49), mainTable[['2[expected]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.489, 0.491, 0.49), mainTable[['2[pcRow]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.449, 0.551, 1), mainTable[['2[pcCol]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.22, 0.27, 0.49), mainTable[['2[pcTot]']], tolerance = 1e-3)
    testthat::expect_equal(c(45, 55, 100), mainTable[['.total[count]']])
    testthat::expect_equal(c(45, 55, 100), mainTable[['.total[exp]']], tolerance = 1e-3)
    testthat::expect_equal(c(1, 1, 1), mainTable[['.total[pcRow]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.45, 0.55, 1), mainTable[['.total[pcCol]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.45, 0.55, 1), mainTable[['.total[pcTot]']], tolerance = 1e-3)

    # Test chi squared tests table
    chiSqTable <- r$chiSq$asDF
    testthat::expect_equal('χ²', chiSqTable[['test[chiSq]']])
    testthat::expect_equal(0, chiSqTable[['value[chiSq]']], tolerance = 1e-3)
    testthat::expect_equal(1, chiSqTable[['df[chiSq]']], tolerance = 1e-3)
    testthat::expect_equal(0.984, chiSqTable[['p[chiSq]']], tolerance = 1e-3)
    testthat::expect_equal('χ² continuity correction', chiSqTable[['test[chiSqCorr]']])
    testthat::expect_equal(0, chiSqTable[['value[chiSqCorr]']], tolerance = 1e-3)
    testthat::expect_equal(1, chiSqTable[['df[chiSqCorr]']], tolerance = 1e-3)
    testthat::expect_equal(1, chiSqTable[['p[chiSqCorr]']], tolerance = 1e-3)
    testthat::expect_equal('z test difference in 2 proportions', chiSqTable[['test[zProp]']])
    testthat::expect_equal(0.02, chiSqTable[['value[zProp]']], tolerance = 1e-3)
    testthat::expect_equal(NA, chiSqTable[['df[zProp]']], tolerance = 1e-3)
    testthat::expect_equal(0.984, chiSqTable[['p[zProp]']], tolerance = 1e-3)
    testthat::expect_equal('Likelihood ratio', chiSqTable[['test[likeRat]']])
    testthat::expect_equal(0, chiSqTable[['value[likeRat]']], tolerance = 1e-3)
    testthat::expect_equal(1, chiSqTable[['df[likeRat]']], tolerance = 1e-3)
    testthat::expect_equal(0.984, chiSqTable[['p[likeRat]']], tolerance = 1e-3)
    testthat::expect_equal('Fisher\'s exact test', chiSqTable[['test[fisher]']])
    testthat::expect_equal(NA, chiSqTable[['value[fisher]']], tolerance = 1e-3)
    testthat::expect_equal(1, chiSqTable[['p[fisher]']], tolerance = 1e-3)
    testthat::expect_equal('N', chiSqTable[['test[N]']], tolerance = 1e-3)
    testthat::expect_equal(100, chiSqTable[['value[N]']], tolerance = 1e-3)


    # Test comparative measures table
    compMeasuresTable <- r$odds$asDF
    testthat::expect_equal('Difference in 2 proportions', compMeasuresTable[['t[dp]']])
    testthat::expect_equal(0.002, compMeasuresTable[['v[dp]']], tolerance = 1e-3)
    testthat::expect_equal(-0.195, compMeasuresTable[['cil[dp]']], tolerance = 1e-3)
    testthat::expect_equal(0.199, compMeasuresTable[['ciu[dp]']], tolerance = 1e-3)
    testthat::expect_equal('Log odds ratio', compMeasuresTable[['t[lo]']])
    testthat::expect_equal(0.008, compMeasuresTable[['v[lo]']], tolerance = 1e-3)
    testthat::expect_equal(-0.78, compMeasuresTable[['cil[lo]']], tolerance = 1e-3)
    testthat::expect_equal(0.796, compMeasuresTable[['ciu[lo]']], tolerance = 1e-3)
    testthat::expect_equal('Odds ratio', compMeasuresTable[['t[o]']])
    testthat::expect_equal(1.008, compMeasuresTable[['v[o]']], tolerance = 1e-3)
    testthat::expect_equal(0.458, compMeasuresTable[['cil[o]']], tolerance = 1e-3)
    testthat::expect_equal(2.217, compMeasuresTable[['ciu[o]']], tolerance = 1e-3)
    testthat::expect_equal('Relative risk', compMeasuresTable[['t[rr]']])
    testthat::expect_equal(1.004, compMeasuresTable[['v[rr]']], tolerance = 1e-3)
    testthat::expect_equal(0.682, compMeasuresTable[['cil[rr]']], tolerance = 1e-3)
    testthat::expect_equal(1.477, compMeasuresTable[['ciu[rr]']], tolerance = 1e-3)


    # Test nominal table
    nominalTable <- r$nom$asDF
    testthat::expect_equal('Contingency coefficient', nominalTable[['t[cont]']])
    testthat::expect_equal(0.002, nominalTable[['v[cont]']], tolerance = 1e-3)
    testthat::expect_equal('Phi-coefficient', nominalTable[['t[phi]']])
    testthat::expect_equal(0.002, nominalTable[['v[phi]']], tolerance = 1e-3)
    testthat::expect_equal('Cramer\'s V', nominalTable[['t[cra]']])
    testthat::expect_equal(0.002, nominalTable[['v[cra]']], tolerance = 1e-3)

    # Test gamma table
    gammaTable <- r$gamma$asDF
    testthat::expect_equal(0.004, gammaTable[['gamma']], tolerance = 1e-3)
    testthat::expect_equal(0.201, gammaTable[['se']], tolerance = 1e-3)
    testthat::expect_equal(-0.39, gammaTable[['cil']], tolerance = 1e-3)
    testthat::expect_equal(0.398, gammaTable[['ciu']], tolerance = 1e-3)

    # Test Kendall's tau table
    tauTable <- r$taub$asDF
    testthat::expect_equal(0.002, tauTable[['taub']], tolerance = 1e-3)
    testthat::expect_equal(0.02, tauTable[['t']], tolerance = 1e-3)
    testthat::expect_equal(0.984, tauTable[['p']], tolerance = 1e-3)

    # Test Mantel-Haenszel test table
    mhTable <- r$mh$asDF
    testthat::expect_equal(0, mhTable[['chi2']], tolerance = 1e-3)
    testthat::expect_equal(1, mhTable[['df']], tolerance = 1e-3)
    testthat::expect_equal(0.984, mhTable[['p']], tolerance = 1e-3)
})

testthat::test_that('conttables works without counts', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(100)

    x <- factor(sample(c("A","B"), 100, replace = TRUE), c("A","B"))
    y <- factor(sample(c("I","II"), 100, replace = TRUE), c("I","II"))
    z <- factor(sample(c("foo","bar"), 100, replace = TRUE), c("foo","bar"))
    w <- factor(sample(c("fred","steve"), 100, replace = TRUE), c("fred","steve"))

    data1 <- data.frame(x = x, y = y, z = z, w = w)

    table1<- jmv::contTables(data=data1, rows="x", cols="y")

    freqs1 <- as.data.frame(table1$freqs)

    testthat::expect_equal(28, freqs1[1, '1[count]'])
    testthat::expect_equal(22, freqs1[1, '2[count]'])
    testthat::expect_equal(22, freqs1[2, '1[count]'])
    testthat::expect_equal(28, freqs1[2, '2[count]'])

    table2 <- jmv::contTables(data=data1, rows="x", cols="y", layers=c("z","w"))

    freqs2 <- as.data.frame(table2$freqs)

    testthat::expect_equal(28, freqs2[25, '1[count]'])
    testthat::expect_equal(22, freqs2[25, '2[count]'])
    testthat::expect_equal(22, freqs2[26, '1[count]'])
    testthat::expect_equal(28, freqs2[26, '2[count]'])

    testthat::expect_equal(9, freqs2[10, '1[count]'])
    testthat::expect_equal(4, freqs2[10, '2[count]'])
    testthat::expect_equal(4, freqs2[11, '1[count]'])
    testthat::expect_equal(6, freqs2[11, '2[count]'])
})

testthat::test_that("conttables works with counts", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(212)

    rows <- factor(c("A","B","C","A","B","C","A","B","C","A","B","C"), c("A","B","C"))
    cols <- factor(c("1","1","1","2","2","2","1","1","1","2","2","2"), c("1","2"))
    layer <- factor(c("I","I","I","I","I","I","II","II","II","II","II","II"), c("I","II"))
    counts <- sample(0:20, 12, replace = TRUE)

    data <- data.frame(rows = rows, cols = cols, layer = layer, counts = counts)

    table <- jmv::contTables(data=data, rows="rows", cols="cols", layers="layer", counts="counts")

    freqs <- as.data.frame(table$freqs)

    testthat::expect_equal(8, freqs[1, '1[count]'])
    testthat::expect_equal(3, freqs[1, '2[count]'])
    testthat::expect_equal(17, freqs[2, '1[count]'])
    testthat::expect_equal(0, freqs[2, '2[count]'])
    testthat::expect_equal(84, freqs[12, '1[count]'])
    testthat::expect_equal(32, freqs[12, '2[count]'])
})

testthat::test_that("conttables works with global integer weights", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(212)

    rows <- factor(c("A","B","C","A","B","C","A","B","C","A","B","C"), c("A","B","C"))
    cols <- factor(c("1","1","1","2","2","2","1","1","1","2","2","2"), c("1","2"))
    layer <- factor(c("I","I","I","I","I","I","II","II","II","II","II","II"), c("I","II"))
    counts <- sample(0:20, 12, replace = TRUE)

    data <- data.frame(rows = rows, cols = cols, layer = layer)
    attr(data, "jmv-weights") <- counts

    table <- jmv::contTables(data=data, rows="rows", cols="cols", layers="layer")

    freqs <- as.data.frame(table$freqs)

    testthat::expect_equal(8, freqs[1, '1[count]'])
    testthat::expect_equal(3, freqs[1, '2[count]'])
    testthat::expect_equal(17, freqs[2, '1[count]'])
    testthat::expect_equal(0, freqs[2, '2[count]'])
    testthat::expect_equal(84, freqs[12, '1[count]'])
    testthat::expect_equal(32, freqs[12, '2[count]'])
})

testthat::test_that("bar plots work with spaces in variable name", {
    data <- ToothGrowth
    data$dose <- factor(data$dose)
    names(data) <- c("len", "su pp", "do se")

    table <- jmv::contTables(data=data, rows="su pp", cols="do se", barplot=TRUE)

    testthat::expect_true(table$barplot$.render())
})
