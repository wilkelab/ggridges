testthat::context('descriptives')

testthat::test_that('All options in the descriptives work for cont vars without split by (sunny)', {
    CI_WIDTH <- 0.95
    QUANTS <- c(0.25, 0.50, 0.75)

    set.seed(1337)
    x <- rnorm(100, 0, 1)
    df <- data.frame(x=x)

    desc <- jmv::descriptives(
        data=df,
        vars=x,
        mode=TRUE,
        sum=TRUE,
        variance=TRUE,
        range=TRUE,
        se=TRUE,
        ci=TRUE,
        iqr=TRUE,
        skew=TRUE,
        kurt=TRUE,
        sw=TRUE,
        pcEqGr=TRUE,
        pc=TRUE
    )

    r <- desc$descriptives$asDF

    # Calculate statistics
    missing <- sum(is.na(x))
    n <- length(x) - missing
    mean <- mean(x)
    se <- sd(x) / sqrt(n)
    tCriticalValue <- 1 - ((1 - CI_WIDTH) / 2)
    ciLower <- mean - qt(tCriticalValue, df=n-1) * se
    ciUpper <- mean + qt(tCriticalValue, df=n-1) * se
    mode <- as.numeric(names(table(x)[table(x)==max(table(x))]))[1]
    shapiro <- shapiro.test(x)
    quantiles <- quantile(x, QUANTS)

    # Test descriptives table
    testthat::expect_equal(n, r[["x[n]"]], tolerance = 1e-5)
    testthat::expect_equal(missing, r[["x[missing]"]], tolerance = 1e-5)
    testthat::expect_equal(mean, r[["x[mean]"]], tolerance = 1e-5)
    testthat::expect_equal(se, r[["x[se]"]], tolerance = 1e-5)
    testthat::expect_equal(ciLower, r[["x[ciLower]"]], tolerance = 1e-5)
    testthat::expect_equal(ciUpper, r[["x[ciUpper]"]], tolerance = 1e-5)
    testthat::expect_equal(median(x), r[["x[median]"]], tolerance = 1e-5)
    testthat::expect_equal(mode, r[["x[mode]"]], tolerance = 1e-5)
    testthat::expect_equal(sum(x), r[["x[sum]"]], tolerance = 1e-5)
    testthat::expect_equal(sd(x), r[["x[sd]"]], tolerance = 1e-5)
    testthat::expect_equal(var(x), r[["x[variance]"]], tolerance = 1e-5)
    testthat::expect_equal(IQR(x), r[["x[iqr]"]], tolerance = 1e-5)
    testthat::expect_equal(range(x)[2] - range(x)[1], r[["x[range]"]], tolerance = 1e-5)
    testthat::expect_equal(min(x), r[["x[min]"]], tolerance = 1e-5)
    testthat::expect_equal(max(x), r[["x[max]"]], tolerance = 1e-5)
    testthat::expect_equal(0.11014, r[["x[skew]"]], tolerance = 1e-5)
    testthat::expect_equal(0.24138, r[["x[seSkew]"]], tolerance = 1e-5)
    testthat::expect_equal(-0.11958, r[["x[kurt]"]], tolerance = 1e-5)
    testthat::expect_equal(0.47833, r[["x[seKurt]"]], tolerance = 1e-5)
    testthat::expect_equal(as.numeric(shapiro$statistic), r[["x[sww]"]], tolerance = 1e-5)
    testthat::expect_equal(as.numeric(shapiro$p.value), r[["x[sw]"]], tolerance = 1e-5)
    testthat::expect_equal(as.numeric(quantiles[1]), r[["x[quant1]"]], tolerance = 1e-5)
    testthat::expect_equal(as.numeric(quantiles[2]), r[["x[quant2]"]], tolerance = 1e-5)
    testthat::expect_equal(as.numeric(quantiles[3]), r[["x[quant3]"]], tolerance = 1e-5)

    # Check footnote for including CI
    testthat::expect_match(desc$descriptives$notes$ci$note, "t-distribution")
})

testthat::test_that('Descriptives transposed table works with splitBy', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    df <- data.frame(
        Q1=rnorm(100),
        Q2=rnorm(100),
        Q3=rnorm(100),
        Q4=rnorm(100),
        group=sample(letters[1:3], 100, replace = TRUE)
    )

    desc <- jmv::descriptives(
        data=df, vars=vars(Q1, Q2, Q3, Q4), splitBy=group, desc="rows"
    )

    r <- desc$descriptivesT$asDF

    testthat::expect_equal(c(36, 28, 36, 36, 28, 36, 36, 28, 36, 36, 28, 36), r$n)
    testthat::expect_equal(
        c(0.1454, 0.2344, 0.3307, 0.09781, -0.02078, 0.03245, -0.2239, -0.1114,
          -0.1302, -0.005095, -0.1445, 0.01393),
        r$mean, tolerance=1e-4
    )
    testthat::expect_equal(
        c(1.138, 0.8853, 1.138, 0.9002, 1.178, 0.9884, 1.044, 1.225, 0.9436,
          0.8925, 1.070, 0.843),
        r$sd, tolerance=1e-3
    )
    testthat::expect_equal(
        c(-2.344, -1.774, -1.679, -1.154, -2.474, -2.32, -2.38, -2.689, -1.979,
          -1.697, -1.867, -1.493),
        r$min, tolerance=1e-3
    )
    testthat::expect_equal(
        c(2.199, 1.785, 3.446, 3.104, 2.929, 2.209, 2.258, 3.406, 1.933, 1.851,
          1.898, 2.163),
        r$max, tolerance=1e-4
    )
})

testthat::test_that("Frequency table is displayed correctly for empty data set", {
    df <- data.frame(
        dep = factor(levels = letters[1:3]),
        group = factor(levels = LETTERS[1:3])
    )

    desc <- jmv::descriptives(data = df, vars = "dep", splitBy = "group", freq = TRUE)
    freq <- desc$frequencies[[1]]$asDF

    testthat::expect_equal(rep(letters[1:3], each=3), freq[[1]])
    testthat::expect_equal(rep(LETTERS[1:3], times=3), freq[[2]])
    testthat::expect_equal(rep(0, 9), freq$counts)
    testthat::expect_equal(rep(0, 9), freq$pc)
    testthat::expect_equal(rep(0, 9), freq$cumpc)
})

testthat::test_that("Non-grouped frequency table is displayed correctly", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    df <- data.frame(
        dep = factor(sample(letters[1:3], 100, replace = TRUE), levels = letters[1:3])
    )

    desc <- jmv::descriptives(data = df, vars = "dep", freq = TRUE)
    freq <- desc$frequencies[[1]]$asDF

    counts <- as.vector(table(df))

    testthat::expect_equal(letters[1:3], freq$dep)
    testthat::expect_equal(counts, freq$counts)
    testthat::expect_equal(counts / sum(counts), freq$pc)
    testthat::expect_equal(cumsum(counts) / sum(counts), freq$cumpc)
})

testthat::test_that("Grouped frequency table is displayed correctly", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)

    df <- data.frame(
        dep = factor(sample(1:3, 100, replace = TRUE), levels = 1:3),
        group = factor(sample(letters[1:3], 100, replace = TRUE), levels = letters[1:3])
    )

    desc <- jmv::descriptives(data = df, vars = "dep", splitBy = "group", freq = TRUE)
    freq <- desc$frequencies[[1]]$asDF

    testthat::expect_equal(as.character(rep(1:3, each=3)), freq[[1]])
    testthat::expect_equal(rep(letters[1:3], times=3), freq[[2]])
    testthat::expect_equal(c(13, 10, 10, 7, 15, 6, 13, 10, 16), freq$counts)
    testthat::expect_equal(c(0.13, 0.1, 0.1, 0.07, 0.15, 0.06, 0.13, 0.1, 0.16), freq$pc)
    testthat::expect_equal(c(0.13, 0.23, 0.33, 0.4, 0.55, 0.61, 0.74, 0.84, 1), freq$cumpc)
})

testthat::test_that('Descriptives works old scenario', {
    w <- as.factor(rep(c("1", "2","3"), each=4))
    x <- as.factor(rep(c("a", "b","c"), 4))
    y <- c(4,4,3,4,8,0,9,8,8,6,0,3)
    z <- c(NA,NaN,3,-1,-2,1,1,-2,2,-2,-3,3)

    data <- data.frame(w=w, x=x, y=y, z=z)
    desc <- jmv::descriptives(data, vars=c("w", "y", "z"), splitBy = "x",
                              freq=TRUE, median=TRUE, mode=TRUE, skew=TRUE,
                              kurt=TRUE, pc=TRUE)

    freq <- desc$frequencies[[1]]$asDF
    descr <- desc$descriptives$asDF

    # Test frequency table numerical values
    testthat::expect_equal(c(2, 1, 1, 1, 2, 1, 1, 1, 2), freq$counts)

    # Test descriptives table numerical values
    testthat::expect_equal(2.619, descr$`y[seKurtb]`, tolerance = 1e-3)
    testthat::expect_equal(-1.289, descr$`z[kurtc]`, tolerance = 1e-3)
    testthat::expect_equal(1, descr$`z[missinga]`, tolerance = 1e-3)
    testthat::expect_equal(5.750, descr$`y[meana]`, tolerance = 1e-3)
    testthat::expect_equal(-2, descr$`z[modeb]`, tolerance = 1e-3)
    testthat::expect_equal(4, descr$`y[mina]`, tolerance = 1e-3)
    testthat::expect_equal(2.25, descr$`y[perc1c]`, tolerance = 1e-3)

})

testthat::test_that('Histogram is created for nominal numeric variable', {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    data <- data.frame(
        a1 = rnorm(100, 0, 10),
        a2 = factor(sample(1:10, 100, replace = TRUE))
    )

    attr(data$a2, 'values') <- 1:10

    desc <- jmv::descriptives(data, c('a1', 'a2'), hist=TRUE)

    testthat::expect_true(desc$plots[[2]]$.render())
})

testthat::test_that("No error is thrown when an empty factor is used as variable", {
    data <- data.frame(x = factor(rep(NA, 10)))
    result <- jmv::descriptives(data, "x", freq = TRUE)
    desc <- result$descriptives$asDF

    testthat::expect_equal(desc[["x[n]"]], 0)
    testthat::expect_equal(desc[["x[missing]"]], 10)
})

testthat::test_that('Sensible error message is provided when splitBy var contains no data', {
    df <- data.frame(
        var = 1:10,
        group = factor(rep(NA, 10))
    )

    testthat::expect_error(
        jmv::descriptives(formula=var~group, data=df),
        "The 'split by' variable 'group' contains no data."
    )
})

testthat::test_that('Extreme values table works', {
    df <- data.frame(
        numeric = rnorm(100),
        ordinal = sample(1:7, 100, replace = TRUE),
        character = factor(sample(letters[1:7], 100, replace = TRUE))
    )

    extremeN <- 5

    r <- jmv::descriptives(
        data=df, vars=c("numeric", "ordinal", "character"), extreme=TRUE, extremeN=extremeN
    )

    e1 <- r$extremeValues[[1]]$asDF
    lowest <- head(df[order(df$numeric),], extremeN)
    highest <- head(df[order(-df$numeric),], extremeN)
    casesExpected <- c(rownames(highest), rownames(lowest))
    valuesExpected <- c(highest$numeric, lowest$numeric)

    testthat::expect_equal(e1$row, casesExpected)
    testthat::expect_equal(e1$value, valuesExpected)

    e2 <- r$extremeValues[[2]]$asDF
    lowest <- head(df[order(df$ordinal),], extremeN)
    highest <- head(df[order(-df$ordinal),], extremeN)
    casesExpected <- c(rownames(highest), rownames(lowest))
    valuesExpected <- c(highest$ordinal, lowest$ordinal)

    testthat::expect_equal(e2$row, casesExpected)
    testthat::expect_equal(e2$value, valuesExpected)

    e3 <- r$extremeValues[[3]]$asDF
    testthat::expect_true(all(is.na(e3)))
})

testthat::test_that('Extreme values provides note if number of cases is lower than extremeN', {
    df <- data.frame(x = c(1.1, 2.3, 3.1))
    extremeN <- 5

    r <- jmv::descriptives(data=df, vars="x", extreme=TRUE, extremeN=extremeN)
    e <- r$extremeValues[[1]]

    testthat::expect_match(
        e$notes$insufficientData$note,
        "Number of requested extreme values is higher than the number of rows in the data."
    )

    eDf <- e$asDF
    testthat::expect_equal(eDf$row, c("3", "2", "1", NA, NA, "1", "2", "3", NA, NA))
    testthat::expect_equal(eDf$value, c(3.1, 2.3, 1.1, NA, NA, 1.1, 2.3, 3.1, NA, NA))
})

