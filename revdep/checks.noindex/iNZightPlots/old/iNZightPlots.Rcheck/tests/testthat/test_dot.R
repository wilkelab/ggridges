context("Dot plots")

test_that("Colour by", {
    expect_is(
        iNZightPlot(Sepal.Width, colby = Species, data = iris),
        "inzplotoutput"
    )
    expect_is(
        iNZightPlot(Sepal.Width, colby = Petal.Width, data = iris),
        "inzplotoutput"
    )
})

test_that("Colour by with quantiles", {
    expect_is(
        iNZightPlot(Sepal.Width, colby = Petal.Width, col.method = "rank",
            data = iris),
        "inzplotoutput"
    )
})

test_that("Mean indicator", {
    pl <- iNZightPlot(Sepal.Width, data = iris, mean_indicator = TRUE)
    pl <- iNZightPlot(Sepal.Width, data = iris, mean_indicator = TRUE,
        plottype = "hist",
        cex.dotpt = 5
    )
})

test_that("Inference information is correct", {
    set.seed(1)
    x <- rnorm(100, sd = 10)
    pl <- iNZightPlot(x,
        plot = interactive(),
        inference.par = "mean",
        inference.type = "conf",
        mean_indicator = FALSE,
        boxplot = TRUE
    )
    xbar <- mean(x)
    wd <- qt(0.975, length(x) - 1) * sd(x) / sqrt(length(x))
    expect_equal(
        as.numeric(pl$all$all$inference.info$mean$conf),
        c(xbar - wd, xbar + wd, xbar)
    )
    expect_equal(
        pl$all$all$meaninfo$all$mean,
        mean(x)
    )
    expect_null(pl$all$all$boxinfo)

    pl <- iNZightPlot(x,
        plot = interactive(),
        inference.par = "median",
        inference.type = "conf",
        mean_indicator = TRUE,
        boxplot = FALSE
    )
    # 1.5 * IQR / sqrt(N)
    xbar <- median(x)
    wd <- 1.5 * as.numeric(diff(quantile(x, c(0.25, 0.75)))) / sqrt(length(x))
    expect_equal(
        as.numeric(pl$all$all$inference.info$median$conf),
        c(xbar - wd, xbar + wd, xbar)
    )
    expect_null(pl$all$all$meaninfo)
    expect_equal(
        pl$all$all$boxinfo$all$quantiles,
        quantile(x, c(0.25, 0.5, 0.75))
    )

    ### Bootstraps
    # pl <- iNZightPlot(x,
    #     plot = FALSE,
    #     inference.par = "iqr",
    #     inference.type = "conf"
    # )

})

test_that("Dot plot with single unique value", {
    p <- inzplot(~x, data = data.frame(x = rep(10, 10)))
    expect_is(p, "inzplotoutput")
})

test_that("Transformations for dot plots", {
    inzplot(~Sepal.Length, data = iris, transform = list(x = "log"))
})

test_that("Confidence level can be adjusted", {
    # single x
    p <- inzplot(~Sepal.Length, data = iris,
        inference.type = "conf", ci.width = 0.9,
        inference.par = "mean", plot = FALSE)
    t <- t.test(iris$Sepal.Length,
        conf.level = 0.9)
    expect_equivalent(
        p$all$all$inf$mean$conf,
        c(t$conf.int, t$estimate)
    )

    # by factor
    p <- inzplot(Sepal.Length ~ Species, data = iris,
        inference.type = "conf", ci.width = 0.9,
        inference.par = "mean", plot = FALSE)
    t <- tapply(iris$Sepal.Length, iris$Species,
        t.test, conf.level = 0.9)
    expect_equivalent(
        p$all$all$inf$mean$conf,
        t(sapply(t, function(x) c(x$conf.int, x$estimate)))
    )
})
