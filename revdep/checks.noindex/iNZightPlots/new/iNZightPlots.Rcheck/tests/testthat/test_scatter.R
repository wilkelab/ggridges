context("Scatter plots")

test_that("Colour by works", {
    expect_is(
        iNZightPlot(Sepal.Width, Sepal.Length,
            colby = Species, data = iris,
            plot = FALSE),
        "inzplotoutput"
    )
    expect_is(
        iNZightPlot(Sepal.Width, Sepal.Length,
            colby = Petal.Length, data = iris,
            plot = FALSE),
        "inzplotoutput"
    )
    expect_is(
        iNZightPlot(Sepal.Width, Sepal.Length,
            colby = Petal.Length, col.method = "rank", data = iris,
            plot = FALSE),
        "inzplotoutput"
    )
})

test_that("Adding inference information", {
    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris,
        trend = c("linear", "quadratic", "cubic"),
        inference.type = "conf",
        plot = FALSE
    )
    expect_is(p, "inzplotoutput")

    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris,
        colby = Species,
        trend = "linear",
        trend.by = TRUE, trend.parallel = FALSE,
        inference.type = "conf",
        plot = FALSE
    )
    expect_is(p, "inzplotoutput")

    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris,
        colby = Species,
        trend = "linear",
        trend.by = TRUE, trend.parallel = TRUE,
        inference.type = "conf",
        plot = FALSE
    )
    expect_is(p, "inzplotoutput")


    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris,
        trend = "linear",
        inference.type = "conf",
        bs.inference = TRUE,
        plot = FALSE
    )
    expect_is(p, "inzplotoutput")

    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris,
        colby = Species,
        trend = "linear",
        trend.by = TRUE, trend.parallel = FALSE,
        inference.type = "conf",
        bs.inference = TRUE,
        plot = FALSE
    )
    expect_is(p, "inzplotoutput")

    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris,
        colby = Species,
        trend = "linear",
        trend.by = TRUE, trend.parallel = TRUE,
        inference.type = "conf",
        bs.inference = TRUE,
        plot = FALSE
    )
    expect_is(p, "inzplotoutput")
})

test_that("Scatter plot with single unique x/y value", {
    d <- data.frame(x = rep(10, 10), y = rnorm(10))
    expect_is(inzplot(y ~ x, data = d, plot = FALSE), "inzplotoutput")
    expect_is(inzplot(x ~ y, data = d, plot = FALSE), "inzplotoutput")
})
