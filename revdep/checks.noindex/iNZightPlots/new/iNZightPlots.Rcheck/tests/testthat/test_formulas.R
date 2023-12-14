context("Formula interface")

test_that("Formula interface works", {
    expect_is(
        inzplot(~Sepal.Width, data = iris),
        "inzplotoutput"
    )
    expect_is(
        inzplot(Sepal.Width ~ Sepal.Length, data = iris),
        "inzplotoutput"
    )

    expect_is(
        inzplot(Sepal.Width ~ Sepal.Length | Species, data = iris),
        "inzplotoutput"
    )

    expression(
        inzplot(Sepal.Width ~ . | Species, data = iris),
        "inzplotoutput"
    )
})

test_that("Formula yields same results", {
    expect_equal(
        inzplot(~Species, data = iris),
        iNZightPlot(Species, data = iris)
    )
    expect_equal(
        inzplot(Sepal.Width ~ Species, data = iris),
        iNZightPlot(Sepal.Width, Species, data = iris)
    )
    expect_equal(
        inzplot(Sepal.Width ~ Sepal.Length | Species, data = iris,
            plot.features = list(order.first = -1)),
        iNZightPlot(Sepal.Length, Sepal.Width, g1 = Species, data = iris,
            plot.features = list(order.first = -1))
    )
})

test_that("Additional variables are parsed correctly", {
    expect_equal(
        inzplot(~Species, data = iris, colby = Species),
        iNZightPlot(Species, data = iris, colby = Species)
    )
    expect_is(
        inzplot(Sepal.Length ~ Sepal.Width,
            data = iris,
            sizeby = Petal.Length,
            colby = Species,
            trend = "linear",
            trend.by = TRUE
        ),
        "inzplotoutput"
    )
})
