context("Axis transformations")

df <- data.frame(x = rnorm(100, 0, 3), stringsAsFactors = TRUE)
df$y <- 0.2 * df$x + rnorm(100, 5, 1)
df$ex <- 10^df$x
df$ey <- 10^df$y

plotit <- FALSE

px <- iNZightPlot(x, data = df,
    plot = plotit,
    inference.par = "median",
    inference.type = "conf"
)
plex <- iNZightPlot(ex, data = df,
    plot = plotit,
    transform = list(x = "log10"),
    inference.par = "median",
    inference.type = "conf"
)

pxy <- iNZightPlot(x, y, data = df,
    plot = plotit,
    trend = "linear"
)
plexley <- iNZightPlot(ex, ey, data = df,
    plot = plotit,
    transform = list(x = "log10", y = "log10"),
    trend = "linear"
)

test_that("Log 10 transform", {
    expect_equal(px$all$all$xlim, plex$all$all$xlim)
    expect_equal(
        px$all$all$boxinfo$all$quantiles,
        plex$all$all$boxinfo$all$quantiles
    )
    expect_equal(
        px$all$all$boxinfo$all$min,
        plex$all$all$boxinfo$all$min
    )
    expect_equal(
        px$all$all$boxinfo$all$max,
        plex$all$all$boxinfo$all$max
    )

    expect_equal(pxy$all$all$xlim, plexley$all$all$xlim)
    expect_equal(pxy$all$all$ylim, plexley$all$all$ylim)

})

test_that("Dot plot inferencial markup is accurate", {
    expect_equal(
        px$all$all$inference.info$median$conf,
        plex$all$all$inference.info$median$conf
    )
})

# test_that("Scatter plot trend lines are OK", {

# })
