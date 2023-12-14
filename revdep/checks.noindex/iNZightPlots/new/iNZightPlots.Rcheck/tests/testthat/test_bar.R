context("Bar plots (one way)")

set.seed(251090)
df <- data.frame(
    x = sample(LETTERS[1:3], 100, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    y = rep(c("G1", "G2"), each = 50),
    stringsAsFactors = TRUE
)
tab <- table(df$x)
pr <- tab / nrow(df)
# CI width
wd <- as.numeric(qnorm(0.975) * sqrt(pr * (1 - pr) / nrow(df)))

plotit <- FALSE

bar1 <- iNZightPlot(x, data = df,
    plot = plotit,
)

bar1_counts <- iNZightPlot(x, data = df,
    plot = plotit,
    bar.counts = TRUE
)

bar2 <- iNZightPlot(x, colby = y, data = df,
    plot = plotit,
)

bar2_counts <- iNZightPlot(x, colby = y, data = df,
    plot = plotit,
    bar.counts = TRUE
)

bar1inf <- iNZightPlot(x, data = df,
    plot = plotit,
    inference.type = "conf",
    inference.par = "proportion"
)

bar1inf_counts <- iNZightPlot(x, data = df,
    plot = plotit,
    inference.type = "conf",
    inference.par = "proportion",
    bar.counts = TRUE
)

bar2inf <- iNZightPlot(x, colby = y, data = df,
    plot = plotit,
    inference.type = "conf",
    inference.par = "proportion",
)

bar2inf_counts <- iNZightPlot(x, colby = y, data = df,
    plot = plotit,
    inference.type = "conf",
    inference.par = "proportion",
    bar.counts = TRUE
)

test_that("Y axis limits computed correctly", {
    expect_equal(bar1$all$all$ylim, c(0, max(pr)))
    expect_equal(bar1_counts$all$all$ylim, c(0, max(pr) * nrow(df)))
    expect_equal(bar2$all$all$ylim, c(0, max(pr)))
    expect_equal(bar2_counts$all$all$ylim, c(0, max(pr) * nrow(df)))

    expect_equal(bar1inf$all$all$ylim, c(0, max(pr + wd)))
    expect_equal(bar1inf_counts$all$all$ylim, c(0, max(pr) * nrow(df)))
    expect_equal(bar2inf$all$all$ylim, c(0, max(pr)))
    expect_equal(bar2inf_counts$all$all$ylim, c(0, max(pr) * nrow(df)))
})

test_that("Y axis is labelled correctly", {
    iNZightPlot(x, data = df)
    expect_equal(grid.get("inz-ylab")$label, "Percentage (%)")
    expect_equal(
        grid.get("inz-yaxis-left.1.1")$label,
        format(seq(10, 50, by = 10))
    )
})

test_that("Inference information is correct", {
    inf <- list(
        lower = unclass(t(pr - wd)),
        upper = unclass(t(pr + wd)),
        estimate = t(as.numeric(pr))
    )

    # expect_equal(bar1inf$all$all$inference.info$conf, inf)
    # expect_equal(bar1inf_counts$all$all$inference.info$conf,
    #     lapply(inf, function(x) x * nrow(df))
    # )


    # counts and colby argument should turn off inference
    expect_null(bar1inf_counts$all$all$inference.info)
    expect_null(bar2inf$all$all$inference.info)
    expect_null(bar2inf_counts$all$all$inference.info)
})
