context("Bar plots (two way)")

set.seed(251090)
df <- data.frame(
    x = sample(LETTERS[1:3], 100, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    y = rep(c("G1", "G2"), c(30, 70)),
    stringsAsFactors = TRUE
)
tab <- t(table(df$x, df$y))
pr <- sweep(tab, 1, rowSums(tab), "/")
# CI width
wd <- unclass(qnorm(0.975) * sqrt(
    sweep(pr * (1 - pr), 1, rowSums(tab), "/")
))

plotit <- FALSE

bar <- iNZightPlot(x, y, data = df,
    plot = plotit,
)

bar_counts <- iNZightPlot(x, y, data = df,
    plot = plotit,
    bar.counts = TRUE
)

barinf <- iNZightPlot(x, y, data = df,
    plot = plotit,
    inference.type = c("comp", "conf"),
    inference.par = "proportion"
)

barinf_counts <- iNZightPlot(x, y, data = df,
    plot = plotit,
    inference.type = c("comp", "conf"),
    inference.par = "proportion",
    bar.counts = TRUE
)

test_that("Y axis limits computed correctly", {
    expect_equal(bar$all$all$ylim, c(0, max(pr)))
    expect_equal(bar_counts$all$all$ylim,
        c(0, max(sweep(pr, 1, rowSums(tab), "*")))
    )

    expect_equal(barinf$all$all$ylim, c(0, max(pr + wd)))
    expect_equal(barinf_counts$all$all$ylim,
        c(0, max(sweep(pr, 1, rowSums(tab), "*")))
    )
})

test_that("Inference information is correct", {
    conf <- list(
        lower = unclass(pr - wd),
        upper = unclass(pr + wd),
        estimate = unclass(pr)
    )
    comp <- lapply(1:ncol(tab), function(i) {
        moecalc(seBinprops(rowSums(tab), pr[, i]), est = pr[, i])
    })
    names(comp) <- colnames(tab)
    comp <- lapply(list(
            lower = sapply(comp, function(x) x$compL),
            upper = sapply(comp, function(x) x$compU)
        ),
        function(c) {
            names(dimnames(c)) <- c("", "")
            c
        }
    )

    expect_equal(barinf$all$all$inference.info$conf, conf)

    expect_equal(barinf$all$all$inference.info$comp, comp)
    # expect_equal(barinf_counts$all$all$inference.info$conf,
    #     lapply(conf, function(x) sweep(x, 1, rowSums(tab), "*"))
    # )

    # counts should turn off interence
    expect_null(barinf_counts$all$all$inference.info$conf)
    expect_null(barinf_counts$all$all$inference.info$comp)
})

cas <- read.csv("cas.csv", stringsAsFactors = TRUE)
test_that("Subsetting works", {
    expect_is(
        iNZightPlot(travel, gender, g1 = cellsource, g2 = getlunch,
            g2.level = "_MULTI", data = cas),
        "inzplotoutput"
    )
    expect_is(
        iNZightPlot(travel, gender, g1 = cellsource, data = cas),
        "inzplotoutput"
    )
    expect_is(
        iNZightPlot(travel, gender, g1 = cellsource, data = cas,
            bar.counts = TRUE),
        "inzplotoutput"
    )
    expect_is(
        iNZightPlot(travel, g1 = cellsource, data = cas),
        "inzplotoutput"
    )
    expect_is(
        iNZightPlot(travel, g1 = cellsource, data = cas,
            bar.counts = TRUE),
        "inzplotoutput"
    )
})
