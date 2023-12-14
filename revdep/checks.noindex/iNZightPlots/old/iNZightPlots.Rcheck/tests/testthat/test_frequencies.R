context("Frequency counts (one- and two-way tables)")

cas <- suppressMessages(iNZightTools::smart_read("cas.csv"))
cas_freq <- suppressMessages(iNZightTools::smart_read("cas_freq.csv"))
# cas <- suppressMessages(iNZightTools::smart_read("tests/testthat/cas.csv"))
# cas_freq <- suppressMessages(iNZightTools::smart_read("tests/testthat/cas_freq.csv"))

test_that("One way tables give the same results", {
    p0 <- iNZightPlot(travel, data = cas, inference.type = "conf")
    expect_is(p0, "inzplotoutput")

    p1 <- iNZightPlot(travel, data = cas_freq, freq = count,
        inference.type = "conf")
    expect_is(p1, "inzplotoutput")

    expect_equivalent(p0$all$all$phat, p1$all$all$phat)

    expect_equivalent(
        p0$all$all$inference$conf,
        p1$all$all$inference$conf
    )
})


test_that("Two way tables give the same results", {
    p0 <- iNZightPlot(travel, gender, data = cas, inference.type = "conf")
    expect_is(p0, "inzplotoutput")
    p1 <- iNZightPlot(travel, gender, data = cas_freq, freq = count,
        inference.type = "conf")
    expect_is(p1, "inzplotoutput")

    expect_equivalent(
        unclass(p0$all$all$phat),
        unclass(p1$all$all$phat)
    )

    expect_equivalent(
        lapply(p0$all$all$inference$conf, unclass),
        lapply(p1$all$all$inference$conf, unclass)
    )
})

test_that("Segmented bar charts are correct", {
    p0 <- iNZightPlot(travel, colby = gender, data = cas)
    expect_is(p0, "inzplotoutput")
    p1 <- iNZightPlot(travel, colby = gender, data = cas_freq, freq = count)
    expect_is(p1, "inzplotoutput")

    expect_equivalent(
        p0$all$all$p.colby,
        p1$all$all$p.colby
    )
})

test_that("Inference information works", {
    expect_is(
        getPlotSummary(cas_freq$travel,
            freq = cas_freq$count,
            summary.type = "inference",
            inference.type = "conf"
        ),
        "inzight.plotsummary"
    )
})
