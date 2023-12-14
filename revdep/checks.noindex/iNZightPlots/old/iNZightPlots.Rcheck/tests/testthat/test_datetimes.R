context("Dates and times")

quakes <- iNZightTools::smart_read("quakes.csv")
# quakes <- iNZightTools::smart_read("tests/testthat/quakes.csv")
quakes$felt <- as.factor(sample(c("yes", "no"), nrow(quakes), replace = TRUE))

test_that("Datetimes plot OK", {
    expect_is(iNZightPlot(origintime, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(origintime, magnitude, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(magnitude, origintime, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(origintime, felt, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(felt, origintime, data = quakes), "inzplotoutput")

    expect_is(
        iNZightPlot(magnitude, g1 = origintime, data = quakes),
        "inzplotoutput"
    )
    expect_is(
        iNZightPlot(magnitude,
            g1 = felt,
            g2 = origintime,
            g2.level = "_MULTI",
            data = quakes
        ),
        "inzplotoutput"
    )
})

quakes <- iNZightTools::extract_part(quakes, "origintime", "Date only", "date")
test_that("Dates plot OK", {
    expect_is(iNZightPlot(date, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(date, magnitude, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(magnitude, date, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(date, felt, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(felt, date, data = quakes), "inzplotoutput")

    expect_is(
        iNZightPlot(magnitude, g1 = date, data = quakes),
        "inzplotoutput"
    )
    expect_is(
        iNZightPlot(magnitude,
            g1 = felt,
            g2 = date,
            g2.level = "_MULTI",
            data = quakes
        ),
        "inzplotoutput"
    )
})

quakes <- iNZightTools::extract_part(quakes, "origintime", "Time only", "time")
test_that("Times plot OK", {
    expect_is(iNZightPlot(time, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(time, magnitude, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(magnitude, time, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(time, felt, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(felt, time, data = quakes), "inzplotoutput")

    skip_if(
        packageVersion("chron") <= numeric_version("2.3.57") &&
            getRversion() >= numeric_version("4.3")
    )
    expect_is(
        iNZightPlot(magnitude, g1 = time, data = quakes),
        "inzplotoutput"
    )
    expect_is(
        iNZightPlot(magnitude,
            g1 = felt,
            g2 = time,
            g2.level = "_MULTI",
            data = quakes
        ),
        "inzplotoutput"
    )
})

test_that("Summaries provide a reasonable summary", {
    dt.smry <- getPlotSummary(origintime, data = quakes)
    date.smry <- getPlotSummary(date, data = quakes)
    time.smry <- getPlotSummary(time, data = quakes)

    expect_is(dt.smry, "inzight.plotsummary")
    expect_is(date.smry, "inzight.plotsummary")
    expect_is(time.smry, "inzight.plotsummary")

    dti <- grep("Sample Size", dt.smry)

    dx <- strsplit(gsub("  +", "|", dt.smry[dti + 1]), "\\|")[[1]][-1]
    names(dx) <- strsplit(gsub("  +", "|", dt.smry[dti]), "\\|")[[1]][-1]
    expect_equal(
        names(dx),
        c("Start Time", "End Time", "Time Range", "Sample Size")
    )
    expect_equal(
        as.character(dx),
        c(
            as.character(range(quakes$origintime)),
            as.character(
                lubridate::seconds_to_period(
                    diff(as.integer(range(quakes$origintime)))
                )
            ),
            nrow(quakes)
        )
    )

    dti <- grep("Sample Size", date.smry)
    dx <- strsplit(gsub("  +", "|", date.smry[dti + 1]), "\\|")[[1]][-1]
    names(dx) <- strsplit(gsub("  +", "|", date.smry[dti]), "\\|")[[1]][-1]
    expect_equal(
        names(dx),
        c("Start Date", "End Date", "Date Range", "Sample Size")
    )
    expect_equal(
        as.character(dx),
        c(
            as.character(range(quakes$date)),
            sprintf("%i days", as.integer(diff(range(quakes$date)))),
            nrow(quakes)
        )
    )

    dti <- grep("Sample Size", time.smry)
    dx <- strsplit(gsub("  +", "|", time.smry[dti + 1]), "\\|")[[1]][-1]
    names(dx) <- strsplit(gsub("  +", "|", time.smry[dti]), "\\|")[[1]][-1]
    expect_equal(
        names(dx),
        c("Earliest Time", "Latest Time", "Sample Size")
    )
    expect_equal(
        as.character(dx),
        c(
            as.character(range(quakes$time)),
            nrow(quakes)
        )
    )
})

test_that("Axes switched correctly for dt/factor", {
    p1 <- iNZightPlot(time, felt, data = quakes)
    p2 <- iNZightPlot(felt, time, data = quakes)

    expect_equal(p1$gen$opts$transform$x, "time")
    expect_equal(p2$gen$opts$transform$x, "time")
})

test_that("HMS time values supported", {
    d <- data.frame(t = hms::parse_hms(c("12:00:00", "13:00:00")), x = 2:3)
    expect_is(inzplot(x ~ t, data = d), "inzplotoutput")
})
