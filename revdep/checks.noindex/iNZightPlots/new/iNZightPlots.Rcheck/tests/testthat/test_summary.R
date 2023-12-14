context("Get Summary")

skip_if_offline()

cas <- read.csv("cas.csv", stringsAsFactors = TRUE)

test_that("One-way table summaries are correct", {
    p <- getPlotSummary(travel, data = cas)
    p <- p[which(grepl("Summary of the distribution", p)) + 3:5]
    expect_equal(
        scan(text = p[1], what = character(), quiet = TRUE),
        c(levels(cas$travel), "Total")
    )
    expect_equivalent(
        scan(text = gsub("Count", "", p[2]), what = integer(), quiet = TRUE),
        c(table(cas$travel), nrow(cas))
    )
    expect_equivalent(
        scan(text = gsub("Percent|%", "", p[3]), what = double(), quiet = TRUE),
        c(table(cas$travel), nrow(cas)) / nrow(cas) * 100
    )
})

test_that("Vertical tables supported", {
    p <- inzsummary(~travel, data = cas, table.direction = "vertical")
    expect_match(
        p[which(grepl("Summary of the distribution", p)) + 3],
        "\\sCount\\s+Percent"
    )
    expect_match(
        p[which(grepl("Summary of the distribution", p)) + 10],
        "-+"
    )

    p2 <- inzsummary(travel~gender, data = cas, table.direction = "vertical")
    expect_match(
        p2[grep("Summary of the distribution", p2)],
        ".+travel \\(rows\\) by gender \\(columns\\).+"
    )
    expect_match(
        p2[grep("Table of Counts", p2) + 2L],
        "\\sfemale\\s+male"
    )
    expect_match(p2, "Column N", all = FALSE)
})


data(api, package = "survey")
dclus2<-svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)

test_that("Survey summaries are correct", {
    expect_is(getPlotSummary(api00, design = dclus2),
        "inzight.plotsummary")
    expect_is(getPlotSummary(api00, sch.wide, design = dclus2),
        "inzight.plotsummary")
    expect_is(getPlotSummary(api00, api99, design = dclus2),
        "inzight.plotsummary")
    expect_is(getPlotSummary(sch.wide, design = dclus2),
        "inzight.plotsummary")
    expect_is(getPlotSummary(sch.wide, awards, design = dclus2),
        "inzight.plotsummary")

})

chis <- try(iNZightTools::smart_read("https://inzight.nz/testdata/chis2.csv"), silent = TRUE)
skip_if(inherits(chis, "try-error"), "Unable to load resource")

dchis <- suppressWarnings(svrepdesign(
    data = chis,
    repweights = "rakedw[1-9]",
    weights = ~rakedw0,
    type = "other", scale = 1, rscales = 1
))
test_that("Survey replicate design summaries are correct", {
    expect_is(getPlotSummary(bmi_p, design = dchis), "inzight.plotsummary")
    expect_is(getPlotSummary(bmi_p, sex, design = dchis), "inzight.plotsummary")
    expect_is(suppressWarnings(getPlotSummary(bmi_p, marit, design = dchis)),
        "inzight.plotsummary")
    expect_is(getPlotSummary(sex, design = dchis), "inzight.plotsummary")
    expect_is(getPlotSummary(sex, smoke, design = dchis), "inzight.plotsummary")
})


test_that("Regression summary: negative coefficients", {
    o <- capture.output(
        getPlotSummary(Sepal.Length, Sepal.Width,
            data = iris, trend = "linear", width = 80)
    )
    expect_match(o, "Sepal.Width = 3.419 - 0.06188 * Sepal.Length",
        fixed = TRUE,
        all = FALSE
    )
})


test_that("inzsummary gives the same output", {
    expect_equal(
        inzsummary(Sepal.Length ~ Sepal.Width,
            data = iris, trend = "linear", width = 80),
        getPlotSummary(Sepal.Width, Sepal.Length,
            data = iris, trend = "linear", width = 80)
    )

    expect_equal(
        inzsummary(Sepal.Length ~ Sepal.Width | Species,
            data = iris, trend = "linear", width = 80),
        getPlotSummary(Sepal.Width, Sepal.Length, g1 =  Species,
            data = iris, trend = "linear", width = 80)
    )
})
