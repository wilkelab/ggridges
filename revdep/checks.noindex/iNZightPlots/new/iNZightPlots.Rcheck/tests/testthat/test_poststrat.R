context("Post stratification weights")

data(api, package = "survey")
dclus1 <- svydesign(id = ~dnum, weights = ~pw,
    data = apiclus1, fpc = ~fpc)
pop.types <- data.frame(
    stype = c("E","H","M"),
    Freq = c(4421,755,1018),
    stringsAsFactors = TRUE
)
dclus1p <- postStratify(dclus1, ~stype, pop.types)

# load_all()
# iNZightPlot(api00, api99, design = dclus1)

test_that("Post strat designs work", {
    expect_is(
        iNZightPlot(api00, api99, design = dclus1p, plot = FALSE),
        "inzplotoutput"
    )
})
