context("Survey methods")

skip_if_offline()

data(api, package = "survey")
dclus2 <- svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)

data(scd, package = "survey")
repweights <- 2 *
    cbind(
        c(1,0,1,0,1,0),
        c(1,0,0,1,0,1),
        c(0,1,1,0,0,1),
        c(0,1,0,1,1,0)
    )
scdrep <- suppressWarnings(
    svrepdesign(data = scd, type = "BRR", repweights = repweights,
        combined.weights = FALSE)
)

test_that("Survey calls are correctly modified", {
    expect_equal(
        modifyCall(dclus2$call, "data", "newData"),
        "svydesign(id = ~dnum + snum, fpc = ~fpc1 + fpc2, data = newData)"
    )

    expect_equal(
        modifyCall(scdrep$call, "data", "newData"),
        "survey:::svrepdesign.default(data = newData, type = \"BRR\", repweights = repweights, combined.weights = FALSE)"
    )
})

test_that("Mean indicator uses correct weights", {
    expect_is(
        p <- inzplot(~api00, design = dclus2, mean_indicator = TRUE, plot = FALSE),
        "inzplotoutput"
    )
    expect_equivalent(p$all$all$meaninfo$all$mean, svymean(~api00, dclus2))
})

chis <- try(iNZightTools::smart_read("https://inzight.nz/testdata/chis2.csv"), silent = TRUE)
skip_if(inherits(chis, "try-error"), "Unable to load resource")

dchis <- suppressWarnings(svrepdesign(
    data = chis,
    repweights = "rakedw[1-9]",
    weights = ~rakedw0,
    type = "other", scale = 1, rscales = 1
))

test_that("Subsetting replicate weight surveys is correct", {
    dchis2 <- update(dchis,
        f = factor(ifelse(dchis$variables$sex == "male", dchis$variables$race, NA)),
        n = ifelse(dchis$variables$sex == "male", rnorm(nrow(dchis$variables)), NA),
        n2 = rnorm(nrow(dchis$variables)),
        n3 = n
    )

    expect_is(inzplot(~f | sex, data = dchis2, plot = FALSE), "inzplotoutput")
    expect_is(inzplot(ab30 ~ f | sex, data = dchis2, plot = FALSE), "inzplotoutput")
    expect_is(inzplot(n2 ~ n | sex, data = dchis2, plot = FALSE), "inzplotoutput")
    expect_is(inzplot(n3 ~ n | sex, data = dchis2, plot = FALSE), "inzplotoutput")
})
