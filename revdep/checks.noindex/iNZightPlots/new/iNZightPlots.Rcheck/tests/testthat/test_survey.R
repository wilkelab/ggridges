context("Survey designs")

data(api, package = "survey")
dclus1 <- svydesign(id = ~dnum, weights = ~pw,
    data = apiclus1, fpc = ~fpc)

test_that("Survey designs work", {
    expect_is(
        iNZightPlot(api00, api99, design = dclus1, plot = FALSE),
        "inzplotoutput"
    )
})

test_that("Summary information is correct - dot plot", {
    x <- getPlotSummary(enroll, design = dclus1)
    pe <- which(grepl("Population estimates", x)) + 3

    xpe <- gsub("\\|", "", x[pe])
    expect_equal(
        round(scan(textConnection(xpe), quiet = TRUE))[-(1:3)],
        round(c(
            # as.numeric(
            #     svyquantile(~enroll,
            #         design = dclus1, quantiles = c(0.25, 0.5, 0.75)
            #     )
            # ),
            as.numeric(svymean(~enroll, design = dclus1)),
            sqrt(as.numeric(svyvar(~enroll, design = dclus1))),
            as.numeric(svytotal(~enroll, design = dclus1)),
            coef(svytotal(cbind(rep(1, nrow(dclus1$variables))), dclus1)),
            nrow(apiclus1),
            min(apiclus1$enroll),
            max(apiclus1$enroll)
        ))
    )

    ## standard errors ...
    # ...
    se <- which(grepl("Standard error of estimates", x)) + 2
    xse <- gsub("\\|", "", x[se])
    expect_equivalent(
        round(
            scan(text = xse, quiet = TRUE),
            c(2, 2, 2, 2, 2, 0, 0)
        )[-(1:3)],
        round(
            c(
                # SE(svyquantile(~enroll,
                #     design = dclus1, quantiles = c(0.25, 0.5, 0.75),
                #     se = TRUE
                # )),
                SE(svymean(~enroll, design = dclus1)),
                sqrt(
                    vcov(svyvar(~enroll, dclus1)) /
                        4 / coef(svyvar(~enroll, dclus1))
                ),
                SE(svytotal(~enroll, design = dclus1)),
                SE(svytotal(cbind(rep(1, nrow(dclus1$variables))), dclus1))
            ),
            c(2, 2, 0, 0)
        )
    )
})

test_that("Summary information is correct - dot plot (by factor)", {
    x <- getPlotSummary(enroll, stype, design = dclus1)
    pe <- which(grepl("Population estimates", x)) + 3:5
    xpe <- gsub("\\||[A-Z]", "", x[pe])
    expect_equivalent(
        round(do.call(
            rbind,
            lapply(xpe, function(z) round(scan(textConnection(z), quiet = TRUE)))
        ))[,-(1:3)],
        round(cbind(
            # as.matrix(svyby(~enroll, ~stype, dclus1, svyquantile, keep.var = FALSE,
            #     quantiles = c(0.25, 0.5, 0.75))[,-1]),
            mean=coef(svyby(~enroll, ~stype, dclus1, svymean)),
            sd=sqrt(coef(svyby(~enroll, ~stype, dclus1, svyvar))),
            total=coef(svyby(~enroll, ~stype, dclus1, svytotal)),
            pop=tapply(weights(dclus1), dclus1$variables$stype, sum),
            n=table(dclus1$variables$stype),
            min=tapply(dclus1$variables$enroll, dclus1$variables$stype, min),
            max=tapply(dclus1$variables$enroll, dclus1$variables$stype, max)
        ))
    )

    ## standard errors
    se <- which(grepl("Standard error of estimates", x)) + 2:4
    xse <- gsub("\\||[A-Z]", "", x[se])
    expect_equivalent(
        do.call(rbind,
            lapply(xse, function(z)
                round(
                    scan(text = z, quiet = TRUE),
                    c(2, 2, 2, 2, 2, 0, 1)
                )
            )
        )[,-(1:3)],
        as.matrix(cbind(
            # suppressWarnings(
            #     round(SE(svyby(~enroll, ~stype, dclus1, svyquantile,
            #         ci = TRUE, se = TRUE,
            #         quantiles = c(0.25, 0.5, 0.75))), 2)
            # ),
            mean = round(SE(svyby(~enroll, ~stype, dclus1, svymean)), 2),
            sd = {
                vv <- svyby(~enroll, ~stype, dclus1, svyvar)
                vc <- suppressWarnings(diag(vcov(vv)))
                round(sqrt(vc / 4 / coef(vv)), 2)
            },
            total = round(SE(svyby(~enroll, ~stype, dclus1, svytotal))),
            pop = round(SE(svytotal(~stype, dclus1)), 1)
        ))
    )
})

test_that("Design effects are included - numeric", {
    x <- getPlotSummary(enroll, design = dclus1,
        survey.options = list(deff = TRUE))
    de <- which(grepl("Design effects", x)) + 2
    xde <- gsub("\\||[A-Z]", "", x[de])
    expect_equivalent(
        round(scan(textConnection(xde), quiet = TRUE)),
        round(c(
            mean=deff(svymean(~enroll, dclus1, deff = TRUE)),
            total=deff(svytotal(~enroll, dclus1, deff = TRUE))
        ))
    )
})

test_that("Design effects are included - numeric x categorical", {
    x <- getPlotSummary(enroll, stype, design = dclus1,
        survey.options = list(deff = TRUE))
    de <- which(grepl("Design effects", x)) + 2:4
    xde <- gsub("\\||[A-Z]", "", x[de])
    expect_equivalent(
        round(do.call(
            rbind,
            lapply(xde, function(z) round(scan(textConnection(z), quiet = TRUE)))
        )),
        round(cbind(
            mean=deff(svyby(~enroll, ~stype, dclus1, svymean, deff = TRUE)),
            total=deff(svyby(~enroll, ~stype, dclus1, svytotal, deff = TRUE))
        ))
    )
})


# test_that("Design effects are included - numeric x numeric", {
#     x <- getPlotSummary(api00, api99, design = dclus1,
#         trend = "linear",
#         survey.options = list(deff = TRUE))
#     # de <- which(grepl("Design effects", x)) + 2:4
#     # xde <- gsub("\\||[A-Z]", "", x[de])
#     # expect_equivalent(
#     #     round(do.call(
#     #         rbind,
#     #         lapply(xde, function(z) round(scan(textConnection(z), quiet = TRUE)))
#     #     )),
#     #     round(cbind(
#     #         mean=deff(svyby(~enroll, ~stype, dclus1, svymean, deff = TRUE)),
#     #         total=deff(svyby(~enroll, ~stype, dclus1, svytotal, deff = TRUE))
#     #     ))
#     # )
# })


test_that("Design effects are included - categorical", {
    x <- getPlotSummary(stype, design = dclus1,
        survey.options = list(deff = TRUE))
    de <- which(grepl("Design effects", x))
    xde <- gsub("\\||Design effects", "", x[de])
    expect_equivalent(
        round(scan(textConnection(xde), quiet = TRUE), 2),
        round(
            deff(svymean(~stype, dclus1, deff = TRUE)),
            2
        )
    )
})

test_that("Design effects are included - categorical x categorical", {
    x <- getPlotSummary(stype, awards, design = dclus1,
            survey.options = list(deff = TRUE))
    de <- which(grepl("Design effects", x)) + 3:4
    xde <- gsub("\\||[A-Za-z]", "", x[de])
    expect_equivalent(
        round(do.call(
            rbind,
            lapply(xde, function(z)
                round(scan(textConnection(z), quiet = TRUE), 2))
        ), 2),
        round(as.matrix(
            deff(svyby(~stype, ~awards, dclus1, svymean, deff=T))
        ), 2)
    )
})

test_that("Scatter plots work for surveys", {
    skip_if_offline()
    nhanes <- try(
        suppressWarnings(
            iNZightTools::smart_read("https://inzight.nz/testdata/nhanes.csv") %>%
                dplyr::mutate(
                    Gender.cat = ifelse(Gender == 1, "Male", "Female"),
                    Education.cat = as.factor(Education)
                )
        )
    )
    skip_if(inherits(nhanes, "try-error"), "Unable to load resource")

    nhanes.svy <- svydesign(~SDMVPSU, strata = ~SDMVSTRA,
        weights = ~WTINT2YR, data = nhanes, nest = TRUE)

    px <- inzplot(Weight ~ Height, design = nhanes.svy, plot = FALSE)

    sx <- nhanes.svy$variables$Height
    sx <- sx[!is.na(sx) & !is.na(nhanes.svy$variables$Weight)]
    expect_equal(px$all$all$x, sx)

    tp <- tempfile(fileext=".pdf")
    on.exit(unlink(tp))
    pdf(tp)
    expect_is(
        inzplot(Weight ~ Height, design = nhanes.svy, plot = TRUE,
            smooth = 0.8),
        "inzplotoutput"
    )
})

test_that("Factors with one level return error", {
    dclus1$variables$test <- factor(rep("test", nrow(dclus1$variables)))
    dclus1$variables$test2 <- factor(rep("test", nrow(dclus1$variables)))
    expect_error(inzplot(test ~ api00, design = dclus1, plot = FALSE))
})

test_that("Log transformation works with surveys", {
    expect_is(
        inzplot(~meals, design = dclus1, transform = list(x = "log"),
            plot = FALSE
        ),
        "inzplotoutput"
    )
})

test_that("SRS fpc-only works", {
    srs_data <- data.frame(
        v = sample(5:20, size = 20, replace = TRUE),
        y = 100
    )
    srs_des <- svydesign(~1, fpc = ~y, data = srs_data)

    expect_is(inzplot(~v, design = srs_des), "inzplotoutput")
})
