context("Privacy controls")

test_that("Privacy control list created correctly", {
    pc <- make_privacy_controls(list(suppression = 10))
    expect_equal(
        names(pc),
        c("round", "suppression_matrix", "suppress",
            "suppress_quantile",
            "rse_matrix", "markup", "has", "get")
    )
    expect_is(pc$round, "function")
    expect_is(pc$suppression_matrix, "function")
    expect_is(pc$suppress, "function")
})

test_that("Suppression matrix created correctly", {
    pc <- make_privacy_controls(list(suppression = 10))

    tab <- as.table(c(A = 20, B = 30, C = 5))
    st <- pc$suppression_matrix(tab)
    expect_equivalent(st, c(FALSE, FALSE, TRUE, TRUE))
    expect_equivalent(
        pc$suppress(c(tab, sum(tab)), st),
        c("20", "30", "S", "S")
    )

    tab <- as.table(cbind(c(20, 30, 5), c(30, 40, 8), c(5, 20, 40)))
    st <- pc$suppression_matrix(tab)
    expect_equivalent(rowSums(st), c(2, 0, 2))
    expect_equivalent(
        pc$suppress(cbind(tab, rowSums(tab)), st),
        cbind(c(20, 30, "S"), c(30, 40, "S"), c("S", 20, 40), c("S", 90, 53))
    )
})

test_that("Rounding is correct", {
    pc <- make_privacy_controls(list(rounding = "RR3"))
    expect_true(
        all(pc$round(sample(0:10, 100L, TRUE)) %% 3L == 0)
    )

    pc <- make_privacy_controls(list(rounding = "GRR"))
    expect_true(
        all(
            pc$round(c(0, 5, 18, 25, 100, 123, 898, 2041)) %%
                c(3, 3, 3, 5, 10, 10, 10, 100) == 0L
        )
    )

    pc <- make_privacy_controls(list(rounding = 100L))
    expect_true(
        all(
            pc$round(sample(1e5, 100)) %% 100L == 0L
        )
    )
})

test_that("RSE checking/annotation/suppression", {
    # devtools::load_all()
    pc <- make_privacy_controls(
        list(
            check_rse = list(
                cut = c(50, 100),
                output = c(" *", "suppress")
            )
        )
    )
    x <- matrix(c(50, 5, 10, 200), nrow = 2L)
    e <- matrix(c(20, 4, 12, 20), nrow = 2L)
    m <- pc$rse_matrix(x, e)
    expect_equal(
        as.character(m),
        c(NA_character_, " *", "suppress", NA_character_, NA_character_, NA_character_)
    )

    ms <- pc$suppression_matrix(x) | !(is.na(m) | m != "suppress")
    x <- cbind(x, rowSums(x))
    x <- pc$suppress(x, ms)
    r <- pc$markup(x, m)
    expect_equal(
        as.character(r),
        c("50", "5 *", "S", "200", "60", "205")
    )
})

library(survey)
data(api)
dclus2 <- svydesign(~dnum+snum, fpc=~fpc1+fpc2, data = apiclus2)

## Some specific examples
# Stats NZ Datalab Surveys
test_that("Unweighted survey counts", {
    # devtools::load_all()
    inf <- inzsummary(sch.wide ~ both,
        data = apiclus2,
        privacy_controls = list(rounding = "RR3")
    )

    # 4.1.1 - empty cells as zero, unless suppress required
    expect_match(
        inf[grep("Table of Counts", inf) + 4L],
        "Yes\\s+0"
    )

    # 4.1.2 - manual check

    # 4.1.3 - randomly round all counts to base 3
    tb <- read.table(
        textConnection(inf[grep("Table of Counts", inf) + 3:4]),
    )
    expect_true(all(tb[,-1] %% 3L == 0L))
})

test_that("Weighted survey counts", {
    # devtools::load_all()
    inf <- inzsummary(~ stype,
        design = dclus2,
        privacy_controls = list(
            rounding = 100L,
            suppression = 700L,
            secondary_suppression = FALSE
        )
    )

    # 4.2.1 - rounded to fixed base, fixed threshold; no secondary suppression
    tbl <- read.table(
        textConnection(
            inf[grep("Population Estimates", inf) + 3L]
        )
    )
    expect_true(all(tbl[c(2, 4:5)] %% 100 == 0L))
    expect_equivalent(tbl[3], "S")

    # 4.2.2 - identify or suppress high estimates with high RSE
    # devtools::load_all()

    inf <- inzsummary(sch.wide ~ stype,
        design = dclus2,
        table.direction = "v",
        privacy_controls = list(
            # rounding = 100L,
            check_rse = list(
                cut = c(20, 30, 50),
                output = c(" *", " **", "suppress")
            )
        )
    )
    expect_match(inf, "242 **", fixed = TRUE, all = FALSE)
    expect_match(inf, "689 *", fixed = TRUE, all = FALSE)

    # 4.2.4 - suppress weighted counts based on unweighted count threshold
    # devtools::load_all()
    inf <- inzsummary(sch.wide ~ stype,
        # data = apiclus2
        design = dclus2,
        privacy_controls = list(
            suppression_raw_counts = 6L
        )
    )
})

test_that("Value magnitudes (cell totals and means)", {
    # devtools::load_all()
    inf <- inzsummary(api00 ~ stype | awards + both,
        g1.level = "Yes",
        g2.level = "Yes",
        design = dclus2,
        privacy_controls = list(
            suppression = 500L,
            suppression_magnitude = 200L
        )
    )

    # 4.3.1 - suppress totals/means if unrounded count < x
    popest <- read.table(
        textConnection(
            inf[grep("Population estimates", inf) + 3:5]
        )
    )
    expect_equal(popest[2, 5], "S")
    expect_equal(popest[2, 7], "S")
    expect_equal(popest[2, 8], "S")
    expect_equal(popest[3, 8], "S")
})

test_that("Medians, quantiles, and percentiles", {
    # devtools::load_all()
    inf <- inzsummary(api00 ~ stype | awards + both,
        g1.level = "Yes",
        g2.level = "Yes",
        design = dclus2,
        privacy_controls = list(
            suppression_quantiles = list(
                p = c(0.25, 0.5, 0.75),
                n = c(20, 10, 20)
            )
        )
    )

    # 4.4.1 - suppress quantiles based on scale
    popest <- read.table(
        textConnection(
            inf[grep("Population estimates", inf) + 3:5]
        )
    )
    expect_equal(popest[2, 2], "S")
})

test_that("Percentages, proportions, and ratios", {
    # devtools::load_all()
    inf <- inzsummary(~stype, design = dclus2, round_percent = 1L)

    # 4.5.2 - round percentages to 1 d.p.
    pr <- inf[grep("Population Estimates", inf) + 5]
    prs <- strsplit(pr, "\\s+")[[1]][3:5]
    expect_match(
        prs,
        "[0-9]+\\.[0-9]\\%"
    )
})
