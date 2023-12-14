context("Get plot inference")

set.seed(100)
d <- data.frame(
    x = rnorm(100, c(150, 155), c(10, 20)),
    y = factor(c("A", "B")),
    stringsAsFactors = TRUE
)

test_that("Two-sample tests use appropriate CI", {
    pTRUE <- getPlotSummary(x, y,
        data = d,
        summary.type = "inference",
        inference.type = "conf",
        hypothesis.var.equal = TRUE
    )
    pFALSE <- getPlotSummary(x, y,
        data = d,
        summary.type = "inference",
        inference.type = "conf",
        hypothesis.var.equal = FALSE
    )

    pvals <- sapply(list(pTRUE, pFALSE), function(p) {
        as.numeric(
            gsub(
                ".+=", "",
                strsplit(p[grep("p-value = ", p)[1]], ",")[[1]][3]
            )
        )
    })
    expect_equal(
        pvals,
        c(
            t.test(x ~ y, data = d, var.equal = TRUE)$p.value,
            t.test(x ~ y, data = d)$p.value
        )
    )

    cis <- lapply(list(pTRUE, pFALSE), function(p) {
        scan(
            text = gsub("A - B", "", p[grep("A - B", p)]), what = double(),
            quiet = TRUE
        )[-1]
    })
    expect_equal(
        cis,
        list(
            as.numeric(round(t.test(x ~ y, data = d, var.equal = TRUE)$conf.int, 3)),
            as.numeric(round(t.test(x ~ y, data = d, var.equal = FALSE)$conf.int, 3))
        )
    )
})

set.seed(400)
d <- data.frame(
    x = sample(c("A", "B"), 100, replace = TRUE, c(0.3, 0.8)),
    stringsAsFactors = TRUE
)
ptest <- list(
    p.value = 2 * pnorm(
        abs((table(d$x)[[1]] / 100 - 0.5) / 0.05),
        lower.tail = FALSE
    )
)
btest <- binom.test(table(d$x), p = 0.4, alternative = "less")
ctest <- chisq.test(table(d$x))
s1 <- getPlotSummary(x,
    data = d, summary.type = "inference",
    inference.type = "conf",
    hypothesis.test = "proportion",
    hypothesis.use.exact = FALSE,
    hypothesis.value = 0.5,
    hypothesis.alt = "two.sided"
)
s2 <- getPlotSummary(x,
    data = d, summary.type = "inference",
    inference.type = "conf",
    hypothesis.test = "proportion",
    hypothesis.use.exact = TRUE,
    hypothesis.value = 0.4,
    hypothesis.alt = "less"
)
s3 <- getPlotSummary(x,
    data = d, summary.type = "inference",
    inference.type = "conf",
    hypothesis.test = "chi2"
)
test_that("One-sample tests give correct p-value", {
    expect_match(
        paste(s1, collapse = "\n"),
        sprintf("p-value = %s", format.pval(ptest$p.value, digits = 5))
    )
    expect_match(
        paste(s2, collapse = "\n"),
        sprintf("p-value = %s", format.pval(btest$p.value, digits = 5))
    )
    expect_match(
        paste(s3, collapse = "\n"),
        sprintf("p-value = %s", format.pval(ctest$p.value, digits = 5))
    )
})

test_that("One-sample tests display correct hypotheses", {
    expect_match(
        paste(s1, collapse = "\n"),
        "Null Hypothesis: true proportion of x = A is 0.5"
    )
    expect_match(
        paste(s1, collapse = "\n"),
        "Alternative Hypothesis: true proportion of x = A is not equal to 0.5"
    )

    expect_match(
        paste(s2, collapse = "\n"),
        "Null Hypothesis: true proportion of x = A is 0.4"
    )
    expect_match(
        paste(s2, collapse = "\n"),
        "Alternative Hypothesis: true proportion of x = A is less than 0.4"
    )

    expect_match(
        paste(s3, collapse = "\n"),
        "Null Hypothesis: true proportions in each category are equal"
    )
    expect_match(
        paste(s3, collapse = "\n"),
        "Alternative Hypothesis: true proportions in each category are not equal"
    )
})


# small counts
d <- expand.grid(
    Machine = c("Desktop of tablet", "Mobile"),
    Course = c("STATS101/G", "STATS108")
)
d <- d[rep(1:4, c(4, 3, 1, 2)), ]
rownames(d) <- NULL
ctest <- chisq.test(table(d$Course, d$Machine), simulate = TRUE)
s1 <- getPlotSummary(Machine, Course,
    data = d, summary.type = "inference",
    inference.type = "conf",
    hypothesis.test = "chi2"
)
test_that("Simulated p-value is included when small expected values", {
    expect_match(
        paste(s1, collapse = "\n"),
        "Simulated p-value (since some expected counts < 5) =",
        fixed = TRUE
    )
})

test_that("Simulated p-value is included when requested", {
    cas <- read.csv("cas.csv", stringsAsFactors = TRUE)
    s <- getPlotSummary(cellsource, gender,
        data = cas,
        summary.type = "inference",
        inference.type = "conf",
        hypothesis.test = "chi2",
        hypothesis.simulated.p.value = TRUE
    )
    expect_match(paste(s, collapse = "\n"), "Simulated p-value =")
})


# a giant table

# 2000?
# tab <- matrix(sample(2000, replace = TRUE), ncol = 50)
# system.time(chisq.test(tab, simulate = TRUE))[3]




test_that("inzinference gives the same output", {
    expect_equal(
        inzinference(Sepal.Length ~ Sepal.Width,
            data = iris, trend = "linear", width = 80
        ),
        getPlotSummary(Sepal.Width, Sepal.Length,
            data = iris, trend = "linear", width = 80,
            summary.type = "inference", inference.type = "conf"
        )
    )

    expect_equal(
        inzinference(Sepal.Length ~ Sepal.Width | Species,
            data = iris, trend = "linear", width = 80
        ),
        getPlotSummary(Sepal.Width, Sepal.Length,
            g1 = Species,
            data = iris, trend = "linear", width = 80,
            summary.type = "inference"
        )
    )

    expect_equal(
        inzinference(Sepal.Length ~ Species | Sepal.Width,
            data = iris, width = 80
        ),
        getPlotSummary(Sepal.Length, Species,
            g1 = Sepal.Width,
            data = iris, width = 80, inference.type = "conf",
            summary.type = "inference"
        )
    )
})

# anova
test_that("ANOVA (one-way) output is the correct way around", {
    inf <- inzinference(Sepal.Length ~ Species, data = iris)
    expect_match(inf, "setosa\\s+-\\s+versicolor\\s+-0.930", all = FALSE)
})


####
test_that("Confidence level can be adjusted - dot plots", {
    # dot plot - one
    inf <- inzinference(~Sepal.Length,
        data = iris,
        ci.width = 0.8
    ) |> as.character()
    expect_match(inf, "Mean with 80% Confidence Interval", all = FALSE)
    expect_equal(
        inf[grep("Estimate\\s+Lower\\s+Upper", inf) + 1L] |>
            strsplit("\\s+") |> unlist() |> as.double() |> round(2L),
        c(NA_real_, 5.84, 5.76, 5.93)
    )

    # dot plot - two
    iris2 <- iris[iris$Species != "setosa", ] |> droplevels()
    inf <- inzinference(Sepal.Length ~ Species,
        data = iris2,
        ci.width = 0.90
    ) |> as.character()
    expect_match(inf, "Group Means with 90% Confidence Intervals",
        all = FALSE
    )
    x <- paste(
        collapse = "\n",
        inf[grep("Group Means with 90% Confidence Intervals", inf) + 3:4]
    )
    m0 <- read.table(textConnection(x))[, -1] |> as.matrix()
    ci <- tapply(iris2$Sepal.Length, iris2$Species, t.test,
        conf.level = 0.9
    ) |>
        sapply(function(x) x$conf.int) |>
        t()
    m <- cbind(
        tapply(iris2$Sepal.Length, iris2$Species, mean),
        ci[, 1],
        ci[, 2]
    ) |> round(3)
    expect_equivalent(m0, m)
    expect_match(inf,
        "Difference in group means with 90% Confidence Interval",
        all = FALSE
    )
    expect_equal(
        inf[grep("Difference in group means", inf) + 3L] |>
            strsplit("\\s+") |> unlist() |> tail(3) |> as.double() |>
            round(3L),
        c(-0.652, -0.844, -0.460)
    )

    # dot plot - 3+
    inf <- inzinference(Sepal.Length ~ Species,
        data = iris,
        ci.width = 0.99
    ) |> as.character()
    expect_match(inf, "Group Means with 99% Confidence Intervals",
        all = FALSE
    )
    x <- paste(
        collapse = "\n",
        inf[grep("Group Means with 99% Confidence Intervals", inf) + 3:5]
    )
    m0 <- read.table(textConnection(x))[, -1] |> as.matrix()
    ci <- tapply(iris$Sepal.Length, iris$Species, t.test,
        conf.level = 0.99
    ) |>
        sapply(function(x) x$conf.int) |>
        t()
    m <- cbind(
        tapply(iris$Sepal.Length, iris$Species, mean),
        ci[, 1],
        ci[, 2]
    ) |> round(3)
    expect_equivalent(m0, m)
    ## - difference CIs
    expect_match(inf,
        "99% Confidence Intervals",
        all = FALSE,
        fixed = TRUE
    )
    x <- paste(
        collapse = "\n",
        gsub("\n", "", inf[grep("adjusted for multiple comparisons", inf) + c(4, 5, 6)],
            fixed = TRUE
        )
    )
    m0 <- read.fwf(textConnection(x), c(29, 9, 10, 10, 10))[, 2:4] |> as.matrix()
    f <- lm(Sepal.Length ~ Species, data = iris)
    m <- s20x::multipleComp(f, 0.99)[, 1:3]
    expect_equivalent(m0, m)

    # bar chart - one way
    inf <- inzinference(~Species,
        data = iris,
        ci.width = 0.92
    ) |> as.character()
    expect_match(
        inf,
        "Estimated Proportions with 92% Confidence Interval",
        all = FALSE
    )
    x <- paste(
        collapse = "\n",
        inf[grep("Estimated Proportions with 92% Confidence Interval", inf) + 3:5]
    )
    m0 <- read.table(textConnection(x))[, -1] |>
        as.matrix() |>
        unname()
    t <- qnorm(0.96) * sqrt(1 / 3 * 2 / 3 / nrow(iris))
    m <- cbind(
        rep(1 / 3, 3),
        1 / 3 - t,
        1 / 3 + t
    ) |> round(3)
    expect_equivalent(m0, m)

    expect_match(inf, "with 92% Confidence Intervals", all = FALSE)
    x <- paste(
        collapse = "\n",
        gsub("\n", "", inf[grep("92% Confidence Intervals", inf) + 4:6], fixed = TRUE)
    )
    m0 <- read.fwf(textConnection(x), c(26, 9, 10, 9))[, -1] |>
        as.matrix() |>
        as.double() |>
        round(3)
    m <- freq1way.edited(t(as.matrix(table(iris$Species))), conf.level = 0.92)[, -(1:2)] |>
        as.matrix() |>
        as.double() |>
        round(3)
    expect_equal(m0, m)

    # bar chart - two way
    set.seed(100)
    d <- data.frame(
        x = sample(LETTERS[1:3], 100, replace = TRUE),
        y = sample(LETTERS[1:2], 100, replace = TRUE),
        stringsAsFactors = TRUE
    )
    inf <- inzinference(y ~ x, data = d, ci.width = 0.8)

    expect_match(inf, "80% Confidence Intervals", all = FALSE)

    # scatter plot
    inf <- inzinference(Sepal.Length ~ Sepal.Width, data = iris, ci.width = 0.9, trend = "linear")
    expect_match(
        inf,
        "Linear Trend Coefficients with 90% Confidence Intervals",
        all = FALSE
    )
})
