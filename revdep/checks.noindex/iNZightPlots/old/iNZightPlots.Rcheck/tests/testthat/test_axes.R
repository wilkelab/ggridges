context("Plot axes")

require(grid)
set.seed(58926)

test_that("Axis label formatting is consistent for large values", {
    d <- data.frame(
        x = runif(100, 0, 150000),
        y = runif(100, -1e5, 1e5),
        stringsAsFactors = TRUE
    )

    p <- inzplot(y ~ x, data = d)
    expect_is(p, "inzplotoutput")
    labs <- grid.get("inz-xaxis-bottom.1.1")$label

    skip_if(length(labs) != 4)
    expect_equal(
        labs,
        format(seq(0, 150000, length = 4), scientific = FALSE)
    )
    expect_false(any(grepl("1e+", labs, fixed = TRUE)))
})

test_that("Axis tick labels don't overlap axis label", {
    d <- data.frame(
        x = runif(1e2),
        y = runif(1e2, 1e5, 2e5),
        z = sample(LETTERS[1:4], 100, replace = TRUE)
    )
    inzplot(y ~ x | z, data = d, ylab = "A very long title",
        transform = list(y = "log")
    )
})
