context("Plot interactivity function")

x <- rnorm(100)
y <- rnorm(100)
a <- factor(sample(LETTERS[1:4], 100, replace = TRUE), levels = LETTERS[1:4])
b <- factor(sample(LETTERS[1:4], 100, replace = TRUE), levels = LETTERS[1:4])

test_that("Supported iNZightPlots return TRUE", {
    expect_true(can.interact(iNZightPlot(x)))
    expect_true(can.interact(iNZightPlot(x, a)))
    expect_true(can.interact(iNZightPlot(a)))
    expect_true(can.interact(iNZightPlot(a, b)))
    expect_true(can.interact(iNZightPlot(x, y)))
})

test_that("Unsupported plots return FALSE", {
    expect_false(can.interact(plot(x, y)))
    expect_false(can.interact(
        ggplot2::ggplot() +
            ggplot2::geom_point(ggplot2::aes(x, y))
    ))
})

df <- data.frame(x = x, y = y, a = a, b = b, stringsAsFactors = TRUE)
test_that("FT plots return the correct response", {
    skip_if_not_installed("ggmosaic")
    skip_if_not_installed("waffle")

    # expect_true(can.interact(
    #     iNZightPlot(a, plottype = "gg_column",
    #         varnames = list(x = "a"), data_name = "df")
    # ))
    expect_false(can.interact(
        iNZightPlot(a,
            plottype = "gg_pie",
            varnames = list(x = "a"), data_name = "df"
        )
    ))
})

test_that("Files are written to temporary directory - one file", {
    curd <- getwd()
    p <- iNZightPlot(x, y)
    expect_equal(length(dev.list()), 1L)
    expect_is(p, "inzplotoutput")
    url <- try(exportHTML(p), silent = TRUE)
    skip_if(inherits(url, "try-error"))
    on.exit(unlink(url))
    expect_is(url, "inzHTML")
    expect_match(as.character(url), "te?mp")

    # working directory is unchanged
    expect_equal(curd, getwd())
})

test_that("Files are written to temporary directory - mutliple files", {
    curd <- getwd()
    p <- iNZightPlot(x, y)
    expect_equal(length(dev.list()), 1L)
    expect_is(p, "inzplotoutput")
    url <- try(exportHTML(p, local = TRUE), silent = TRUE)
    skip_if(inherits(url, "try-error"))
    on.exit(unlink(url))
    expect_is(url, "inzHTML")
    expect_match(as.character(url), "te?mp")
    expect_true(dir.exists(file.path(dirname(url), "assets")))

    # working directory is unchanged
    expect_equal(curd, getwd())
})

test_that("Directory for local can be specified", {
    curd <- getwd()
    p <- iNZightPlot(x, y)
    td <- file.path(tempdir(), "test")
    dir.create(td, recursive = TRUE)
    on.exit(unlink(td, TRUE, TRUE)) # will also delete `url`
    url <- try(exportHTML(p, local = TRUE, dir = td), silent = TRUE)
    skip_if(inherits(url, "try-error"))
    expect_is(url, "inzHTML")
    expect_true(file.exists(file.path(td, "index.html")))
    # expect_equal(dirname(as.character(url)), td)
    expect_true(dir.exists(file.path(td, "assets")))
})



try(unlink("Rplot.pdf"), silent = TRUE)
