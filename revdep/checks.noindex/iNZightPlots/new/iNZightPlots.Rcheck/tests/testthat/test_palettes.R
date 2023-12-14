context("Colour palettes")

f <- tempfile()
pdf(f)
on.exit(unlink(f))

test_that("Names are valid", {
    expect_equal(
        names(viridis_palette_names()),
        c("viridis", "magma", "plasma", "inferno")
    )
    expect_equal(
        names(cat_palette_names()),
        c(
            "contrast", "bright", "light",
            names(viridis_palette_names()),
            "colourblind", "rainbow"
        )
    )
    expect_warning(const_palette_names())
    expect_equal(
        names(cont_palette_names()),
        c(
            names(viridis_palette_names()),
            "rainbow", "blue", "green", "red",
            "greenyellow", "redblue",
            "terrain", "heat",
            "bluewhitepink", "bluewhitered"
        )
    )
})

test_that("Palettes have the correct length", {
    n <- 3
    ok <- sapply(names(cat_palette_names()), function(x) {
        length(inzpalette(x)(n)) == n
    })
    expect_equal(sum(ok), length(ok))
})

test_that("Discrete palattes default when n is too large", {
    pal <- inzpar()$col.default$cat
    expect_equal(inzpalette("contrast")(10), pal(10))
    expect_equal(inzpalette("bright")(10), pal(10))
    expect_equal(inzpalette("light")(15), pal(15))
})

test_that("Emphasize works", {
    pal <- inzpalette("light")
    cols <- pal(5)
    ecols <- emphasize_pal_colour(5, 2, fn = pal)
    expect_equal(cols[2], ecols[2])
    expect_equal(as.character(shade(cols[-2], 0.7)), ecols[-2])
})

test_that("Emphasized points on top", {
    p <- inzplot(Sepal.Width ~ Sepal.Length,
        data = iris, colby = Species,
        pch = 19, col.fun = inzpalette("bright"), col.emph = 2L
    )
    expect_equal(
        p$gen$opts$plot.features$order.first,
        which(iris$Species == unique(iris$Species)[2])
    )
})

test_that("Plot function handles strings", {
    p <- iNZightPlot(Sepal.Width, data = iris, colby = Species, col.fun = "contrast")
    expect_equivalent(
        p$gen$col.args$f.cols,
        inzpalette("contrast")(3)
    )
    expect_warning(
        iNZightPlot(Sepal.Width, data = iris, colby = Species, col.fun = "none"),
        "Invalid palette name"
    )

    p <- iNZightPlot(Sepal.Width,
        data = iris, colby = Species,
        col.fun = "contrast", col.emph = 2
    )
    expect_equivalent(
        p$gen$col.args$f.cols,
        emphasize_pal_colour(3L, 2L, TRUE, 3L, inzpalette("contrast"))
    )

    p <- iNZightPlot(Sepal.Width, Sepal.Length,
        data = iris, colby = Sepal.Length,
        col.fun = "viridis", col.emph = 2, col.emphn = 5, pch = 19
    )
    expect_equal(length(p$gen$col.args$n.cols), 200L)
})

test_that("Global options ", {
    cols <- c("pink", "palevioletred1", "grey90", "lavenderblush4", "tan2")
    op <- options(
        inzight.default.palette.cat = cols,
        inzight.default.palette.num = function(n) cols[1:n]
    )
    on.exit(options(op))

    p <- default_palette()
    expect_equal(p$cat(3), cols[1:3])
    expect_equal(
        p$cont(5),
        cols[1:5]
    )
})
