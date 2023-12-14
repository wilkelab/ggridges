context("Financial Times plots")

gg_pkgs <- c(
    "ggplot2",
    "dplyr",
    "tidyr",
    "forcats",
    "ggmosaic",
    "waffle",
    "ggthemes",
    "ggbeeswarm",
    "ggridges"
)
gg_pkgs_check <- sapply(gg_pkgs, requireNamespace, quietly = TRUE)
skip_if(any(!gg_pkgs_check), "Unable to check FT plots as some packages are missing.")

iris$Test.Cat.Var <- sample(letters[1:3], size = nrow(iris), replace = TRUE)
iris$Test.Cat.Var2 <- sample(LETTERS[1:3], size = nrow(iris), replace = TRUE)

test_that("Barcode plots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode")
    expect_match(attr(p1, "code"), 'ggplot2::geom_point\\(shape = "\\|"\\)')

    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_barcode")
    expect_match(attr(p1, "code"), 'ggplot2::geom_point\\(shape = "\\|"\\)')

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode", gg_barSize = 12)
    expect_equal(p1$layers[[1]]$aes_params$size, 12)

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode2")
    expect_equal(p1$labels$y, "")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode3")
    expect_match(attr(p1, "code"), "ggplot2::geom_spoke")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode3", gg_width = 2, gg_height = 1.5)
    expect_equal(p1$layers[[1]]$aes_params$linewidth, 2)
    expect_equal(p1$layers[[1]]$aes_params$radius, 1.5)
})

test_that("Boxplots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_boxplot")
    expect_match(attr(p1, "code"), "ggplot2::geom_boxplot()")

    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_boxplot")
    expect_match(attr(p1, "code"), "ggplot2::geom_boxplot()")
    expect_match(attr(p1, "code"), "fill = Species")
})

test_that("Violin plots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin", fill_colour = "red", alpha = 0.2)
    expect_match(attr(p1, "code"), "ggplot2::geom_violin()")
    expect_equal(p1$layers[[1]]$aes_params$fill, "red")
    expect_equal(p1$layers[[1]]$aes_params$alpha, 0.2)

    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_violin")
    expect_match(attr(p1, "code"), "ggplot2::geom_violin()")
    expect_match(attr(p1, "code"), "fill = Species")

    ## Flipping x and y variables works
    p1 <- iNZightPlot(Species, Sepal.Length, data = iris, plottype = "gg_violin")
    expect_match(attr(p1, "code"), "ggplot2::geom_violin()")
})

test_that("Dotstrip plots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_dotstrip")
    expect_match(attr(p1, "code"), "ggplot2::geom_point()")

    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_dotstrip")
    expect_match(attr(p1, "code"), "ggplot2::geom_point()")
    expect_match(attr(p1, "code"), "colour = Species")
})

test_that("Beeswarm plots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_beeswarm")
    expect_match(attr(p1, "code"), "ggbeeswarm::geom_beeswarm()")

    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_beeswarm")
    expect_match(attr(p1, "code"), "ggbeeswarm::geom_beeswarm()")
    expect_match(attr(p1, "code"), "colour = Species")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_quasirandom")
    expect_match(attr(p1, "code"), "ggbeeswarm::geom_quasirandom()")

    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_quasirandom")
    expect_match(attr(p1, "code"), "ggbeeswarm::geom_quasirandom()")
    expect_match(attr(p1, "code"), "colour = Species")

    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_quasirandom", gg_swarmwidth = 0.2)
    expect_equal(p1$layers[[1]]$position$width, 0.2)
})

test_that("Density plots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_density", alpha = 0.2, fill_colour = "red")
    expect_match(attr(p1, "code"), "ggplot2::geom_density()")
    expect_equal(p1$layers[[1]]$aes_params$alpha, 0.2)
    expect_equal(p1$layers[[1]]$aes_params$fill, "red")

    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_density", alpha_densitygroup = 0.2)
    expect_match(attr(p1, "code"), "ggplot2::geom_density()")
    expect_match(attr(p1, "code"), "fill = Species")
    expect_equal(p1$layers[[1]]$aes_params$alpha, 0.2)
})

test_that("Ridgeline plots work", {
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_ridgeline")
    expect_match(attr(p1, "code"), "ggridges::geom_density_ridges()")
    expect_match(attr(p1, "code"), "fill = Species")
})

test_that("Cumulative curve plots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_cumcurve")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_step")

    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_cumcurve")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_step")
    expect_match(attr(p1, "code")[2], "colour = Species")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_cumcurve", fill_colour = "blue")
    expect_equal(p1$layers[[1]]$aes_params$colour, "blue")
})

test_that("Lollipop count plots work", {
    p1 <- iNZightPlot(Species, data = iris, plottype = "gg_lollipop2", fill_colour = "red")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_point")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_segment")

    p1 <- iNZightPlot(Species, Test.Cat.Var, data = iris, plottype = "gg_lollipop2")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_point")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_linerange")
    expect_match(attr(p1, "code")[2], "colour = Test.Cat.Var")

    p1 <- iNZightPlot(Test.Cat.Var, data = iris, plottype = "gg_lollipop2", ordered = "asc")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_point")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_segment")
    expect_match(attr(p1, "code")[1], "forcats::fct_reorder")

    p1 <- iNZightPlot(Test.Cat.Var, Species, data = iris, plottype = "gg_lollipop2", ordered = "asc")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_point")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_linerange")
    expect_match(attr(p1, "code")[2], "colour = Species")
    expect_match(attr(p1, "code")[1], "forcats::fct_reorder")
})

test_that("Pie plots work", {
    p1 <- iNZightPlot(Test.Cat.Var, data = iris, plottype = "gg_pie")
    expect_match(attr(p1, "code"), "ggplot2::geom_bar")
    expect_match(attr(p1, "code"), "ggplot2::coord_polar")

    p1 <- iNZightPlot(Species, data = iris, plottype = "gg_pie", ordered = "desc")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_bar")
    expect_match(attr(p1, "code")[1], "forcats::fct_infreq\\(Species\\)")

    p1 <- iNZightPlot(Species, data = iris, plottype = "gg_pie", ordered = "asc")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_bar")
    expect_match(attr(p1, "code")[1], "forcats::fct_infreq\\(Species\\)")
    expect_match(attr(p1, "code")[1], "forcats::fct_rev")
})

test_that("Donut plots work", {
    p1 <- iNZightPlot(Species, data = iris, plottype = "gg_donut")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_rect")

    p1 <- iNZightPlot(Species, data = iris, plottype = "gg_donut", ordered = "desc")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_rect")
    expect_match(attr(p1, "code")[1], "forcats::fct_infreq\\(Species\\)")

    p1 <- iNZightPlot(Species, data = iris, plottype = "gg_donut", ordered = "asc")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_rect")
    expect_match(attr(p1, "code")[1], "forcats::fct_infreq\\(Species\\)")
    expect_match(attr(p1, "code")[1], "forcats::fct_rev")
})

test_that("Grid plots work", {
    p1 <- iNZightPlot(Species, data = iris, plottype = "gg_gridplot")
    expect_match(attr(p1, "code")[2], "waffle::waffle")
})

test_that("Lollipop distribution plots work", {
    iris2 <- iris
    iris2$id <- as.character(1:nrow(iris2))

    p1 <- iNZightPlot(Sepal.Length, data = iris2, plottype = "gg_lollipop")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_point")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_segment")

    is.na(iris2$Sepal.Length) <- rep(c(TRUE, FALSE), times = c(5, nrow(iris2) - 5))

    suppressWarnings(
        p1 <- iNZightPlot(Sepal.Length, data = iris2, plottype = "gg_lollipop")
    )
    expect_equal(p1$labels$subtitle, "5 Missing Observations Removed")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_point")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_segment")
})

test_that("Column distribution plots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_column2")
    expect_match(attr(p1, "code")[2], "ggplot2::geom_col")
})



test_that("Facetting works", {
    p1 <- iNZightPlot(Sepal.Length, g1 = Species, data = iris, plottype = "gg_violin")
    expect_equal(names(p1$facet$params$facets), "Species")

    p1 <- iNZightPlot(Sepal.Length, g1 = Species, g1.level = "setosa", data = iris, plottype = "gg_violin")
    expect_match(attr(p1, "code")[1], 'Species == \\"setosa\\"')

    p1 <- iNZightPlot(Sepal.Length, g1 = Species, g2 = Test.Cat.Var, data = iris, plottype = "gg_violin")
    expect_equal(names(p1$facet$params$facets), "Species")

    p1 <- iNZightPlot(Sepal.Length, g1 = Species, g2 = Test.Cat.Var, g2.level = "_MULTI", data = iris, plottype = "gg_violin")
    expect_match(attr(p1, "code")[1], "ggplot2::facet_grid")
    expect_equal(names(p1$facet$params$cols), "Species")
    expect_equal(names(p1$facet$params$rows), "Test.Cat.Var")

    p1 <- iNZightPlot(Sepal.Length, g1 = Species, g1.level = "setosa", g2 = Test.Cat.Var, g2.level = "_MULTI", data = iris, plottype = "gg_violin")
    expect_match(attr(p1, "code")[2], "ggplot2::facet_grid")
    expect_equal(names(p1$facet$params$cols), "Species")
    expect_equal(names(p1$facet$params$rows), "Test.Cat.Var")
    expect_match(attr(p1, "code")[1], 'Species == \\"setosa\\"')
})

test_that("Facetting works for plots with data step", {
    p1 <- iNZightPlot(Test.Cat.Var2, g1 = Species, data = iris, plottype = "gg_lollipop2")
    expect_equal(names(p1$facet$params$facets), "Species")

    p1 <- iNZightPlot(Test.Cat.Var2, g1 = Species, g1.level = "setosa", data = iris, plottype = "gg_lollipop2")
    expect_match(attr(p1, "code")[1], 'Species == \\"setosa\\"')

    p1 <- iNZightPlot(Test.Cat.Var2, g1 = Species, g2 = Test.Cat.Var, g2.level = "_MULTI", data = iris, plottype = "gg_lollipop2")
    expect_match(attr(p1, "code")[2], "ggplot2::facet_grid")
    expect_equal(names(p1$facet$params$cols), "Species")
    expect_equal(names(p1$facet$params$rows), "Test.Cat.Var")

    p1 <- iNZightPlot(Test.Cat.Var2, g1 = Species, g1.level = "setosa", g2 = Test.Cat.Var, g2.level = "_MULTI", data = iris, plottype = "gg_lollipop2")
    expect_match(attr(p1, "code")[2], "ggplot2::facet_grid")
    expect_equal(names(p1$facet$params$cols), "Species")
    expect_equal(names(p1$facet$params$rows), "Test.Cat.Var")
    expect_match(attr(p1, "code")[1], 'Species == \\"setosa\\"')
})

test_that("Data names can be replaced", {
    test_expr <- rlang::expr(
        ggplot2::ggplot(iris, ggplot2::aes(Species)) +
            ggplot2::geom_bar()
    )

    test_expr2 <- replace_data_name(test_expr, "test_name")
    expect_equal(as.character(test_expr2[[2]][[2]]), "test_name")
})

test_that("Other expressions can be inserted into expression", {
    test_expr <- rlang::expr(
        plot_data <- other_data %>%
            dplyr::group_by(var1, var2) %>%
            dplyr::summarise(count = sum(count))
    )

    insert_into_first_place(test_expr, rlang::expr(dplyr::mutate(new_var = var1 + var2)))
})

test_that("Other expressions can be inserted into expression", {
    test_expr <- rlang::expr(
        plot_data <- other_data %>%
            dplyr::group_by(var1, var2) %>%
            dplyr::ungroup() %>%
            dplyr::summarise(count = sum(count))
    )

    test_expr2 <- add_to_group(test_expr[[3]], rlang::sym("facet_var"))

    expect_equal(as.character(test_expr2[[2]][[2]][[3]]), c("dplyr::group_by", "var1", "var2", "facet_var"))
    expect_equal(as.character(test_expr2[[2]][[3]]), c("dplyr::group_by", "facet_var"))
})


test_that("Inversing x/y doesn't affect graph", {
    p1 <- iNZightPlot(Species, Sepal.Length, data = iris, plottype = "gg_violin")
    p2 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_violin")
    expect_equal(attr(p1, "code"), attr(p2, "code"))
})

test_that("Y-axis label for one-way numeric is blank", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode")
    expect_equal(p1$labels$y, "")
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode2")
    expect_equal(p1$labels$y, "")
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode3")
    expect_equal(p1$labels$y, "")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_boxplot")
    expect_equal(p1$labels$x, "")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin")
    expect_equal(p1$labels$x, "")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_dotstrip")
    expect_equal(p1$labels$y, "")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_beeswarm")
    expect_equal(p1$labels$x, "")
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_quasirandom")
    expect_equal(p1$labels$x, "")
})

test_that("Plots can be rotated", {
    p1 <- iNZightPlot(Sepal.Length,
        data = iris, plottype = "gg_violin",
        rotation = FALSE
    )
    expect_match(attr(p1, "code"), "coord_flip")

    p1 <- iNZightPlot(Sepal.Length,
        data = iris, plottype = "gg_violin",
        rotation = TRUE
    )
    expect_false(grepl("coord_flip", attr(p1, "code")))

    p1 <- iNZightPlot(Species,
        data = iris, plottype = "gg_gridplot",
        rotation = TRUE
    )
    expect_match(attr(p1, "code")[2], "flip = TRUE")

    # p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin",
    #     rotation = TRUE, rotate_labels = list(x = TRUE))
    # expect_match(attr(p1, "code"), "coord_flip")
})

test_that("Plots can have themes", {
    p1 <- iNZightPlot(Sepal.Length,
        data = iris, plottype = "gg_violin",
        gg_theme = "bw"
    )
    expect_match(attr(p1, "code"), "theme_bw")

    p1 <- iNZightPlot(Sepal.Length,
        data = iris, plottype = "gg_violin",
        gg_theme = "tufte"
    )
    expect_match(attr(p1, "code"), "theme_tufte")
})

test_that("General plot formatting works", {
    p1 <- iNZightPlot(Sepal.Length,
        data = iris, plottype = "gg_violin",
        rotate_labels = list(x = TRUE, y = TRUE)
    )
    expect_equal(p1$theme$axis.text.x$angle, 45)
    expect_equal(p1$theme$axis.text.y$angle, 45)

    p1 <- iNZightPlot(Sepal.Length,
        data = iris, plottype = "gg_violin",
        bg = "black"
    )
    expect_equal(p1$theme$panel.background$fill, "black")

    p1 <- iNZightPlot(Sepal.Length, Species,
        data = iris, plottype = "gg_violin",
        caption = "Test caption"
    )
    expect_equal(p1$labels$caption, "Test caption")

    p1 <- iNZightPlot(Sepal.Length, Species,
        data = iris, plottype = "gg_violin",
        cex = 2
    )
    expect_equal(p1$theme$text$size, 22)
})

test_that("Each type of palette works", {
    ## General ggplot palettes
    p1 <- iNZightPlot(Sepal.Length, Species,
        data = iris, plottype = "gg_violin",
        palette = "Reds"
    )
    expect_match(attr(p1, "code"), 'scale_fill_brewer\\(palette = \\"Reds\\"\\)')

    p1 <- iNZightPlot(Sepal.Length, Species,
        data = iris, plottype = "gg_cumcurve",
        palette = "Reds"
    )
    expect_match(attr(p1, "code")[2], 'scale_colour_brewer\\(palette = \\"Reds\\"\\)')

    iris$Test.Cat.Var <- sample(letters[1:3], size = nrow(iris), replace = TRUE)

    p1 <- iNZightPlot(Species, Test.Cat.Var,
        data = iris, plottype = "gg_heatmap",
        palette = "Reds"
    )
    expect_match(attr(p1, "code")[2], 'scale_fill_distiller\\(palette = \\"Reds\\"\\)')

    ## Viridis palettes
    p1 <- iNZightPlot(Sepal.Length, Species,
        data = iris, plottype = "gg_violin",
        palette = "inferno"
    )
    expect_match(attr(p1, "code"), 'scale_fill_viridis_d\\(option = \\"inferno\\"\\)')

    p1 <- iNZightPlot(Sepal.Length, Species,
        data = iris, plottype = "gg_cumcurve",
        palette = "inferno"
    )
    expect_match(attr(p1, "code")[2], 'scale_colour_viridis_d\\(option = \\"inferno\\"\\)')

    p1 <- iNZightPlot(Species, Test.Cat.Var,
        data = iris, plottype = "gg_heatmap",
        palette = "inferno"
    )
    expect_match(attr(p1, "code")[2], 'scale_fill_viridis_c\\(option = \\"inferno\\"\\)')

    ## Greyscale palette
    p1 <- iNZightPlot(Sepal.Length, Species,
        data = iris, plottype = "gg_violin",
        palette = "greyscale"
    )
    expect_match(attr(p1, "code"), "scale_fill_grey")

    p1 <- iNZightPlot(Sepal.Length, Species,
        data = iris, plottype = "gg_cumcurve",
        palette = "greyscale"
    )
    expect_match(attr(p1, "code")[2], "scale_colour_grey")

    p1 <- iNZightPlot(Species, Test.Cat.Var,
        data = iris, plottype = "gg_heatmap",
        palette = "greyscale"
    )
    expect_match(attr(p1, "code")[2], "scale_fill_gradient")
})
