context("Identify points")

test_that("Points can be labelled by another variable", {
    p <- inzplot(~Sepal.Width, data = iris, locate = Species, locate.id = c(1, 100),
        plot = FALSE)
    expect_equal(
        p$all$all$toplot$all$text.labels,
        ifelse(1:nrow(iris) %in% c(1, 100),
            as.character(iris$Species), "")[order(iris$Sepal.Width)]
    )

    p <- inzplot(Sepal.Length ~ Sepal.Width, data = iris, locate = Species,
        locate.id = 1:5, plot.features = list(order.first = -1), plot = FALSE)
    expect_equal(
        p$all$all$text.labels,
        ifelse(seq_len(nrow(iris)) > 5, "", "setosa")
    )
})

test_that("Points can be labelled by their row id", {
    p <- inzplot(~Sepal.Width, data = iris, locate = "id", locate.id = 1:5, plot = FALSE)
    expect_equal(
        p$all$all$toplot$all$text.labels,
        ifelse(1:nrow(iris) > 5, "", as.character(1:nrow(iris)))[order(iris$Sepal.Width)]
    )

    p <- inzplot(Sepal.Length ~ Sepal.Width, data = iris, locate = "id",
        locate.id = 1:5, plot.features = list(order.first = -1), plot = FALSE)
    expect_equal(
        p$all$all$text.labels,
        ifelse(seq_len(nrow(iris)) > 5, "", seq_len(nrow(iris)))
    )
})

test_that("Points identified by an expression", {
    p <- inzplot(~Sepal.Width, data = iris, locate = Species,
        locate.id = Species == "setosa", plot = FALSE)
    expect_equal(
        p$all$all$toplot$all$text.labels,
        ifelse(iris$Species == "setosa", "setosa", "")[order(iris$Sepal.Width)]
    )
})

test_that("Points with same level of X are identified", {
    p <- inzplot(~Sepal.Width, data = iris, locate = NULL,
        locate.id = 1, locate.same.level = Species,
        locate.col = "red", highlight = 1, plot = FALSE)
    expect_equal(
        p$all$all$toplot$all$text.labels,
        ifelse(iris$Species == "setosa", " ", "")[order(iris$Sepal.Width)]
    )

    p <- inzplot(Sepal.Length ~ Sepal.Width, data = iris, locate = NULL,
        locate.id = c(1), locate.same.level = Species,
        locate.col = "red", highlight = 1,
        plot.features = list(order.first = -1), plot = FALSE)
    expect_equal(
        p$all$all$text.labels,
        ifelse(iris$Species == "setosa", " ", "")
    )
})

test_that("Locating extreme points", {
    # dot plot
    p <- inzplot(~Sepal.Width, data = iris, colby = Species, locate.extreme = c(1, 4),
        locate = Species, plot = FALSE)
    expect_equal(p$all$all$toplot$all$extreme.ids, c(61, 15, 33, 34, 16))
    expect_equal(
        p$all$all$toplot$all$text.labels,
        c("versicolor", rep("", 145), rep("setosa", 4))
    )

    p <- inzplot(~Sepal.Width, data = iris, colby = Species,
        locate.extreme = c(1, 0), locate = Species, locate.same.level = Species,
        plot = FALSE, locate.col = "red")
    expect_equal(
        sort(p$all$all$toplot$all$extreme.ids),
        which(iris$Species == "versicolor")
    )
    expect_equal(
        p$all$all$toplot$all$text.labels,
        ifelse(iris$Species == "versicolor", "versicolor", "")[order(iris$Sepal.Width)]
    )

    # scatter plot
    p <- inzplot(Sepal.Width ~ Sepal.Length, data = iris,
        colby = Species, locate.extreme = c(3),
        locate = Species, plot = T, plot.features = list(order.first = -1))
    px <- c(132, 16, 118)
    expect_equal(p$all$all$extreme.ids, px)
    expect_equal(
        p$all$all$text.labels[px],
        c("virginica", "setosa", "virginica")
    )
    expect_equal(p$all$all$text.labels[-px], rep("", 147))

    p <- inzplot(Sepal.Width ~ Sepal.Length, data = iris,
        colby = Species, locate.extreme = 1, locate.same.level = Species,
        locate = Species, plot = T, plot.features = list(order.first = -1))
    expect_equal(p$all$all$extreme.ids, which(iris$Species == "virginica"))
    expect_equal(
        p$all$all$text.labels,
        ifelse(iris$Species == "virginica", "virginica", "")
    )
})

test_that("No IDs = no labels", {
    p <- inzplot(~Sepal.Width, data = iris, locate.same.level = Species, plot = FALSE)
    expect_equal(
        p$all$all$toplot$all$text.labels,
        rep(NA_character_, nrow(iris))
    )
})

test_that("Missing values in locate same level", {
    iris$Species2 <- NA_character_
    p <- inzplot(Sepal.Width ~ Sepal.Length, data = iris,
        colby = Species, locate.extreme = 1, locate.same.level = Species,
        locate = Species2, plot = T, plot.features = list(order.first = -1))
    expect_equal(
        p$all$all$text.labels,
        c(rep("", 100), rep("missing", 50))
    )
})
