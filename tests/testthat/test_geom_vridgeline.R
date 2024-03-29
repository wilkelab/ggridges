context("geom_vridgeline")


# Visual tests ------------------------------------------------------------
test_that("visual appearance of geom_vridgeline", {
  d <- data.frame(y = rep(1:5, 3), x = c(rep(0, 5), rep(1, 5), rep(3, 5)),
                  width = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1))
  p <- ggplot(d, aes(x, y, width = width, group = x)) + geom_vridgeline(fill="lightblue")
  expect_doppelganger("geom_vridgeline basic use", p)

  p <- ggplot(iris, aes(x=Species, y=Sepal.Width, width = after_stat(density), fill=Species)) +
    geom_vridgeline(stat="ydensity", trim=FALSE, alpha = 0.85, scale = 2)
  expect_doppelganger("geom_vridgeline using stat ydensity", p)
})
