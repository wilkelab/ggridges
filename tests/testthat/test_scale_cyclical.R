context("scale_*_cyclical")


test_that("basic tests", {
  df <- data.frame(x=sample(1:26), y=sample(1:26),
                   letters)
  p <- ggplot(df, aes(x, y, label=letters, color=letters)) + geom_text() +
    scale_color_cyclical(values = c("#F00000", "#0000F0"))
  d <- layer_data(p)

  # make sure color pattern repeats as expected
  expect_equal(d$colour, rep(c("#F00000", "#0000F0"), 13))

  # once again, different aesthetic, different cyclical pattern
  p <- ggplot(df, aes(x, y, label=letters, color=factor(x))) + geom_text() +
    scale_color_cyclical(values = c("#F00000", "#0000F0", "#F0F000"))
  d <- layer_data(p)

  # make sure color pattern repeats as expected
  expect_equal(d$colour[order(d$x)], rep(c("#F00000", "#0000F0", "#F0F000"), 9)[1:26])

  # test that breaks must match labels
  expect_error(
    ggplot(df, aes(x, y, label=letters, color=factor(x))) + geom_text() +
      scale_color_cyclical(values = c("#F00000", "#0000F0", "#F0F000"),
                           breaks = c(1, 2, 3),
                           labels = c("red", "blue")),
    "`breaks` and `labels` must have the same length")
})


# Visual tests ------------------------------------------------------------
test_that("visual appearance", {
  df <- data.frame(x=1:30, y=1:30)
  p <- ggplot(df, aes(x, y, fill = factor(x))) + geom_point(shape = 21, size = 3) +
    scale_fill_cyclical(values = c("#F00000", "#00F000", "#0000F0"))
  vdiffr::expect_doppelganger("scale_fill_cyclical red-green-blue dots, no legend", p)

  p <- ggplot(df, aes(x, y, color = factor(x))) + geom_point(size = 3) +
    scale_color_cyclical(values = c("#F00000", "#00F000", "#0000F0"), guide = "legend")
  vdiffr::expect_doppelganger("scale_fill_cyclical red-green-blue dots, with legend", p)

})
