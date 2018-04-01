context("scale_*_cyclical")


test_that("basic tests", {
  df <- data.frame(x=sample(1:26), y=sample(1:26),
                   letters)
  p <- ggplot(df, aes(x, y, label=letters, color=letters)) + geom_text() +
    scale_color_cyclical(values = c("#F00000", "#0000F0"))
  d <- layer_data(p)

  # make sure color pattern repeats as expected
  expect_equal(d$colour, rep(c("#F00000", "#0000F0"), 13))

  # make sure there is no legend being generated
  expect_equal("guide-box" %in% ggplotGrob(p)$layout$name, FALSE)

  # once again, different aesthetic, different cyclical pattern, now with legend
  p <- ggplot(df, aes(x, y, label=letters, color=factor(x))) + geom_text() +
    scale_color_cyclical(values = c("#F00000", "#0000F0", "#F0F000"), guide = "legend")
  d <- layer_data(p)

  # make sure color pattern repeats as expected
  expect_equal(d$colour[order(d$x)], rep(c("#F00000", "#0000F0", "#F0F000"), 9)[1:26])

  # make sure there is a legend
  expect_equal("guide-box" %in% ggplotGrob(p)$layout$name, TRUE)

  # test that breaks must match labels
  expect_error(
    ggplot(df, aes(x, y, label=letters, color=factor(x))) + geom_text() +
      scale_color_cyclical(values = c("#F00000", "#0000F0", "#F0F000"),
                           breaks = c(1, 2, 3),
                           labels = c("red", "blue")),
    "`breaks` and `labels` must have the same length")

  # test that legend is omitted if breaks are manually set to NULL, even when legend is switched on
  p <- ggplot(df, aes(x, y, label=letters, color=factor(x))) + geom_text() +
    scale_color_cyclical(values = c("#F00000", "#0000F0", "#F0F000"), breaks = NULL, guide = "legend")
  expect_equal("guide-box" %in% ggplotGrob(p)$layout$name, FALSE)

})


# Visual tests ------------------------------------------------------------
test_that("visual appearance of scale_*_cyclical", {
  testthat::skip_on_cran()
  testthat::skip("skip for now, something's broken")

  df <- data.frame(x=1:30, y=1:30)
  p <- ggplot(df, aes(x, y, fill = factor(x))) + geom_point(shape = 21, size = 3) +
    scale_fill_cyclical(values = c("#F00000", "#00F000", "#0000F0"))
  vdiffr::expect_doppelganger("scale_fill_cyclical red-green-blue dots, no legend", p)

  p <- ggplot(df, aes(x, y, color = factor(x))) + geom_point(size = 3) +
    scale_color_cyclical(values = c("#F00000", "#00F000", "#0000F0"), guide = "legend")
  vdiffr::expect_doppelganger("scale_fill_cyclical red-green-blue dots, with legend", p)

})
