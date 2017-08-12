context("theme_joy")

test_that("key theme_joy settings", {
  # y axis labels are vertically aligned
  expect_equal(theme_joy()$axis.text.y$vjust, 0)
  # no minor grid
  expect_equal(theme_joy()$panel.grid.minor, ggplot2::element_blank())

  # major grid can be switched off
  expect_equal(theme_joy(grid = FALSE)$panel.grid.major, ggplot2::element_blank())

  # centered axis labels can be switched on
  expect_equal(theme_joy(center_axis_labels = TRUE)$axis.title.x$hjust, 0.5)
  expect_equal(theme_joy(center_axis_labels = TRUE)$axis.title.y$hjust, 0.5)
})

# Visual tests ------------------------------------------------------------

test_that("theme_joy draws correctly", {
  d <- data.frame(x = rep(1:5, 3), y = c(rep(0, 5), rep(1, 5), rep(3, 5)),
                  height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1))
  p <- ggplot(d, aes(x, y, height = height, group = y)) + geom_ridgeline(fill="lightblue")


  vdiffr::expect_doppelganger("theme_joy default",
    p + theme_joy()
  )

  vdiffr::expect_doppelganger("theme_joy without grid",
    p + theme_joy(grid = FALSE)
  )

  vdiffr::expect_doppelganger("theme_joy centered axis labels",
    p + theme_joy(center_axis_labels = TRUE)
  )
})
