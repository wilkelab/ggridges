context("geom_joy")



# Visual tests ------------------------------------------------------------

test_that("geom_joy draws correctly", {
  vdiffr::expect_doppelganger("geom_joy basic",
    ggplot(iris, aes(x = Sepal.Length, y = Species)) +
      geom_joy()
  )

  vdiffr::expect_doppelganger("geom_joy no trailing lines",
    ggplot(iris, aes(x = Sepal.Length, y = Species)) +
      geom_joy(rel_min_height = 0.005)
  )

  vdiffr::expect_doppelganger("geom_joy scale=3",
    ggplot(iris, aes(x = Sepal.Length, y = Species)) +
      geom_joy(scale = 3)
  )

  vdiffr::expect_doppelganger("geom_joy2 solid polygons",
    ggplot(iris, aes(x = Sepal.Length, y = Species)) +
      geom_joy2()
  )
})
