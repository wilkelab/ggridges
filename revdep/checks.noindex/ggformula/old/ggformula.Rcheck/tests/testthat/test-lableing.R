context("labeling")

mtcars2 <- df_stats(wt ~ cyl, data = mtcars, median_wt = median)

test_that(
  "HELPrct labels work", {
    wrapped_expect_doppelganger(
      "cesd ~ i1",
      gf_point(cesd ~ i1, data = mosaicData::HELPrct)
    )
  }
)


test_that(
  "Weather labels work", {
    wrapped_expect_doppelganger(
      "high_temp, low_temp, avg_temp",
      gf_point(high_temp ~ low_temp, color = ~avg_temp, data = mosaicData::Weather)
    )
  }
)

test_that(
  "gf_relabel() works", {
    wrapped_expect_doppelganger(
      "using a list",
      gf_point(mpg ~ hp, color = ~ cyl, data = mtcars) |>
        gf_relabel(list(hp = "horsepower", mpg = "mileage (miles/gallon)", cyl = "cylinders"))
    )
    wrapped_expect_doppelganger(
      "using ...",
      gf_point(mpg ~ hp, color = ~ cyl, data = mtcars) |>
        gf_relabel(hp = "horsepower", mpg = "mileage (miles/gallon)", cyl = "cylinders")
    )
  }
)

test_that(
  "set_variable_labels() works", {
    # labelled doesnt handle a list argument
    # wrapped_expect_doppelganger(
    #   "set_variable_labels() with list",
    #   set_variable_labels(mtcars,
    #              list(hp = "horsepower", mpg = "mileage (miles/gallon)", cyl = "cylinders")) |>
    #   gf_point(mpg ~ hp, color = ~ cyl, data = mtcars)
    # )
    wrapped_expect_doppelganger(
      "set_variable_labels() without list",
      set_variable_labels(mtcars,
                 hp = "horsepower", mpg = "mileage (miles/gallon)", cyl = "cylinders") |>
      gf_point(mpg ~ hp, color = ~ cyl, data = mtcars)
    )
  }
)
