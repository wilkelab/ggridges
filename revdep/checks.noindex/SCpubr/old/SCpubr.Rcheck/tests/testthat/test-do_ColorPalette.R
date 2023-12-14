if (base::isFALSE(dep_check[["do_ColorPalette"]])){
  testthat::test_that("do_BarPlot: PASS - color vectors", {

    out <- SCpubr::do_ColorPalette(colors.use = "steelblue")
    testthat::expect_length(out, 12)

    out <- SCpubr::do_ColorPalette(colors.use = "#440154FF")
    testthat::expect_length(out, 12)

    out <- SCpubr::do_ColorPalette(colors.use = "steelblue", opposite = TRUE)
    testthat::expect_length(out, 2)

    out <- SCpubr::do_ColorPalette(colors.use = "steelblue", adjacent = TRUE)
    testthat::expect_length(out, 3)

    out <- SCpubr::do_ColorPalette(colors.use = "steelblue", triadic = TRUE)
    testthat::expect_length(out, 3)

    out <- SCpubr::do_ColorPalette(colors.use = "steelblue", split_complementary = TRUE)
    testthat::expect_length(out, 3)

    out <- SCpubr::do_ColorPalette(colors.use = "steelblue", tetradic = TRUE)
    testthat::expect_length(out, 4)

    out <- SCpubr::do_ColorPalette(colors.use = "steelblue", square = TRUE)
    testthat::expect_length(out, 4)


  })


  testthat::test_that("do_BarPlot: PASS - color vectors using n", {

    out <- SCpubr::do_ColorPalette(colors.use = "steelblue", n = 4)
    testthat::expect_length(out, 4)
  })

  testthat::test_that("do_BarPlot: PASS - color vectors plot = TRUERUE", {

    p <- SCpubr::do_ColorPalette(colors.use = "steelblue", plot = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ColorPalette(colors.use = "steelblue", opposite = TRUE, plot = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ColorPalette(colors.use = "steelblue", adjacent = TRUE, plot = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ColorPalette(colors.use = "steelblue", triadic = TRUE, plot = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ColorPalette(colors.use = "steelblue", split_complementary = TRUE, plot = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ColorPalette(colors.use = "steelblue", tetradic = TRUE, plot = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ColorPalette(colors.use = "steelblue", square = TRUE, plot = TRUE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BarPlot: PASS - complete output = TRUE", {

    out <- SCpubr::do_ColorPalette(colors.use = "steelblue", complete_output = TRUE)
    testthat::expect_length(names(out), 3)
    testthat::expect_type(out, "list")
  })

  testthat::test_that("do_BarPlot: FAIL - more than one color", {

    testthat::expect_error({SCpubr::do_ColorPalette(colors.use = c("red", "blue"))})
  })

  testthat::test_that("do_BarPlot: FAIL - not a color", {

    testthat::expect_error({SCpubr::do_ColorPalette(colors.use = 3)})
  })

  testthat::test_that("do_BarPlot: FAIL - negative n", {

    testthat::expect_error({SCpubr::do_ColorPalette(colors.use = "steelblue", n = -8)})
  })

  testthat::test_that("do_BarPlot: FAIL - more than one option", {

    testthat::expect_error({SCpubr::do_ColorPalette(colors.use = "steelblue", opposite = TRUE, tetradic = TRUE)})
  })

  testthat::test_that("do_BarPlot: FAIL - not a number", {

    testthat::expect_error({SCpubr::do_ColorPalette(colors.use = "steelblue", n = "wrong")})
  })

  testthat::test_that("do_BarPlot: WARNING - n set when an option is used", {

    testthat::expect_warning({SCpubr::do_ColorPalette(colors.use = "steelblue", n = 8, opposite = TRUE)})
  })

  testthat::test_that("do_BarPlot: FAIL - complete output and plot are TRUE", {

    testthat::expect_error({SCpubr::do_ColorPalette(colors.use = "steelblue", complete_output = TRUE, plot = TRUE)})
  })
}

