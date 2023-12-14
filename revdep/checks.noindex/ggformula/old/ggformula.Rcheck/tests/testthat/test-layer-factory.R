context("layer factory")

mtcars2 <- df_stats(wt ~ cyl, data = mtcars, median_wt = median)
data(penguins, package = "palmerpenguins")
penguins2 <-
  palmerpenguins::penguins |>
  dplyr::filter(!is.na(sex), !is.na(bill_depth_mm), !is.na(bill_length_mm))

penguins3 <-
  palmerpenguins::penguins |>
  dplyr::filter(!is.na(sex), !is.na(bill_depth_mm), !is.na(bill_length_mm)) |>
  dplyr::rename(`bill length` = bill_length_mm, `bill depth` = bill_depth_mm,
                `sex of penguin` = sex)

TestData <- dplyr::bind_rows(
  mosaic::do(20) * tibble(a = "A", b = "E", c = "X"),
  mosaic::do(20) * tibble(a = "A", b = "E", c = "Z"),
  mosaic::do(10) * tibble(a = "A", b = "F", c = "X"),
  mosaic::do(10) * tibble(a = "A", b = "F", c = "Z"),
  mosaic::do(5)  * tibble(a = "B", b = "E", c = "X"),
  mosaic::do(10) * tibble(a = "B", b = "E", c = "Z"),
  mosaic::do(15) * tibble(a = "B", b = "F", c = "X"),
  mosaic::do(10) * tibble(a = "B", b = "F", c = "Z"),
)

test_that(
  "gf_abline()", {
    wrapped_expect_doppelganger(
      "gf_abline1",
      gf_point(bill_length_mm ~ bill_depth_mm, data = penguins2) |>
        gf_abline(intercept = 10, slope = 2, color = "red")
    )
    wrapped_expect_doppelganger(
      "gf_abline2",
      gf_plot(data = penguins2) |>
        gf_point(bill_length_mm ~ bill_depth_mm) |>
        gf_abline(intercept = 10:12, slope = 2, color = c("red", "green", "blue"))
    )
    wrapped_expect_doppelganger(
      "gf_abline3",
      gf_point(wt ~ hp, size = ~wt, color = ~cyl, data = mtcars) |>
        gf_abline(slope = ~0, intercept = ~median_wt, color = ~cyl, data = mtcars2)
    )

    wrapped_expect_doppelganger(
      "gf_abline4",
      gf_point(wt ~ hp, size = ~wt, color = ~cyl, data = mtcars) |>
        gf_abline(slope = ~ 0, intercept = ~ 3, color = "green")
    )

    wrapped_expect_doppelganger(
      "gf_abline5",
      gf_point(wt ~ hp, size = ~wt, color = ~cyl, data = mtcars) |>
        gf_hline(yintercept = ~median_wt, color = ~cyl, data = mtcars2)
    )

    wrapped_expect_doppelganger(
      "gf_abline6",
      gf_point(mpg ~ hp, color = ~cyl, size = ~wt, data = mtcars) |>
        gf_abline(color = "red", slope = ~ -0.10, intercept = ~ 35)
    )

    wrapped_expect_doppelganger(
      "gf_abline7",
      gf_point(mpg ~ hp, color = ~cyl, size = ~wt, data = mtcars) |>
        gf_abline(
          color = "red", slope = ~slope, intercept = ~intercept,
          data = data.frame(slope = -0.10, intercept = 33:35)
        )
    )
  }
)

test_that(
  "gf_area() & gf_ribbon()", {
    Temps <- mosaicData::Weather |>
      dplyr::filter(city == "Chicago", year == 2016, month <= 4)
    wrapped_expect_doppelganger(
      "gf_ribbon1",
      gf_ribbon(low_temp + high_temp ~ date, data = Temps, color = "navy", alpha = 0.3)
    )
    wrapped_expect_doppelganger(
      "gf_area1",
      gf_area(high_temp ~ date, data = Temps, color = "navy", alpha = 0.3)
    )

    wrapped_expect_doppelganger(
      "gf_ribbon2",
      gf_ribbon(low_temp + high_temp ~ date, data = mosaicData::Weather, alpha = 0.3) |>
        gf_facet_grid(city ~ .)
    )
  }
)


test_that(
  "gf_ash()", {
    wrapped_expect_doppelganger(
      "gf_ash1",
      gf_ash(~bill_length_mm, color = ~species, data = penguins2)
    )
    wrapped_expect_doppelganger(
      "gf_ash2",
      gf_ash(~bill_length_mm, color = ~species, data = penguins2, binwidth = 1)
    )
    wrapped_expect_doppelganger(
      "gf_ash3",
      gf_ash(~bill_length_mm, color = ~species, data = penguins2, binwidth = 1, adjust = 10)
    )
  }
)

# if (require(ggforce)) {
#   test_that(
#     "gf_sina()", {
#       set.seed(1234)
#       wrapped_expect_doppelganger(
#         "gf_sina",
#         gf_sina(bill_length_mm ~ species, data = penguins2)
#       )
#       wrapped_expect_doppelganger(
#         "gf_sina2",
#         gf_sina(bill_length_mm ~ species, data = penguins2, color = ~species)
#       )
#       wrapped_expect_doppelganger(
#         "gf_sina3",
#         midwest |>
#           dplyr::mutate(area = cut_number(area, 5)) |>
#           gf_sina(popdensity ~ area, color = ~area, show.legend = FALSE) |>
#           gf_refine(scale_y_log10())
#       )
#     }
#   )
# }


test_that(
  "gf_bar() and gf_col()", {
    wrapped_expect_doppelganger(
      "gf_bar1",
      gf_bar(~species, data = penguins2)
    )
    D <- data.frame(
      group = LETTERS[1:3],
      count = c(20, 25, 18)
    )
    wrapped_expect_doppelganger(
      "gf_col1",
      gf_col(count ~ group, data = D)
    )
  }
)

if (FALSE) {
  test_that(
    "gf_barh() and gf_colh()", {
      SomeData <- data.frame(
        group = LETTERS[1:3],
        count = c(20, 25, 18)
      )
      wrapped_expect_doppelganger(
        "gf_barh1",
        gf_bar(substance ~ ., data = mosaicData::HELPrct)
      )
      wrapped_expect_doppelganger(
        "gf_barh1a",
        gf_bar(substance ~ ., data = mosaicData::HELPrct)
      )
      wrapped_expect_doppelganger(
        "gf_colh1",
        gf_col(group ~ count, data = SomeData)
      )
      wrapped_expect_doppelganger(
        "gf_colh1a",
        gf_col(group ~ count, data = SomeData)
      )
    }
  )
}

test_that(
  "gf_bin2d()", {
    wrapped_expect_doppelganger(
      "gf_bin2d1",
      gf_bin2d(eruptions ~ waiting, data = faithful, bins = 15) |>
        gf_refine(scale_fill_viridis_c(begin = 0.1, end = 0.9))
    )
  }
)

test_that(
  "gf_contour(), gf_density2d(), and gf_density_2d()", {
    wrapped_expect_doppelganger(
      "gf_countour1",
      gf_point(eruptions ~ waiting, data = faithful) |>
        gf_contour(density ~ waiting + eruptions, data = faithfuld)
    )
    wrapped_expect_doppelganger(
      "gf_density2d2",
      gf_point(eruptions ~ waiting, data = faithful) |>
        gf_density2d(eruptions ~ waiting, data = faithful)
    )
    wrapped_expect_doppelganger(
      "gf_density_2d",
      gf_point(eruptions ~ waiting, data = faithful) |>
        gf_density_2d(eruptions ~ waiting, data = faithful)
    )
  }
)

test_that(
  "gf_boxplot()", {
    wrapped_expect_doppelganger(
      "gf_boxplot1",
      gf_boxplot(bill_length_mm ~ species, color = ~species, data = penguins2)
    )
    wrapped_expect_doppelganger(
      "gf_boxplot2",
      # move boxplots away a bit by adjusting dodge
      gf_boxplot(age ~ substance,
                 data = mosaicData::HELPrct, color = ~sex,
                 position = position_dodge(width = 0.9)
      )
    )
    wrapped_expect_doppelganger(
      "gf_boxplot3",
      gf_boxplot(age ~ substance | sex, data = mosaicData::HELPrct, coef = 5, width = 0.4) |>
        gf_jitter(width = 0.2, alpha = 0.3, seed = 123)
    )
  }
)


test_that(
  "gf_coefline()", {
    mdl <- lm(bill_length_mm ~ bill_depth_mm, data = penguins2)
    wrapped_expect_doppelganger(
      "gf_coefline1",
      gf_point(bill_length_mm ~ bill_depth_mm, data = penguins2) |>
        gf_coefline(coef = coef(mdl))
    )
    wrapped_expect_doppelganger(
      "gf_coefline2",
      gf_point(bill_length_mm ~ bill_depth_mm, data = penguins2) |>
        gf_coefline(model = mdl)
    )
  }
)

test_that(
  "gf_contour(), gf_density_2d(), gf_density2d()", {
    wrapped_expect_doppelganger(
      "gf_contour1",
      gf_density_2d(eruptions ~ waiting, data = faithful, alpha = 0.5, color = "navy") |>
        gf_contour(density ~ waiting + eruptions, data = faithfuld, bins = 10, color = "red")
    )
    wrapped_expect_doppelganger(
      "gf_contour2",
      gf_jitter(avg_drinks ~ age,
                data = mosaicData::HELPrct,
                seed = 123,
                color = ~sex, alpha = 0.2, width = 0.4, height = 0.4
      ) |>
        gf_density_2d()
    )
    wrapped_expect_doppelganger(
      "gf_contour3",
      gf_jitter(avg_drinks ~ age,
                data = mosaicData::HELPrct,
                seed = 123,
                color = ~sex, alpha = 0.2, width = 0.4, height = 0.4
      ) |>
        gf_density2d()
    )
  }
)

if (FALSE) {
  test_that(
    "gf_countsh(), gf_percentsh(), gf_propsh()", {
      wrapped_expect_doppelganger(
        "gf_countsh1a",
        gf_counts(substance ~ ., data = mosaicData::HELPrct, fill = ~sex, position = "dodge")
      )
      wrapped_expect_doppelganger(
        "gf_countsh2a",
        gf_counts(substance ~ ., data = mosaicData::HELPrct, fill = ~sex, position = "dodge")
      )
      wrapped_expect_doppelganger(
        "gf_percentsh1a",
        gf_percents(substance ~ ., data = mosaicData::HELPrct, fill = ~sex, position = "dodge")
      )
      wrapped_expect_doppelganger(
        "gf_percentsh2a",
        gf_percents(substance ~ ., data = mosaicData::HELPrct, fill = ~sex, position = "dodge")
      )
      wrapped_expect_doppelganger(
        "gf_propsh1a",
        gf_props(substance ~ ., data = mosaicData::HELPrct, fill = ~sex, position = "dodge")
      )
      wrapped_expect_doppelganger(
        "gf_propsh2a",
        gf_props(substance ~ ., data = mosaicData::HELPrct, fill = ~sex, position = "dodge")
      )
    }
  )
}

test_that(
  "gf__counts(), gf_props(), gf_percents()", {
    wrapped_expect_doppelganger(
      "gf_counts1",
      gf_counts(~substance, data = mosaicData::HELPrct, fill = ~sex, position = position_dodge())
    )
    wrapped_expect_doppelganger(
      "gf_props1",
      gf_props(~substance, data = mosaicData::HELPrct, fill = ~sex, position = position_dodge())
    )
    wrapped_expect_doppelganger(
      "gf_percents1",
      gf_percents(~substance, data = mosaicData::HELPrct, fill = ~sex, position = position_dodge())
    )
  }
)

test_that(
  "gf_crossbar(), gf_errorbar(), gf_pointrange()", {
    HELP2 <- mosaicData::HELPrct |>
      dplyr::group_by(substance, sex) |>
      dplyr::summarise(
        mean.age = mean(age),
        median.age = median(age),
        max.age = max(age),
        min.age = min(age),
        sd.age = sd(age),
        lo = mean.age - sd.age,
        hi = mean.age + sd.age
      )

    wrapped_expect_doppelganger(
      "gf_pointrange2",
      gf_jitter(age ~ substance,
                data = mosaicData::HELPrct, seed = 123,
                alpha = 0.5, width = 0.2, height = 0, color = "skyblue"
      ) |>
        gf_pointrange(mean.age + lo + hi ~ substance, data = HELP2) |>
        gf_facet_grid(~sex)
    )
    wrapped_expect_doppelganger(
      "gf_errorbar1",
      gf_jitter(age ~ substance,
                data = mosaicData::HELPrct, seed = 123,
                alpha = 0.5, width = 0.2, height = 0, color = "skyblue"
      ) |>
        gf_errorbar(lo + hi ~ substance, data = HELP2, inherit = FALSE) |>
        gf_facet_grid(~sex)
    )
    wrapped_expect_doppelganger(
      "gf_crossbar1",
      gf_jitter(age ~ substance,
                data = mosaicData::HELPrct, seed = 123,
                alpha = 0.5, width = 0.2, height = 0, color = "skyblue"
      ) |>
        gf_boxplot(age ~ substance, data = mosaicData::HELPrct, color = "red") |>
        gf_crossbar(mean.age + lo + hi ~ substance, data = HELP2) |>
        gf_facet_grid(~sex)
    )
  }
)

if (FALSE) {
  test_that(
    "gf_crossbarh(), gf_errorbarh(), gf_pointrangeh()", {
      HELP2 <- mosaicData::HELPrct |>
        dplyr::group_by(substance, sex) |>
        dplyr::summarise(
          mean.age = mean(age),
          median.age = median(age),
          max.age = max(age),
          min.age = min(age),
          sd.age = sd(age),
          lo = mean.age - sd.age,
          hi = mean.age + sd.age
        )

      wrapped_expect_doppelganger(
        "gf_pointrangeh1",
        gf_jitter(substance ~ age,
                  data = mosaicData::HELPrct, seed = 123,
                  alpha = 0.5, height = 0.2, width = 0, color = "skyblue"
        ) |>
          gf_pointrange(substance ~ mean.age + lo + hi, inherit = FALSE, linewidth = 1,
                         data = HELP2) |>
          gf_facet_grid(sex ~ .)
      )
      wrapped_expect_doppelganger(
        "gf_pointrangeh1a",
        gf_jitter(substance ~ age, inherit = FALSE,
                  data = mosaicData::HELPrct, seed = 123,
                  alpha = 0.5, height = 0.2, width = 0, color = "skyblue"
        ) |>
          gf_pointrange(substance ~ mean.age + lo + hi, inherity = FALSE,
                        data = HELP2) |>
          gf_facet_grid(sex ~ .)
      )
      wrapped_expect_doppelganger(
        "gf_errorbarh1",
        gf_jitter(substance ~ age,
                  data = mosaicData::HELPrct, seed = 123,
                  alpha = 0.5, height = 0.2, width = 0, color = "skyblue"
        ) |>
          gf_errorbar(substance ~ lo + hi, data = HELP2, inherit = FALSE) |>
          gf_facet_grid(sex ~ .)
      )
      wrapped_expect_doppelganger(
        "gf_errorbarh1a",
        gf_jitter(substance ~ age,
                  data = mosaicData::HELPrct, seed = 123,
                  alpha = 0.5, height = 0.2, width = 0, color = "skyblue"
        ) |>
          gf_errorbar(substance ~ lo + hi, data = HELP2, inherit = FALSE) |>
          gf_facet_grid(sex ~ .)
      )
      wrapped_expect_doppelganger(
        "gf_crossbarh1",
        gf_jitter(substance ~ age,
                  data = mosaicData::HELPrct, seed = 123,
                  alpha = 0.5, height = 0.2, width = 0, color = "skyblue"
        ) |>
          gf_crossbarh(substance ~ mean.age + lo + hi, data = HELP2, inherit = FALSE) |>
          gf_facet_grid(sex ~ .)
      )
      wrapped_expect_doppelganger(
        "gf_crossbarh1a",
        gf_jitter(substance ~ age,
                  data = mosaicData::HELPrct, seed = 123,
                  alpha = 0.5, height = 0.2, width = 0, color = "skyblue"
        ) |>
          gf_crossbar(substance ~ mean.age + lo + hi, data = HELP2, inherit = FALSE) |>
          gf_facet_grid(sex ~ .)
      )
    }
  )
}

test_that(
  "gf_curve() and gf_segment()", {
    SomeData <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
    wrapped_expect_doppelganger(
      "gf_curve_segment1",
      gf_point(mpg ~ wt, data = mtcars) |>
        gf_curve(y1 + y2 ~ x1 + x2, data = SomeData, color = "navy") |>
        gf_segment(y1 + y2 ~ x1 + x2, data = SomeData, color = "red")
    )
  }
)

test_that(
  "gf_dens() and gf_density", {
    wrapped_expect_doppelganger(
      "gf_dens1",
      gf_dens(~bill_length_mm, data = penguins2)
    )
    wrapped_expect_doppelganger(
      "gf_dens2",
      gf_dens(~bill_length_mm, data = penguins2, color = ~species)
    )
    wrapped_expect_doppelganger(
      "gf_density1",
      gf_density(~bill_length_mm, fill = ~species, data = penguins2, alpha = 0.5)
    )
    wrapped_expect_doppelganger(
      "gf_density2",
      gf_density(~ bill_length_mm | species ~ ., fill = ~species, data = penguins2, alpha = 0.5)
    )
  }
)
test_that(
  "gf_dist()", {
    wrapped_expect_doppelganger(
      "gf_dist1",
      gf_dist("norm", mean = 10, sd = 2)
    )
    wrapped_expect_doppelganger(
      "gf_dist2",
      gf_dist("norm", fill = ~ (abs(x) <= 2), geom = "area")
    )
    wrapped_expect_doppelganger(
      "gf_dist3",
      gf_dist("norm", fill = "red", kind = "histogram")
    )
    wrapped_expect_doppelganger(
      "gf_dist4",
      gf_dist("norm", color = "red", kind = "qqstep", resolution = 25) |>
        gf_dist("norm", color = "black", kind = "qq", resolution = 25, linewidth = 2, alpha = 0.5)
    )
    wrapped_expect_doppelganger(
      "gf_dist5",
      gf_dist("binom", size = 20, prob = 0.25, plot_size = 2, linewidth = 0.8)
    )
    wrapped_expect_doppelganger(
      "gf_dist6",
      gf_dist("binom", params = list(size = 20, prob = 0.25), size = 2, linewidth = 0.8)
    )
  }
)

test_that(
  "gf_dotplot()", {
    wrapped_expect_doppelganger(
      "gf_dotplot1",
      gf_dotplot(~bill_length_mm, fill = ~species, data = penguins2, binwidth = 0.2) |>
        gf_labs(title = "dotdensity")
    )
    wrapped_expect_doppelganger(
      "gf_dotplot2",
      gf_dotplot(~bill_length_mm,
                 fill = ~species, data = penguins2,
                 binwidth = 0.2, method = "histodot"
      ) |>
        gf_labs(title = "histodot")
    )
  }
)

test_that(
  "gf_empty(), gf_blank(), and gf_frame()", {
    wrapped_expect_doppelganger(
      "gf_empty1",
      gf_empty() |>
        gf_labs(title = "empty")
    )
    wrapped_expect_doppelganger(
      "gf_empty2",
      gf_empty() |>
        gf_point(bill_length_mm ~ bill_depth_mm, data = penguins2, color = ~species) |>
        gf_labs(title = "empty + point")
    )
    wrapped_expect_doppelganger(
      "gf_frame1",
      gf_frame(c(0, 10) ~ c(0, 5)) |>
        gf_labs(title = "frame")
    )
    wrapped_expect_doppelganger(
      "gf_blank1",
      gf_point((c(0, 1)) ~ (c(0, 5))) |>
        gf_blank((c(0, 3)) ~ (c(-2, 7)))
    )
  }
)

test_that(
  "gf_fitdistr()", {
    set.seed(12345)
    Dat <- data.frame(g = rgamma(500, 3, 10), f = rf(500, df1 = 3, df2 = 47))
    wrapped_expect_doppelganger(
      "gf_fitdistr1",
      gf_fitdistr(~length, data = mosaicData::KidsFeet, inherit = FALSE) |>
        gf_dhistogram(~length,
                      data = mosaicData::KidsFeet, binwidth = 0.5,
                      alpha = 0.25
        )
    )
    wrapped_expect_doppelganger(
      "gf_fitdistr2",
      gf_dhistogram(~g, data = Dat) |>
        gf_fitdistr(dist = "dgamma")
    )
  }
)

test_that(
  "gf_freqpoly()", {
    wrapped_expect_doppelganger(
      "gf_freqpoly1",
      gf_freqpoly(~bill_length_mm, color = ~species, data = penguins2)
    )
    wrapped_expect_doppelganger(
      "gf_freqpoly2",
      gf_freqpoly(~bill_length_mm,
                  color = ~species, data = penguins2,
                  binwidth = 0.5
      )
    )
  }
)

test_that(
  "gf_freqpoly()", {
    wrapped_expect_doppelganger(
      "gf_fun1",
      gf_fun(sin(x) ~ x, color = ~"sin", xlim = pi * c(-2, 2)) |>
        gf_fun(cos(x) ~ x, color = ~"cosine", xlim = pi * c(-2, 2))
    )
    wrapped_expect_doppelganger(
      "gf_fun2",
      gf_point(length ~ width, data = mosaicData::KidsFeet) |>
        gf_fun(4 * cos(5 * x) + 24 ~ x, color = ~"cosine", xlim = pi * c(-2, 2)) |>
        gf_labs(color = "")
    )
  }
)

test_that(
  "gf_fun2d()", {
    wrapped_expect_doppelganger(
      "gf_fun2d1",
      gf_function_2d(fun = function(x, y) sin(2 * x * y),
                     xlim = c(-pi, pi), ylim = c(-pi, pi)) |>
        gf_refine(scale_fill_viridis_c())
    )
    wrapped_expect_doppelganger(
      "gf_fun2d2",
      gf_function_2d(fun = function(x, y) x + y, contour = FALSE)
    )
    wrapped_expect_doppelganger(
      "gf_fun2d3",
      gf_function_tile(fun = function(x, y) x * y) |>
        gf_function_contour(fun = function(x, y) x * y, color = "white", inherit = FALSE) |>
        gf_refine(scale_fill_viridis_c())
    )
    wrapped_expect_doppelganger(
      "gf_fun2d4",
      gf_fun_tile(x * y ~ x + y, xlim = c(-3, 3), ylim = c(-2, 2)) |>
        gf_fun_contour(x * y ~ x + y, color = "white", inherit = FALSE) |>
        gf_refine(scale_fill_viridis_c()) |>
        gf_labs(fill = "product")
    )
    wrapped_expect_doppelganger(
      "gf_fun2d5",
      gf_fun(sin(x) ~ x, color = ~"sin", xlim = pi * c(-2, 2)) |>
        gf_fun(cos(x) ~ x, color = ~"cosine", xlim = pi * c(-2, 2))
    )
    wrapped_expect_doppelganger(
      "gf_fun2d6",
      gf_point(length ~ width, data = mosaicData::KidsFeet) |>
        gf_fun(4 * cos(5 * x) + 24 ~ x, color = ~"cosine", xlim = pi * c(-2, 2)) |>
        gf_labs(color = "")
    )
  }
)


test_that(
  "gf_hex()", {
    wrapped_expect_doppelganger(
      "gf_hex1",
      gf_hex(avg_drinks ~ age, data = mosaicData::HELPrct, bins = 15) |>
        gf_density2d(avg_drinks ~ age,
                     data = mosaicData::HELPrct,
                     color = "yellow", alpha = 0.5
        )
    )
  }
)



test_that(
  "gf_histogram() and gf_dhistogram", {
    wrapped_expect_doppelganger(
      "gf_histogram1",
      gf_histogram(~bill_length_mm, data = penguins2)
    )
    wrapped_expect_doppelganger(
      "gf_histogram2",
      gf_histogram(~ bill_length_mm | species ~ .,
                   fill = ~species, data = penguins2, alpha = 0.5,
                   binwidth = 0.25
      )
    )
    wrapped_expect_doppelganger(
      "gf_dhistogram1",
      gf_dhistogram(~bill_length_mm, data = penguins2)
    )
    wrapped_expect_doppelganger(
      "gf_dhistogramh1",
      gf_dhistogramh(bill_length_mm ~ ., data = penguins2)
    )
    wrapped_expect_doppelganger(
      "gf_dhistogramh1a",
      gf_dhistogramh(bill_length_mm ~ ., data = penguins2)
    )
    wrapped_expect_doppelganger(
      "gf_dhistogramh2",
      gf_dhistogramh(bill_length_mm ~ after_stat(ndensity), data = penguins2)
    )
    wrapped_expect_doppelganger(
      "gf_dhistogramh2a",
      gf_dhistogram(bill_length_mm ~ after_stat(ndensity), data = penguins2)
    )
  }
)

test_that(
  "gf_line()", {
    wrapped_expect_doppelganger(
      "gf_line1",
      gf_line(births ~ date, data = mosaicData::Births78)
    )
    wrapped_expect_doppelganger(
      "gf_line2",
      gf_line(births ~ date, color = ~wday, data = mosaicData::Births78)
    )
  }
)

test_that(
  "gf_label() and gf_text()", {
    wrapped_expect_doppelganger(
      "gf_label1",
      gf_label(width ~ length, data = mosaicData::KidsFeet, label = ~name)
    )
    wrapped_expect_doppelganger(
      "gf_label2",
      gf_label(width ~ length, data = mosaicData::KidsFeet, label = ~name,
               nudge_x = 0.1, nudge_y = -0.05, color = ~ sex) |>
        gf_point()
    )
    wrapped_expect_doppelganger(
      "gf_text1",
      gf_text(width ~ length, data = mosaicData::KidsFeet, label = ~name)
    )
    wrapped_expect_doppelganger(
      "gf_text2",
      gf_text(width ~ length, data = mosaicData::KidsFeet, label = ~name,
              color = ~ sex, nudge_x = 0.1, nudge_y = -0.05) |>
        gf_point()
    )
  }
)

test_that(
  "gf_linerange() and gf_pointrange()", {
    wrapped_expect_doppelganger(
      "gf_linerange1",
      gf_linerange(low_temp + high_temp ~ date,
                   data = mosaicData::Weather, color = ~avg_temp
      ) |>
        gf_facet_grid(city ~ year, scale = "free") |>
        gf_refine(scale_color_viridis_c(option = "C", begin = 0.1, end = 0.8))
    )
    wrapped_expect_doppelganger(
      "gf_pointrange1",
      gf_pointrange(avg_temp + low_temp + high_temp ~ date,
                    data = mosaicData::Weather |> head(200),
                    color = ~avg_temp
      ) |>
        gf_facet_grid(city ~ year, scale = "free") |>
        gf_refine(scale_color_viridis_c(option = "C", begin = 0.1, end = 0.8))
    )
  }
)

test_that(
  "gf_point()", {
    wrapped_expect_doppelganger(
      "gf_point1",
      gf_point(bill_length_mm ~ bill_depth_mm, data = penguins2)
    )
    wrapped_expect_doppelganger(
      "gf_point2",
      gf_point(bill_length_mm ~ bill_depth_mm | species, color = ~species, data = penguins2)
    )
  }
)

test_that(
  "gf_qq(), gf_qqstep(), gf_qqline()", {
    wrapped_expect_doppelganger(
      "gf_qq1",
      gf_qq(~ age | substance, data = mosaicData::HELPrct) |>
        gf_qqline()
    )
    wrapped_expect_doppelganger(
      "gf_qqstep1",
      gf_qqstep(~ age | substance, data = mosaicData::HELPrct) |>
        gf_qqline()
    )
  }
)



test_that(
  "gf_quantile()", {
    wrapped_expect_doppelganger(
      "gf_quantile",
      gf_point((1 / hwy) ~ displ, data = mpg) |>
        gf_quantile((1 / hwy) ~ displ, quantiles = 0.5, color = "red") |>
        gf_quantile((1 / hwy) ~ displ, quantiles = c(0.2, 0.8))
    )
  }
)


test_that(
  "gf_raster(), gf_tile(), gf_density2d()", {
    wrapped_expect_doppelganger(
      "gf_raster1",
      gf_raster(density ~ eruptions + waiting, data = faithfuld)
    )
    wrapped_expect_doppelganger(
      "gf_tile1",
      # inherit = FALSE avoids warning about dropping fill
      gf_tile(density ~ eruptions + waiting, data = faithfuld) |>
        gf_contour(density ~ eruptions + waiting, color = "yellow", inherit = FALSE) |>
        gf_refine(scale_fill_viridis_c(begin = 0.2))
    )
    wrapped_expect_doppelganger(
      "gf_density2d1",
      gf_density2d(eruptions ~ waiting, data = faithful)
    )
  }
)

test_that(
  "gf_rect()", {
    wrapped_expect_doppelganger(
      "gf_rect1",
      gf_rect(1.5 + 3 ~ 40 + 68, fill = "red", alpha = 0.2) |>
        gf_rect(3 + 5.5 ~ 68 + 100, fill = "green", alpha = 0.2) |>
        gf_point(eruptions ~ waiting, data = faithful)
    )
  }
)

test_that(
  "gf_rect()", {
    set.seed(1234)
    SomeData <- expand.grid(x = 1:10, y = 1:10)
    SomeData$angle <- runif(100, 0, 2 * pi)
    SomeData$speed <- runif(100, 0, sqrt(0.1 * SomeData$x))

    wrapped_expect_doppelganger(
      "gf_spoke1",
      gf_point(y ~ x, data = SomeData) |>
        gf_spoke(y ~ x, angle = ~angle, radius = 0.5)
    )

    wrapped_expect_doppelganger(
      "gf_spoke2",
      gf_point(y ~ x, data = SomeData) |>
        gf_spoke(y ~ x, angle = ~angle, radius = ~speed)
    )
  }
)

test_that(
  "gf_rug(), etc.", {
    set.seed(1234)
    wrapped_expect_doppelganger(
      "gf_rugx()/gf_rug_y() #1",
      gf_point(bill_length_mm ~ bill_depth_mm, data = penguins2) |>
        gf_rugx(~bill_depth_mm, data = penguins2, color = "red") |>
        gf_rugy(bill_length_mm ~ ., data = penguins2, color = "green")
    )
    wrapped_expect_doppelganger(
      "gf_rugx()/gf_rug_y() with jitter",
      gf_jitter(bill_length_mm ~ bill_depth_mm, data = penguins2, seed = 123) |>
        gf_rugx( ~ bill_depth_mm, data = penguins2, color = "red", seed = 123) |>
        gf_rugy(bill_length_mm ~ ., data = penguins2, color = "green", seed = 123)
    )
    wrapped_expect_doppelganger(
      "gf_rugx() without jitter",
      penguins2 |>
      gf_dhistogram( ~ bill_length_mm) |>
        gf_rugx(~ bill_length_mm, alpha = 0.4, color = "red", seed = 123)
    )
    wrapped_expect_doppelganger(
      "gf_rugx() with jitter",
      penguins2 |>
      gf_dhistogram( ~ bill_length_mm) |>
        gf_rugx(0 ~ bill_length_mm, position = "jitter", alpha = 0.4, color = "red",
                seed = 123)
    )
    wrapped_expect_doppelganger(
      "gf_rugy()",
      gf_dhistogram( bill_length_mm ~ ., data = penguins2) |>
        gf_rugy( ~ bill_length_mm , alpha = 0.4, color = "navy", seed = 123)
    )
    wrapped_expect_doppelganger(
      "gf_rug() on y only but with jitter",
      gf_dhistogram( bill_length_mm ~ ., data = penguins2) |>
        gf_rug( bill_length_mm ~ 0, alpha = 0.4, color = "navy", seed = 123)
    )

###
    wrapped_expect_doppelganger(
      "gf_rug() #1",
      gf_point(bill_length_mm ~ bill_depth_mm, data = penguins2) |>
        gf_rug(. ~ bill_depth_mm, data = penguins2, color = "red", inherit = FALSE) |>
        gf_rug(bill_length_mm ~ ., data = penguins2, color = "green", inherit = FALSE)
    )
    wrapped_expect_doppelganger(
      "gf_rug() #2",
      gf_point(bill_length_mm ~ bill_depth_mm, data = penguins2) |>
        gf_rug(. ~ bill_depth_mm, data = penguins2, color = "red", sides = "b") |>
        gf_rug(bill_length_mm ~ ., data = penguins2, color = "green", sides = "l")
    )
    # jitter requires both an x and a y, but we can turn off one or the other with sides
    wrapped_expect_doppelganger(
      "gf_rug() #3",
      gf_jitter(bill_length_mm ~ bill_depth_mm, data = penguins2) |>
        gf_rug(color = "green", sides = "b", position = "jitter", seed = 123)
    )
    # rugs work with some 1-varialbe plots as well.
    wrapped_expect_doppelganger(
      "gf_rug #4",
      gf_histogram(~eruptions, data = faithful) |>
        gf_rug(~eruptions, data = faithful, color = "red") |>
        gf_rug(~eruptions, data = faithful, color = "navy", sides = "t")
    )
    # we can take advantage of inheritance to shorten the code
    wrapped_expect_doppelganger(
      "gf_rug #5",
      gf_histogram(~eruptions, data = faithful) |>
        gf_rug(color = "red") |>
        gf_rug(color = "navy", sides = "t")
    )
    # Need to turn off inheritance when using gf_dhistogram:
    wrapped_expect_doppelganger(
      "gf_rug() #6",
      gf_dhistogram(~eruptions, data = faithful) |>
        gf_rug(~eruptions, data = faithful, color = "red", inherit = FALSE)
    )
    # using jitter with gf_histogram() requires manually setting the y value.
    wrapped_expect_doppelganger(
      "gf_rug() #7",
      gf_dhistogram(~bill_depth_mm, data = penguins2) |>
        gf_rug(0 ~ bill_depth_mm, data = penguins2, color = "green", sides = "b",
               position = "jitter", seed = 123)
    )
    # the choice of y value can affect how the plot looks.
    wrapped_expect_doppelganger(
      "gf_rug() #8",
      gf_dhistogram(~bill_depth_mm, data = penguins2) |>
        gf_rug(0.5 ~ bill_depth_mm, data = penguins2, color = "green", sides = "b",
               position = "jitter", seed = 123)
    )
  }
)

test_that(
  "discrete_breaks()", {
    wrapped_expect_doppelganger(
      "discrete_breaks -- default",
      gf_histogram( ~ bill_length_mm, data = penguins2) |>
        gf_refine(scale_x_continuous(breaks = discrete_breaks()))
    )
    wrapped_expect_doppelganger(
      "discrete_breaks -- 2.0",
      gf_histogram( ~ bill_length_mm, data = penguins2) |>
        gf_refine(scale_x_continuous(breaks = discrete_breaks(2)))
    )
  }
)


test_that(
  "non-syntactic names", {
    wrapped_expect_doppelganger(
      "gf_point() with facets",
      gf_point(`bill length` ~ `bill depth` | `sex of penguin`, data = penguins3)
    )
    wrapped_expect_doppelganger(
      "gf_histogram() with facets",
      gf_histogram(~ `bill depth` | `sex of penguin` ~ ., data = penguins3)
    )
    wrapped_expect_doppelganger(
      "gf_density() with fill",
      gf_density( ~ `bill depth`, fill = ~ `sex of penguin`, data = penguins3)
    )
  }
)

test_that(
  "ellipses", {
    wrapped_expect_doppelganger(
      "multiple ellipses of two different types",
      gf_point(eruptions ~ waiting, data = faithful, color = ~ (eruptions > 3)) |>
        gf_ellipse(type = "norm", linetype = ~ "norm") |>
        gf_ellipse(type = "t",    linetype = ~ "t")
    )
  }
)


test_that(
  "gf_props() and denom",
  {
    wrapped_expect_doppelganger(
      "proportions within panel (dodge)",
      TestData |> gf_props( ~ a | b ~ c, position = "dodge",
                      title = "proportions within panel")
    )
    wrapped_expect_doppelganger(
      "proportions within fill (dodge)",
      TestData |> gf_props( ~ a | b ~ c, fill = ~ b, position = "dodge", denom = ~ fill,
                title = "proportions within fill")
    )
    wrapped_expect_doppelganger(
      "proportions within all (dodge)",
      TestData |> gf_props( ~ a | b, fill = ~ c, position = "dodge", denom = ~ 1,
                      title = "proportions within all")

    )
    wrapped_expect_doppelganger(
      "proportions within fill (facet grid)",
      TestData |> gf_props( ~ a | b ~ c, fill = ~ b, denom = ~ fill,
                      title = "proportions within fill")

    )
    wrapped_expect_doppelganger(
      "proportions within fill (facet grid and group)",
      TestData |> gf_props( ~ a | b ~ c, fill = ~ c, group = ~b, denom = ~ fill,
                      title = "proportions within fill")
    )
    wrapped_expect_doppelganger(
      "proportions within group (facet grid)",
      TestData |> gf_props( ~ a | b ~ c, fill = ~ c, group = ~b, denom = ~ group,
                      title = "proportions in group (b = E or F)")
    )
  }
)










