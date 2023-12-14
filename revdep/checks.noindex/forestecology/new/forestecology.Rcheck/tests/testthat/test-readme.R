context("utilities")

test_that("readme code works", {
  set.seed(76)

  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(blockCV)

  comp_dist <- 1

  growth_ex <-
    compute_growth(
      census_1 = census_1_ex %>%
        mutate(sp = to_any_case(sp) %>% factor()),
      census_2 = census_2_ex %>%
        filter(!str_detect(codes, "R")) %>%
        mutate(sp = to_any_case(sp) %>% factor()),
      id = "ID"
    ) %>%
    # Compute basal area:
    mutate(basal_area = 0.0001 * pi * (dbh1 / 2)^2) %>%
    add_buffer_variable(direction = "in", size = comp_dist, region = study_region_ex)

  expect_true(check_inherits(growth_ex, "data.frame"))

  buffer_region <- study_region_ex %>%
    compute_buffer_region(direction = "in", size = comp_dist)

  expect_true(check_inherits(buffer_region, "data.frame"))

  fold1 <- rbind(c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0))
  fold2 <- rbind(c(5, 0), c(10, 0), c(10, 5), c(5, 5), c(5, 0))

  blocks_ex <- bind_rows(
    sf_polygon(fold1),
    sf_polygon(fold2)
  ) %>%
    mutate(folds = c(1, 2) %>% factor())

  SpatialBlock_ex <- spatialBlock(
    speciesData = growth_ex, k = 2, selection = "systematic", blocks = blocks_ex,
    showBlocks = FALSE, verbose = FALSE
  )

  growth_ex <- growth_ex %>%
    mutate(foldID = SpatialBlock_ex$foldID %>% factor())

  focal_vs_comp_ex <- growth_ex %>%
    create_focal_vs_comp(comp_dist, blocks = blocks_ex, id = "ID", comp_x_var = "basal_area")

  # Checks each column in focal_vs_comp is of appropriate type
  expect_true(check_inherits(focal_vs_comp_ex, "data.frame"))
  expect_true(
    check_focal_vs_comp(focal_vs_comp_ex) %>%
      unlist() %>%
      all()
  )

  comp_bayes_lm_ex <- focal_vs_comp_ex %>%
    comp_bayes_lm(prior_param = NULL)

  # Check comp_bayes_lm fit works
  expect_true(check_comp_bayes_lm(comp_bayes_lm_ex))

  focal_vs_comp_ex <- focal_vs_comp_ex %>%
    mutate(growth_hat = predict(comp_bayes_lm_ex, focal_vs_comp_ex))

  # Check model fit outputs:
  expect_true(check_inherits(focal_vs_comp_ex, "data.frame"))
  expect_true(
    check_inherits(comp_bayes_lm_ex %>% autoplot(type = "intercepts"), "ggplot")
  )
  expect_true(
    check_inherits(
      focal_vs_comp_ex %>%
        run_cv(comp_dist = comp_dist, blocks = blocks_ex) %>%
        right_join(growth_ex, by = c("focal_ID" = "ID")),
      "data.frame"
    )
  )

  # Check RMSE's are within tolerance
  expect_equal(
    focal_vs_comp_ex %>%
      rmse(truth = growth, estimate = growth_hat) %>%
      pull(.estimate),
    0.1900981,
    tolerance = .01
  )

  focal_vs_comp_ex <- focal_vs_comp_ex %>%
    run_cv(comp_dist = comp_dist, blocks = blocks_ex)

  expect_equal(
    focal_vs_comp_ex %>%
      rmse(truth = growth, estimate = growth_hat) %>%
      pull(.estimate),
    0.4068709,
    tolerance = .01
  )
})
