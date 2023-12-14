context("simulate_migration")

test_that("simulate_migration base", {
  testthat::skip_on_os("solaris")
  testthat::expect_silent(
    vx <- simulate_admixture(migration = migration_settings(migration_rate = 0.1),
                           total_runtime = 5)
  )
})

test_that("simulate_migration", {
  testthat::skip_on_os("solaris")
  vx <- simulate_admixture(migration = migration_settings(migration_rate = 0.1),
                           total_runtime = 10)

  testthat::expect_true(verify_population(vx$population_1))
  testthat::expect_true(verify_population(vx$population_2))

  testthat::expect_silent(
    vz <- simulate_admixture(module = ancestry_module(input_population =
                                                        list(vx$population_1[[1]],
                                                             vx$population_2[[2]])),
                            migration = migration_settings(migration_rate = 0.01),
                             total_runtime = 10)
  )

  markers <- seq(from = 0.4, to = 0.6, length.out = 100)
  vy <- simulate_admixture(module = ancestry_module(markers = markers),
                           migration =
                             migration_settings(migration_rate = 0.01,
                                                initial_frequencies =
                                                  list(c(0.5, 0.5, 0, 0),
                                                       c(0, 0, 0.5, 0.5))),
                           total_runtime = 100)

  testthat::expect_true(verify_population(vy$population_1))
  testthat::expect_true(verify_population(vy$population_2))

  testthat::expect_true(length(markers) ==
                          length(unique(vy$frequencies$location)))


  select_matrix <- matrix(NA, nrow = 1, ncol = 5)

  s <- 5
  select_matrix[1, ] <- c(0.5, 1.0, 1.0 + 0.25 * s, 1.0 + s, 0)

  markers <- seq(from = 0.4, to = 0.60, by = 0.01)

  testthat::expect_message(
    vy <- simulate_admixture(module = ancestry_module(markers = markers),
                             migration = migration_settings(migration_rate = 0.01,
                                                            initial_frequencies = list(c(1, 0),
                                                                                       c(0, 1))),
                             select_matrix = select_matrix,
                             total_runtime = 100)
  )

  plot_difference_frequencies(vy)
  plot_start_end(vy)

  plot_frequencies(vy$population_1, locations = c(0.3, 0.5, 0.8))
  plot_frequencies(vy$population_2, locations = c(0.3, 0.5, 0.8))
  vv <- plot_joyplot_frequencies(vy$frequencies, time_points = c(0, 10, 50))

  testthat::expect_silent(
  vy <- simulate_admixture(module = ancestry_module(markers = 0.5,
                                                    track_junctions = TRUE),
    migration = migration_settings(migration_rate = 0.0,
                                   initial_frequencies =
                                     list(c(0.5, 0.5, 0, 0),
                                          c(0, 0, 0.5, 0.5))),
    total_runtime = 100)
  )
})
