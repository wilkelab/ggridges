context("utilities")

test_that("utilities", {
  testthat::skip_on_os("solaris")
  vx <- simulate_admixture(module = ancestry_module(number_of_founders = 50),
                           pop_size = 100,
                           total_runtime = 5)
  testthat::expect_silent(
    plot_chromosome(vx$population[[1]]$chromosome1)
  )

  vx <- simulate_admixture(module = ancestry_module(markers = seq(0, 1, by = 0.01)),
                           total_runtime = 100,
                           pop_size = 100)
  testthat::expect_silent(
    plot_over_time(vx$frequencies, focal_location = 0.5)
  )

  vy <- simulate_admixture(migration =
                             migration_settings(migration_rate = 0.01))

  testthat::expect_error(plot_over_time(vy$frequencies, focal_location = 0.5))

  vy <- simulate_admixture(
    module = ancestry_module(markers = 0.5),
      migration = migration_settings(migration_rate = 0.01))

  testthat::expect_silent(
    plot_over_time(vy$frequencies, focal_location = 0.5)
  )
})

test_that("initial_frequencies", {
  testthat::skip_on_os("solaris")
  testthat::expect_error(
    simulate_admixture(
          migration = migration_settings(migration_rate = 0.1,
                                         initial_frequencies = c(0.5, 0.5),
                                         population_size = c(1000, 1000)),
          total_runtime = 100
      )
  )
  testthat::expect_message(
  testthat::expect_warning(
      vx <- simulate_admixture(total_runtime = 5,
                migration = migration_settings(
                  migration_rate = 0.0,
                  initial_frequencies = c(1, 1, 0, 0, 0, 0, 1, 1)))
  ))

  # warning that frequencies don't add up to 1.
testthat::expect_warning(
  vx <- simulate_admixture(total_runtime = 5,
                             migration = migration_settings(
                               migration_rate = 0.0,
                               initial_frequencies = list(c(1, 1, 0, 0),
                                                          c(0, 0, 1, 1)))
                           )
  )

  testthat::expect_error(
    simulate_admixture(total_runtime = 5,
                         migration = migration_settings(
                           migration_rate = 0.0,
                           initial_frequencies = c(1, 1, 0, 0,
                                                   0, 0, 1, 1, 1))
                       )
  )
})

testthat::test_that("random markers", {
  testthat::skip_on_os("solaris")
  set.seed(42)
  vx <- create_random_markers(1e3)
  vy <- create_random_markers(1e6)
  testthat::expect_true(length(vy) > length(vx))
})

testthat::test_that("verify datatypes", {

  vx <- simulate_admixture(total_runtime = 2,
                           pop_size = 100,
                           migration = migration_settings(migration_rate = 0.001))

testthat::expect_warning(
  check_input_pop(vx)
)
})

