
test_that("expected_number_junctions", {
  testthat::skip_on_os("solaris")
  test_expected_junction_number <- function(pop_size,
                                            run_time,
                                            morgan,
                                            replicates) {
    found <- c()
    for (r in 1:replicates) {
      vx <- simulate_admixture(module = ancestry_module(morgan = morgan),
                               pop_size = pop_size,
                               total_runtime = run_time)

      junct <- calculate_dist_junctions(vx$population)
      found <- c(found, mean(junct))
    }

    require(junctions)
    expected <- junctions::number_of_junctions(N = pop_size,
                                               H_0 = 0.5,
                                               C = morgan,
                                               t = run_time)

    testthat::expect_equal(mean(found), expected, tolerance = 1)
  }

  test_expected_junction_number(pop_size = 1000, run_time = 20,
                                morgan = 1, replicates = 100)

  test_expected_junction_number(pop_size = 1000, run_time = 20,
                                morgan = 0.5, replicates = 100)

  test_expected_junction_number(pop_size = 1000, run_time = 20,
                                morgan = 3, replicates = 100)

  vx <- simulate_admixture(pop_size = 100,
                           total_runtime = 5)

  plot_dist_junctions(vx$population)
})
