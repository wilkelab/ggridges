context("allele_frequencies")


test_that("calculate_allele_frequencies", {
  testthat::skip_on_os("solaris")
  pop_size <- 100
  number_of_founders <- 2
  run_time <- 5

  morgan <- 1

  found <- c()
  for (r in 1:30) {
    vx <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                        number_of_founders,
                                                      morgan = morgan),
                             pop_size = pop_size,
                             total_runtime = run_time)

    testthat::expect_true(verify_population(vx))

    for (i in 1:pop_size) {
      found <- rbind(found,
                     calc_allele_frequencies(vx$population[[i]],
                                             alleles =
                                               rep(0, number_of_founders * 2)
                                             )
      )
    }
  }

  v <- colMeans(found)
  testthat::expect_equal(v[[1]], 0.5, tolerance = 0.2)

  testthat::expect_equal(v[[2]], 0.5, tolerance = 0.2)

  found <- c()
  for (r in 1:30) {
    vx <- simulate_admixture(module = ancestry_module(number_of_founders = 4,
                                                      morgan = morgan),
                             pop_size = pop_size,
                             total_runtime = run_time)

    testthat::expect_true(verify_population(vx))

    for (i in 1:pop_size) {
      found <- rbind(found,
                     calc_allele_frequencies(vx$population[[i]],
                                             alleles =
                                               rep(0, number_of_founders * 2)
                                             )
      )
    }
  }

  v <- mean(colMeans(found))
  testthat::expect_equal(v, 0.25, tolerance = 0.05)


  number_of_founders <- 20
  sourcepop <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                             number_of_founders),
                                  pop_size = 1000,
                                  total_runtime = 100)

  testthat::expect_true(verify_population(sourcepop))

  freq_output <- calculate_allele_frequencies(sourcepop$population)

  b <- subset(freq_output, freq_output$location == 0)

  testthat::expect_equal(mean(b$frequency),
                         1 / number_of_founders,
                         tolerance = 1 / number_of_founders)
  testthat::expect_equal(sum(b$frequency), 1,
                         tolerance = 1 / number_of_founders)

  number_of_founders <- 5
  sourcepop <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                             number_of_founders),
                                  pop_size = 1000,
                                  total_runtime = 100)

  testthat::expect_true(verify_population(sourcepop))

  freq_output <- calculate_allele_frequencies(sourcepop$population)
  plot_frequencies(sourcepop)

  testthat::expect_equal(length(unique(freq_output$ancestor)),
                         number_of_founders)

  b <- subset(freq_output, freq_output$location == 0)

  testthat::expect_equal(sum(b$frequency), 1, tolerance = 0.1)
  testthat::expect_equal(mean(b$frequency),
                         1 / number_of_founders,
                         tolerance = 0.1)

  number_founders <- 20
  sourcepop <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                             number_of_founders),
                                  pop_size = 1000,
                                  total_runtime = 1)

  testthat::expect_true(verify_population(sourcepop))
})
