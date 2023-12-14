context("isoFemale creation")

test_that("create_isofemale", {
  testthat::skip_on_os("solaris")
  pop_size <- 100
  number_of_founders <- 2
  run_time <- 100
  morgan <- 1

  pop <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                     number_of_founders,
                                                     morgan = morgan),
                            pop_size = pop_size,
                            total_runtime = run_time)


  testthat::expect_true(verify_population(pop))

 testthat::expect_silent(
   females <- create_iso_female(module = ancestry_module(input_population = pop),
                                n = 1,
                                run_time = 1000)
 )

  testthat::expect_equal(length(females), 1)
})

test_that("create_population_from_isofemales", {
  testthat::skip_on_os("solaris")
  pop_size <- 100
  number_of_founders <- 10
  run_time <- 100
  morgan <- 1

  pop1 <- simulate_admixture( module = ancestry_module(number_of_founders =
                                                         number_of_founders,
                                                       morgan = morgan),
                              pop_size = pop_size,
                             total_runtime = run_time)

  pop2 <- simulate_admixture( module = ancestry_module(number_of_founders =
                                                         number_of_founders,
                                                       morgan = morgan),
                              pop_size = pop_size,
                              total_runtime = run_time)

  pop2 <- increase_ancestor(pop2, number_of_founders)

  testthat::expect_true(verify_population(pop1))
  testthat::expect_true(verify_population(pop2))


  testthat::expect_silent(
      female_1 <- create_iso_female(module = ancestry_module(input_population = pop1),
                                    n = 1,
                                    run_time = 20000)
  )
  testthat::expect_silent(
   female_2 <- create_iso_female(module = ancestry_module(input_population = pop2),
                                 n = 1,
                                 run_time = 20000)
)
  testthat::expect_true(verify_individual(female_1[[1]]))
  testthat::expect_true(verify_individual(female_2[[1]]))

testthat::expect_silent(
  females <- create_iso_female(module = ancestry_module(input_population = pop1),
                               n = 2,
                               run_time = 2000)
)
  pop_size <- 100
  vy <- simulate_admixture(module = ancestry_module(input_population = females,
                                                    morgan = morgan),
                           pop_size = pop_size,
                           total_runtime = 200)

  testthat::expect_equal(length(vy$population), pop_size)
  testthat::expect_true(verify_population(vy))

  vy <- simulate_admixture(module = ancestry_module(input_population =
                                                      list(female_1[[1]],
                                                           female_2[[1]]),
                                                    morgan = morgan),
                           pop_size = pop_size,
                           total_runtime = 2000)

  testthat::expect_equal(length(vy$population), pop_size)
  testthat::expect_true(verify_population(vy))

  testthat::expect_silent(
    plot_chromosome(female_1[[1]]$chromosome1, 0, 1)
  )
})

test_that("cpp classes", {
  testthat::skip_on_os("solaris")
  a <- matrix(c(0.1, 1, 2, 2), nrow = 2)
  b <- matrix(c(0, 1, 1, -1), nrow = 2)
  indiv <- list(chromosome1 = a, chromosome2 = a)
  class(indiv) <- "individual"

  # chromosome 1
  testthat::expect_warning(v <- verify_individual(indiv),
                           "Chromosome doesn't start at 0")

  indiv <- list(chromosome1 = b, chromosome2 = a)
  class(indiv) <- "individual"

  # chromosome 2
  testthat::expect_warning(v <- verify_individual(indiv),
                          "Chromosome doesn't start at 0")

  a <- matrix(c(0.0, 1, 2, 2), nrow = 2)
  b <- matrix(c(0, 1, 1, -1), nrow = 2)
  indiv <- list(chromosome1 = b, chromosome2 = a)
  class(indiv) <- "individual"
  indiv$chromosome2 <-  indiv$chromosome1
  indiv$chromosome1 <- a


  a <- matrix(c(0.0, 1, 0.5, 29192875037,  1, -1), ncol = 2)

  indiv$chromosome1 <- a
  testthat::expect_warning(v <- verify_individual(indiv),
                          "Memory error recorded in chromosome")

  a <- matrix(c(0.0, 1, 0.5, -92875037,  1, -1), ncol = 2)
  indiv$chromosome2 <- a
  indiv$chromosome1 <- b
  testthat::expect_warning(v <- verify_individual(indiv),
                          "Memory error recorded in chromosome")
})

test_that("create_isofemale_data", {
  testthat::skip_on_os("solaris")

 data("dgrp2.3R.5k.data")

 females <- create_iso_female(module = sequence_module(
                                           molecular_data = dgrp2.3R.5k.data,
                                           morgan = 1),
                              n = 2,
                              inbreeding_pop_size = 100,
                              run_time = 100)

 testthat::expect_equal(length(females), 2)
})
