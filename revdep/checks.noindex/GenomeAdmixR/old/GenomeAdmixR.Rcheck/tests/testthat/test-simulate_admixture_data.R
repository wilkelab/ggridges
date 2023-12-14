context("test simulate admixture data")

test_that("simulate_admixture_data", {
  testthat::skip_on_os("solaris")

  num_markers <- 100
  num_indiv <- 100
  chosen_markers <- 1:num_markers

  fake_input_data1 <- create_artificial_genomeadmixr_data(
    number_of_individuals = num_indiv,
    marker_locations = chosen_markers,
    used_nucleotides = 1:2
  )

  fake_input_data2 <- create_artificial_genomeadmixr_data(
    number_of_individuals = num_indiv,
    marker_locations = chosen_markers,
    used_nucleotides = 3:4
  )

  simul_pop <- simulate_admixture(module = sequence_module(
                                    molecular_data = list(fake_input_data1,
                                                          fake_input_data2),
                                    initial_frequencies = c(0.5, 0.5),
                                    markers = chosen_markers,
                                    morgan = 1),
                                  pop_size = 100,
                                  total_runtime = 100)

  testthat::expect_silent(
    plot_chromosome(simul_pop$population[[1]]$chromosome1)
  )

  testthat::expect_silent(
    plot_difference_frequencies(results = simul_pop)
  )
  testthat::expect_silent(
    calculate_allele_frequencies(source_pop = simul_pop,
                                 progress_bar = FALSE)

  )
  testthat::expect_silent(
    plot_frequencies(simul_pop,
                     locations = unique(simul_pop$frequencies$location))
  )
  testthat::expect_silent(
    plot_over_time(simul_pop$frequencies, focal_location = 50)
  )
  testthat::expect_silent(
    plot_start_end(simul_pop)
  )

  testthat::expect_silent(
    calculate_heterozygosity(simul_pop$population,
                             locations = unique(simul_pop$frequencies$location))
  )

  testthat::expect_silent(
    plot_joyplot_frequencies(simul_pop$frequencies,
                             time_points = c(0, 10, 50))
  )

  testthat::expect_silent(
    calculate_ld(simul_pop$population)
  )
  testthat::expect_silent(
    calculate_marker_frequency(simul_pop, location = 50)
  )
})

test_that("simulate_admixture_data_mutation", {
  testthat::skip_on_os("solaris")

  num_markers <- 100
  num_indiv <- 100
  chosen_markers <- 1:num_markers

  fake_input_data1 <- create_artificial_genomeadmixr_data(
    number_of_individuals = num_indiv,
    marker_locations = chosen_markers,
    used_nucleotides = 1:2
  )

  fake_input_data2 <- create_artificial_genomeadmixr_data(
    number_of_individuals = num_indiv,
    marker_locations = chosen_markers,
    used_nucleotides = 3:4
  )

  combined_data <- combine_input_data(input_data_list = list(fake_input_data1,
                                                             fake_input_data2),
                                      frequencies = c(0.5, 0.5),
                                      pop_size = 1000)

  sub_matrix <- matrix(0.25, nrow = 4, ncol = 4)

  testthat::expect_output(
    simul_pop <- simulate_admixture(module = sequence_module(
                                                molecular_data = combined_data,
                                                mutation_rate = 0.1,
                                                substitution_matrix = sub_matrix,
                                                markers = chosen_markers),
                                    pop_size = 100,
                                    total_runtime = 100,
                                    verbose = TRUE)
  )

  a1 <- simul_pop$initial_frequency
  a2 <- simul_pop$final_frequency

  aa <- a1 %>%
          dplyr::group_by(ancestor) %>%
    dplyr::summarise("mean_freq" = mean(frequency))

  bb <- a2 %>%
    dplyr::group_by(ancestor) %>%
    dplyr::summarise("mean_freq" = mean(frequency))

  testthat::expect_equal(mean(bb$mean_freq[2:5]), 0.25)

  bases <- c("a", "c", "t", "g")
  for (i in 1:4) {
    sub_matrix <- matrix(0, nrow = 4, ncol = 4)
    sub_matrix[, i] <- 1

    testthat::expect_output(
      testthat::expect_warning(
        simul_pop <- simulate_admixture(module = sequence_module(
                                                    molecular_data = combined_data,
                                                    mutation_rate = 0.1,
                                                    substitution_matrix = sub_matrix,
                                                    markers = chosen_markers),
                                        pop_size = 100,
                                        total_runtime = 100,
                                        verbose = TRUE)
      )
    )

    bb <- simul_pop$final_frequency %>%
      dplyr::group_by(ancestor) %>%
      dplyr::summarise("mean_freq" = mean(frequency))

    highest_base <- bb$ancestor[which.max(bb$mean_freq)]
    testthat::expect_equal(bases[i], highest_base)
  }
})

test_that("simulate_admixture_data_recombination_map", {
  num_markers <- 2
  num_indiv <- 10
  chosen_markers <- c(1000000, 2000000)
  recom_rate <- 1

  fake_input_data1 <- create_artificial_genomeadmixr_data(
    number_of_individuals = num_indiv,
    marker_locations = chosen_markers,
    used_nucleotides = 1
  )

  fake_input_data2 <- create_artificial_genomeadmixr_data(
    number_of_individuals = num_indiv,
    marker_locations = chosen_markers,
    used_nucleotides = 2
  )

  pop_size <- 10000
  simul_pop <- simulate_admixture(module = sequence_module(
                                              molecular_data = list(fake_input_data1,
                                                                    fake_input_data2),
                                              initial_frequencies = c(0.5, 0.5),
                                              markers = chosen_markers,
                                              recombination_rate = recom_rate),
                                  pop_size = pop_size,
                                  total_runtime = 2,
                                  verbose = FALSE)


  found_junctions <- c()
  for (i in seq_along(simul_pop$population)) {
    a <- simul_pop$population[[i]]$chromosome1[, 2]
    b <- simul_pop$population[[i]]$chromosome2[, 2]
    a <- sum(abs(diff(a)))
    b <- sum(abs(diff(b)))
    found_junctions <- c(found_junctions, a, b)
  }

  all_j <- sum(found_junctions)


  # expected number recombinations is:
  # 0.5: only half the population is admixing
  # 2  : 2 chromosomes
  # recom_rate / 100 : recom_rate is in cM, and we want in Morgan.
  # recom_rate per bp -> div by 1e6
  # then, num indivs (pop size) * 0.5 * recom_prob = expected junctions

  recom_prob <-  (recom_rate / 100) * diff(chosen_markers) / 1e6

  expected_num_j <- 2 * pop_size * recom_prob * 0.5

  testthat::expect_equal(all_j, expected_num_j, tolerance = 0.4)
})
