context("general usage")


test_that("general usage", {

  data("dgrp2.3R.5k.data")

  mks <- sample(dgrp2.3R.5k.data$markers, size = 100,
                replace = FALSE, prob = NULL)


  testthat::expect_silent(
    simulated_pop <- simulate_admixture(
      module = sequence_module(molecular_data = dgrp2.3R.5k.data,
                               morgan = 1,
                               markers = mks),
      pop_size = 1000,
      total_runtime = 11)
  )

  ### The list option is working now with genomeadmixr_data type:
  testthat::expect_output(
    testthat::expect_message(
      simulated_pop_2 <- simulate_admixture(
        module = sequence_module(molecular_data =
                                   list(dgrp2.3R.5k.data,
                                        dgrp2.3R.5k.data),
                                 markers = mks,
                                 morgan = 1),
        pop_size = 100,
        total_runtime = 10,
        verbose = TRUE),
      "found multiple input populations"
    )
  )

  testthat::expect_message(
    testthat::expect_output(
      simulated_pop_2 <- simulate_admixture(
        module = sequence_module(molecular_data =
                                   list(simulated_pop,
                                        simulated_pop),
                                 morgan = 1,
                                 markers = mks),
        pop_size = 100,
        total_runtime = 10,
        verbose = TRUE)
    )
  )

  a1 <- calculate_marker_frequency(simulated_pop, location = mks[50])
  a2 <- subset(simulated_pop$frequencies, time == 10 & location == mks[50])

  for (x in unique(a1$ancestor)) {
    a3 <- subset(a1, a1$ancestor == x)
    if (x == 0) x = "-" # 0 is - in a2.
    a4 <- subset(a2, a2$ancestor == x)
    testthat::expect_equal(a3$frequency[1],
                           a4$frequency[1], tolerance = 0.05)
  }

  selection_matrix <- matrix(nrow = 1, ncol = 5)
  selection_matrix[1, ] <- c(mks[50], 0.4, 0.7, 1.0, "t")


  testthat::expect_silent(
    iso_100 <- create_iso_female(
      module = sequence_module(molecular_data = dgrp2.3R.5k.data,
                               morgan = 1),
      inbreeding_pop_size = 100,
      n = 20,
      run_time = 20)
  )

  testthat::expect_silent(
    iso_100 <- create_iso_female(
      module = sequence_module(molecular_data = simulated_pop,
                               morgan = 1),
      inbreeding_pop_size = 100,
      n = 20,
      run_time = 20)
  )

  testthat::expect_message(
    selected_pop <- simulate_admixture(
      module = sequence_module(molecular_data = simulated_pop,
                               morgan = 1,
                               markers = mks),
      pop_size = 100,
      total_runtime = 11,
      select_matrix = selection_matrix)
  )

  testthat::expect_output(
    testthat::expect_message(
      two_pops <- simulate_admixture(
        module = sequence_module(molecular_data = list(
          simulated_pop, simulated_pop),
          markers = mks,
          morgan = 1),
        verbose = TRUE,
        migration = migration_settings(migration_rate = 0,
                                       population_size = c(100, 100),
                                       stop_at_critical_fst = TRUE,
                                       critical_fst = 0.05,
                                       generations_between_update = 100),
        total_runtime = 10)
    )
  )
})

test_that("isofemale usage", {
  data("dgrp2.3R.5k.data")

  mks = sample(dgrp2.3R.5k.data$markers, size = 300, replace = FALSE, prob = NULL)

  testthat::expect_silent(
    simulated_pop <- simulate_admixture(
      module = sequence_module(molecular_data = dgrp2.3R.5k.data,
                               morgan = 1,
                               markers = mks),
      pop_size = 100,
      total_runtime = 10)
  )

  testthat::expect_output(
    isos <- create_iso_female(
      module = sequence_module(molecular_data = simulated_pop),
      n = 20,
      inbreeding_pop_size = 100,
      run_time = 50,
      verbose = TRUE)
  )

  testthat::expect_output(
    simulated_pop <- simulate_admixture(
      module = sequence_module(molecular_data = isos[1],
                               morgan = 1,
                               markers = mks),

      pop_size = 100,
      total_runtime = 10,
      verbose = TRUE)
  )
})