context("test simulate admixture data migration")

test_that("simulate_admixture_data", {
  testthat::skip_on_os("solaris")

  num_markers <- 100
  num_indiv <- 100
  chosen_markers <- 1:num_markers

  fake_input_data1 <- list()
  fake_input_data1$genomes <- matrix(data = 1,
                                     nrow = num_indiv,
                                     ncol = num_markers)


  fake_input_data1$markers <- chosen_markers

  fake_input_data2 <- list()
  fake_input_data2$genomes <- matrix(data = 2,
                                     nrow = num_indiv,
                                     ncol = num_markers)
  fake_input_data2$markers <- chosen_markers

  class(fake_input_data1) <- "genomeadmixr_data"
  class(fake_input_data2) <- "genomeadmixr_data"

  simul_two_pop <- simulate_admixture(
    module = sequence_module(molecular_data = list(fake_input_data1,
                                                   fake_input_data2),
                             markers = chosen_markers),
    migration = migration_settings(
      population_size = c(100, 100),
      migration_rate = 0.0),

    total_runtime = 100)

  a <- simul_two_pop$initial_frequency
  a1 <- a %>%
    dplyr::group_by(population, ancestor) %>%
    dplyr::summarise("mean_freq" = mean(frequency))

  b <- simul_two_pop$final_frequency
  b1 <- b %>%
    dplyr::group_by(population, ancestor) %>%
    dplyr::summarise("mean_freq" = mean(frequency))

  testthat::expect_true(all.equal(a1, b1))

  a2 <- subset(a1, a1$population == 1 &
                 a1$ancestor == 1)
  testthat::expect_equal(a2$mean_freq, 1)
  b2 <- subset(b1, b1$population == 1 &
                 b1$ancestor == 1)
  testthat::expect_equal(b2$mean_freq, 1)


  simul_two_pop <- simulate_admixture(
    module = sequence_module(molecular_data = list(fake_input_data1,
                                                   fake_input_data2),
                             markers = chosen_markers),
    migration = migration_settings(population_size = c(100, 100),
                                   migration_rate = 0.5),
    total_runtime = 100)

  a <- simul_two_pop$initial_frequency
  a1 <- a %>%
    dplyr::group_by(population, ancestor) %>%
    dplyr::summarise("mean_freq" = mean(frequency))

  b <- simul_two_pop$final_frequency
  b1 <- b %>%
    dplyr::group_by(population, ancestor) %>%
    dplyr::summarise("mean_freq" = mean(frequency))

  v <- all.equal(a1, b1)
  testthat::expect_false(v == TRUE)
})
