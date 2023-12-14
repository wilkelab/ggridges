context("test input data")

test_that("input data", {
  testthat::skip_on_os("solaris")

  chosen_markers <- 1:100

  fake_input_data1 <- GenomeAdmixR::create_artificial_genomeadmixr_data(
    number_of_individuals = 100,
    marker_locations = chosen_markers,
    used_nucleotides = 1
  )

  fake_input_data2 <- GenomeAdmixR::create_artificial_genomeadmixr_data(
    number_of_individuals = 100,
    marker_locations = chosen_markers,
    used_nucleotides = 2
  )

  combined_data <- combine_input_data(input_data_list = list(fake_input_data1,
                                                             fake_input_data2),
                                      frequencies = c(0.3, 0.7),
                                      pop_size = 1000)

  vv <- table(combined_data$genomes)
  a <- vv[[1]] / (sum(vv))
  testthat::expect_equal(a, 0.3, tolerance = 0.1)

  testthat::expect_error(
    combined_data <- combine_input_data(input_data_list =
                                          list(fake_input_data1,
                                               fake_input_data2),
                                        frequencies = c(0.1, 0.7, 0.2),
                                        pop_size = 1000)
  )

  chosen_markers <- 1:1000

  fake_input_data1 <- GenomeAdmixR::create_artificial_genomeadmixr_data(
    number_of_individuals = 100,
    marker_locations = chosen_markers,
    used_nucleotides = 1
  )

  fake_input_data2 <- GenomeAdmixR::create_artificial_genomeadmixr_data(
    number_of_individuals = 100,
    marker_locations = chosen_markers,
    used_nucleotides = 2
  )

  combined_data <- combine_input_data(input_data_list = list(fake_input_data1,
                                                             fake_input_data2),
                                      frequencies = c(0.5, 0.5),
                                      pop_size = 100)

  simulation_result <- simulate_admixture(module = sequence_module(molecular_data =
                                                                     combined_data,
                                                                   morgan = 1),
                                               pop_size = 1000,
                                               total_runtime = 10)

  num_j <- c()
  for (i in seq_along(simulation_result$population)) {
    focal_indiv <- simulation_result$population[[i]]
    chrom1 <- focal_indiv$chromosome1
    chrom2 <- focal_indiv$chromosome2
    num_j_1 <- sum(abs(diff(chrom1[, 2])))
    num_j_2 <- sum(abs(diff(chrom2[, 2])))
    num_j <- c(num_j, num_j_1, num_j_2)
  }
  expected_j <- junctions::number_of_junctions(N = 1000, R = 1000, t = 10)
  testthat::expect_equal(mean(num_j), expected_j, tolerance = 1)
})

test_that("input data simulation", {
  testthat::skip_on_os("solaris")

  vx <- simulate_admixture(pop_size = 100,
                           total_runtime = 100)

  vy <- simulation_data_to_genomeadmixr_data(vx,
                                              markers = seq(0, 1,
                                                            length.out = 1000))

  for (i in seq_along(vx$population)) {
    num_j_1 <- sum(abs(diff(vx$population[[i]]$chromosome1)))
    num_j_2 <- sum(abs(diff(vx$population[[i]]$chromosome2)))

    index_c1 <- 1 + (i - 1) * 2
    index_c2 <- index_c1 + 1
    nj1 <- sum(abs(diff(vy$genomes[index_c1, ])))
    nj2 <- sum(abs(diff(vy$genomes[index_c2, ])))
    testthat::expect_lte(nj1, num_j_1)
    testthat::expect_lte(nj2, num_j_2)
  }
})
