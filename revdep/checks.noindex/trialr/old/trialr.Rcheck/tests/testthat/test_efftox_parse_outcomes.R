# Test efftox_parse_outcomes.
# This function gets its own test script to keep things tidy.

# Test some cases that should work ----
# Regular case
test_that('EffTox outcomes "" parse correctly', {
  x <- efftox_parse_outcomes('', as.list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 0)
  expect_equal(length(x$doses), 0)
  expect_equal(length(x$eff), 0)
  expect_equal(length(x$tox), 0)
})

test_that('EffTox outcomes "" parse correctly to list', {
  x <- efftox_parse_outcomes('', as.list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 0)
  expect_equal(length(x$doses), 0)
  expect_equal(length(x$eff), 0)
  expect_equal(length(x$tox), 0)
})

test_that('EffTox outcomes "1EEE 3TTT" parse correctly', {
  x <- efftox_parse_outcomes('1EEE 3TTT', as.list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 6)
  expect_equal(x$doses, c(1, 1, 1, 3, 3, 3))
  expect_equal(x$eff, c(1, 1, 1, 0, 0, 0))
  expect_equal(x$tox, c(0, 0, 0, 1, 1, 1))
})

test_that('EffTox outcomes "1EEE 3TTT" parse correctly to list', {
  x <- efftox_parse_outcomes('1EEE 3TTT', as.list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 6)
  expect_equal(x$doses, c(1, 1, 1, 3, 3, 3))
  expect_equal(x$eff, c(1, 1, 1, 0, 0, 0))
  expect_equal(x$tox, c(0, 0, 0, 1, 1, 1))
})

# A regular case with no spaces; I do not recommend this because it is hard to
# read but it should work, nevertheless.
test_that('EffTox outcomes "1E2T2N2E2E" parse correctly', {
  x <- efftox_parse_outcomes('1E2T2N2E2E', as.list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 5)
  expect_equal(x$doses, c(1, 2, 2, 2, 2))
  expect_equal(x$eff, c(1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 0, 0, 0))
})

test_that('EffTox outcomes "1E2T2N2E2E" parse correctly to list', {
  x <- efftox_parse_outcomes('1E2T2N2E2E', as.list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 5)
  expect_equal(x$doses, c(1, 2, 2, 2, 2))
  expect_equal(x$eff, c(1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 0, 0, 0))
})

# Odd shaped cohorts
test_that('EffTox outcomes "1ETT 2T 2NBENTE 3E" parse correctly', {
  x <- efftox_parse_outcomes('1ETT 2T 2NBENTE 3E', as.list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 11)
  expect_equal(x$doses, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3))
  expect_equal(x$eff, c(1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

test_that('EffTox outcomes "1ETT 2T 2NBENTE 3E" parse correctly to list', {
  x <- efftox_parse_outcomes('1ETT 2T 2NBENTE 3E', as.list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 11)
  expect_equal(x$doses, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3))
  expect_equal(x$eff, c(1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

# Silly but valid dose-levels
test_that('EffTox outcomes "96ETT 40T 1NBENTE 174E" parse correctly', {
  x <- efftox_parse_outcomes('96ETT 40T 1NBENTE 174E', as.list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 11)
  expect_equal(x$doses, c(96, 96, 96, 40, 1, 1, 1, 1, 1, 1, 174))
  expect_equal(x$eff, c(1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

test_that('EffTox outcomes "96ETT 40T 1NBENTE 174E" parse correctly to list', {
  x <- efftox_parse_outcomes('96ETT 40T 1NBENTE 174E', as.list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 11)
  expect_equal(x$doses, c(96, 96, 96, 40, 1, 1, 1, 1, 1, 1, 174))
  expect_equal(x$eff, c(1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

# Leading white-space
test_that('EffTox outcomes " 1ETT 2T 2NBENTE 2E" parse correctly', {
  x <- efftox_parse_outcomes(' 1ETT 2T 2NBENTE 2E', as.list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 11)
  expect_equal(x$doses, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2))
  expect_equal(x$eff, c(1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

test_that('EffTox outcomes " 1ETT 2T 2NBENTE 2E" parse correctly to list', {
  x <- efftox_parse_outcomes(' 1ETT 2T 2NBENTE 2E', as.list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 11)
  expect_equal(x$doses, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2))
  expect_equal(x$eff, c(1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

# Test some cases that should not work ----
# A string that is not welcome here
test_that('EffTox outcomes "12ETT Nigel Farage" fail to parse', {
  expect_error(efftox_parse_outcomes('12ETT Nigel Farage', as.list = FALSE))
})

test_that('EffTox outcomes "12ETT Nigel Farage" fail to parse to list', {
  expect_error(efftox_parse_outcomes('12ETT Nigel Farage', as.list = TRUE))
})

# Decimal dose-levels
test_that('EffTox outcomes " 1ETT 2.0T 2NBENTE 2E" fail to parse', {
  expect_error(efftox_parse_outcomes(' 1ETT 2.0T 2NBENTE 2E', as.list = FALSE))
})

test_that('EffTox outcomes " 1ETT 2.0T 2NBENTE 2E" fail to parse to list', {
  expect_error(efftox_parse_outcomes(' 1ETT 2.0T 2NBENTE 2E', as.list = TRUE))
})

test_that('EffTox outcomes ".1ETT 2T 2NBENTE 2E" fail to parse', {
  expect_error(efftox_parse_outcomes('.1ETT 2T 2NBENTE 2E', as.list = FALSE))
})

test_that('EffTox outcomes ".1ETT 2T 2NBENTE 2E" fail to parse to list', {
  expect_error(efftox_parse_outcomes('.1ETT 2T 2NBENTE 2E', as.list = TRUE))
})

# Negative dose-levels
test_that('EffTox outcomes "12ETT 2T 2NBENTE -1E" fail to parse', {
  expect_error(efftox_parse_outcomes('12ETT 2T 2NBENTE -1E', as.list = FALSE))
})

test_that('EffTox outcomes "12ETT 2T 2NBENTE -1E" fail to parse to list', {
  expect_error(efftox_parse_outcomes('12ETT 2T 2NBENTE -1E', as.list = TRUE))
})

test_that('EffTox outcomes "-12ETT 2T 2NBENTE 1E" fail to parse', {
  expect_error(efftox_parse_outcomes('-12ETT 2T 2NBENTE 1E', as.list = FALSE))
})

test_that('EffTox outcomes "-12ETT 2T 2NBENTE 1E" fail to parse to list', {
  expect_error(efftox_parse_outcomes('-12ETT 2T 2NBENTE 1E', as.list = TRUE))
})

test_that('EffTox outcomes "12ETT 2T -2NBENTE 1E" fail to parse', {
  expect_error(efftox_parse_outcomes('12ETT 2T -2NBENTE 1E', as.list = FALSE))
})

test_that('EffTox outcomes "12ETT 2T -2NBENTE 1E" fail to parse to list', {
  expect_error(efftox_parse_outcomes('12ETT 2T -2NBENTE 1E', as.list = TRUE))
})

# Zero dose-level
test_that('EffTox outcomes "1T 0NN" fail to parse', {
  expect_error(efftox_parse_outcomes('1T 0NN', as.list = FALSE))
})

test_that('EffTox outcomes "1T 0NN" fail to parse to list', {
  expect_error(efftox_parse_outcomes('1T 0NN', as.list = TRUE))
})

test_that('EffTox outcomes "0ENTBENTB" fail to parse', {
  expect_error(efftox_parse_outcomes('0ENTBENTB', as.list = FALSE))
})

test_that('EffTox outcomes "0ENTBENTB" fail to parse to list', {
  expect_error(efftox_parse_outcomes('0ENTBENTB', as.list = TRUE))
})

# Nothing but white-space
test_that('EffTox outcomes " " fail to parse', {
  expect_error(efftox_parse_outcomes(' ', as.list = FALSE))
})

test_that('EffTox outcomes " " fail to parse to list', {
  expect_error(efftox_parse_outcomes(' ', as.list = TRUE))
})

# Nothing
# test_that('EffTox outcomes "" fail to parse', {
#   expect_error(efftox_parse_outcomes('', as.list = FALSE))
# })
#
# test_that('EffTox outcomes "" fail to parse to list', {
#   expect_error(efftox_parse_outcomes('', as.list = TRUE))
# })

# Looks plausible
test_that('EffTox outcomes "1ET TNB" fail to parse', {
  expect_error(efftox_parse_outcomes('1ET TNB', as.list = FALSE))
})

test_that('EffTox outcomes "1ET TNB" fail to parse to list', {
  expect_error(efftox_parse_outcomes('1ET TNB', as.list = TRUE))
})

test_that('EffTox outcomes "1ET T3NB" fail to parse', {
  expect_error(efftox_parse_outcomes('1ET T3NB', as.list = FALSE))
})

test_that('EffTox outcomes "1ET T3NB" fail to parse to list', {
  expect_error(efftox_parse_outcomes('1ET T3NB', as.list = TRUE))
})

test_that('EffTox outcomes "1ET 3TNB 4" fail to parse', {
  expect_error(efftox_parse_outcomes('1ET 3TNB 4', as.list = FALSE))
})

test_that('EffTox outcomes "1ET 3TNB 4" fail to parse to list', {
  expect_error(efftox_parse_outcomes('1ET 3TNB 4', as.list = TRUE))
})

