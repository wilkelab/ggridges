# Test parse_eff_tox_dose_finding_outcomes.

# Test some cases that should work ----
# Regular cases

# The empty string should not in fail
test_that('parse_eff_tox_dose_finding_outcomes parses "" correctly', {
  x <- parse_eff_tox_dose_finding_outcomes('')
  expect_true(is.list(x))
  expect_equal(length(x), 0)
})

test_that('parse_eff_tox_dose_finding_outcomes parses "1EEE 3TTT" correctly', {
  x <- parse_eff_tox_dose_finding_outcomes('1EEE 3TTT')

  expect_true(is.list(x))
  expect_equal(length(x), 2)

  expect_equal(x[[1]]$dose, 1)
  expect_equal(x[[1]]$outcomes, 'EEE')
  expect_equal(x[[2]]$dose, 3)
  expect_equal(x[[2]]$outcomes, 'TTT')
})

# A regular case with no spaces; I do not recommend this because it is hard to
# read but it should work, nevertheless.
test_that('parse_eff_tox_dose_finding_outcomes parses "1E2T2N2E2E" correctly', {
  x <- parse_eff_tox_dose_finding_outcomes('1E2T2N2E3E')

  expect_true(is.list(x))
  expect_equal(length(x), 5)

  expect_equal(x[[1]]$dose, 1)
  expect_equal(x[[1]]$outcomes, 'E')
  expect_equal(x[[2]]$dose, 2)
  expect_equal(x[[2]]$outcomes, 'T')
  expect_equal(x[[3]]$dose, 2)
  expect_equal(x[[3]]$outcomes, 'N')
  expect_equal(x[[4]]$dose, 2)
  expect_equal(x[[4]]$outcomes, 'E')
  expect_equal(x[[5]]$dose, 3)
  expect_equal(x[[5]]$outcomes, 'E')
})

# n=1 example
test_that('parse_eff_tox_dose_finding_outcomes parses "5T" correctly', {
  x <- parse_eff_tox_dose_finding_outcomes('5T')

  expect_true(is.list(x))
  expect_equal(length(x), 1)

  expect_equal(x[[1]]$dose, 5)
  expect_equal(x[[1]]$outcomes, 'T')
})

# Irregular cohorts
test_that('parse_eff_tox_dose_finding_outcomes parses "1ETT 2T 2NBENTE 3E" correctly', {
  x <- parse_eff_tox_dose_finding_outcomes('1ETT 2T 2NBENTE 3E')

  expect_true(is.list(x))
  expect_equal(length(x), 4)

  expect_equal(x[[1]]$dose, 1)
  expect_equal(x[[1]]$outcomes, 'ETT')
  expect_equal(x[[2]]$dose, 2)
  expect_equal(x[[2]]$outcomes, 'T')
  expect_equal(x[[3]]$dose, 2)
  expect_equal(x[[3]]$outcomes, 'NBENTE')
  expect_equal(x[[4]]$dose, 3)
  expect_equal(x[[4]]$outcomes, 'E')
})

# Silly but valid dose-levels
test_that('parse_eff_tox_dose_finding_outcomes parses "96ETT 40T 1NBENTE 174E" correctly', {
  x <- parse_eff_tox_dose_finding_outcomes('96ETT 40T 1NBENTE 174E')

  expect_true(is.list(x))
  expect_equal(length(x), 4)

  expect_equal(x[[1]]$dose, 96)
  expect_equal(x[[1]]$outcomes, 'ETT')
  expect_equal(x[[2]]$dose, 40)
  expect_equal(x[[2]]$outcomes, 'T')
  expect_equal(x[[3]]$dose, 1)
  expect_equal(x[[3]]$outcomes, 'NBENTE')
  expect_equal(x[[4]]$dose, 174)
  expect_equal(x[[4]]$outcomes, 'E')
})

# Leading white-space
test_that('parse_eff_tox_dose_finding_outcomes parses " 1ETT 2T 2NBENTE 2E" correctly', {
  x <- parse_eff_tox_dose_finding_outcomes(' 1ETT 2T 2NBENTE 2E')

  expect_true(is.list(x))
  expect_equal(length(x), 4)

  expect_equal(x[[1]]$dose, 1)
  expect_equal(x[[1]]$outcomes, 'ETT')
  expect_equal(x[[2]]$dose, 2)
  expect_equal(x[[2]]$outcomes, 'T')
  expect_equal(x[[3]]$dose, 2)
  expect_equal(x[[3]]$outcomes, 'NBENTE')
  expect_equal(x[[4]]$dose, 2)
  expect_equal(x[[4]]$outcomes, 'E')
})

# Trailing white space
test_that('parse_eff_tox_dose_finding_outcomes parses "1ETT 2T 2NBENTE 2E  " correctly', {
  x <- parse_eff_tox_dose_finding_outcomes(' 1ETT 2T 2NBENTE 2E  ')

  expect_true(is.list(x))
  expect_equal(length(x), 4)

  expect_equal(x[[1]]$dose, 1)
  expect_equal(x[[1]]$outcomes, 'ETT')
  expect_equal(x[[2]]$dose, 2)
  expect_equal(x[[2]]$outcomes, 'T')
  expect_equal(x[[3]]$dose, 2)
  expect_equal(x[[3]]$outcomes, 'NBENTE')
  expect_equal(x[[4]]$dose, 2)
  expect_equal(x[[4]]$outcomes, 'E')
})

# Test some cases that should not work ----
# A string that is not welcome here
test_that('parse_eff_tox_dose_finding_outcomes parses "12ETT Nigel Farage" with failure', {
  expect_error(parse_eff_tox_dose_finding_outcomes('12ETT Nigel Farage'))
})

# Decimal dose-levels
test_that('parse_eff_tox_dose_finding_outcomes parses " 1ETT 2.0T 2NBENTE 2E" with failure', {
  expect_error(parse_eff_tox_dose_finding_outcomes(' 1ETT 2.0T 2NBENTE 2E'))
})

test_that('parse_eff_tox_dose_finding_outcomes parses ".1ETT 2T 2NBENTE 2E" with failure', {
  expect_error(parse_eff_tox_dose_finding_outcomes('.1ETT 2T 2NBENTE 2E'))
})

# Negative dose-levels
test_that('parse_eff_tox_dose_finding_outcomes parses "12ETT 2T 2NBENTE -1E" with failure', {
  expect_error(parse_eff_tox_dose_finding_outcomes('12ETT 2T 2NBENTE -1E'))
})

test_that('parse_eff_tox_dose_finding_outcomes parses "-12ETT 2T 2NBENTE 1E" with failure', {
  expect_error(parse_eff_tox_dose_finding_outcomes('-12ETT 2T 2NBENTE 1E'))
})

test_that('parse_eff_tox_dose_finding_outcomes parses "12ETT 2T -2NBENTE 1E" with failure', {
  expect_error(parse_eff_tox_dose_finding_outcomes('12ETT 2T -2NBENTE 1E'))
})

# Zero dose-level
test_that('parse_eff_tox_dose_finding_outcomes parses "1T 0NN" with failure', {
  expect_error(parse_eff_tox_dose_finding_outcomes('1T 0NN'))
})

test_that('parse_eff_tox_dose_finding_outcomes parses "0ENTBENTB" with failure', {
  expect_error(parse_eff_tox_dose_finding_outcomes('0ENTBENTB'))
})

# Nothing but white-space
test_that('parse_eff_tox_dose_finding_outcomes parses " " with failure', {
  expect_error(parse_eff_tox_dose_finding_outcomes(' '))
})

# Looks plausible
test_that('parse_eff_tox_dose_finding_outcomes parses "1ET TNB" with failure', {
  expect_error(parse_eff_tox_dose_finding_outcomes('1ET TNB'))
})

test_that('parse_eff_tox_dose_finding_outcomes parses "1ET T3NB" with failure', {
  expect_error(parse_eff_tox_dose_finding_outcomes('1ET T3NB'))
})

test_that('parse_eff_tox_dose_finding_outcomes parses "1ET 3TNB 4" with failure', {
  expect_error(parse_eff_tox_dose_finding_outcomes('1ET 3TNB 4'))
})
