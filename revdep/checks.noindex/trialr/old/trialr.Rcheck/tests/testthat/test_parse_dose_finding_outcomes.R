
# Test parse_dose_finding_outcomes ----
# Test some cases that should work

# The empty string should not in fail
test_that('Dose-finding outcomes "" parses correctly', {
  x <- parse_dose_finding_outcomes('')

  expect_true(is.list(x))
  expect_equal(length(x), 0)
})

# A regular looking example
test_that('Dose-finding outcomes "1NNN 3NTT" parse correctly', {
  x <- parse_dose_finding_outcomes('1NNN 3NTT')

  expect_true(is.list(x))
  expect_equal(length(x), 2)

  expect_equal(x[[1]]$dose, 1)
  expect_equal(x[[1]]$outcomes, 'NNN')
  expect_equal(x[[2]]$dose, 3)
  expect_equal(x[[2]]$outcomes, 'NTT')
})

# An example with no spaces; I do not recommend this because it is hard to
# read but it should work, nevertheless.
test_that('Dose-finding outcomes "1N2T2N3T2N" parse correctly', {
  x <- parse_dose_finding_outcomes('1N2T2N3T2N')

  expect_true(is.list(x))
  expect_equal(length(x), 5)

  expect_equal(x[[1]]$dose, 1)
  expect_equal(x[[1]]$outcomes, 'N')
  expect_equal(x[[2]]$dose, 2)
  expect_equal(x[[2]]$outcomes, 'T')
  expect_equal(x[[3]]$dose, 2)
  expect_equal(x[[3]]$outcomes, 'N')
  expect_equal(x[[4]]$dose, 3)
  expect_equal(x[[4]]$outcomes, 'T')
  expect_equal(x[[5]]$dose, 2)
  expect_equal(x[[5]]$outcomes, 'N')
})

# n=1 example
test_that('Dose-finding outcomes "5T" parse correctly', {
  x <- parse_dose_finding_outcomes('5T')

  expect_true(is.list(x))
  expect_equal(length(x), 1)

  expect_equal(x[[1]]$dose, 5)
  expect_equal(x[[1]]$outcomes, 'T')
})

# Irregular cohorts
test_that('Dose-finding outcomes "1NTT 2T 2NTNNTN 3N" parse correctly', {
  x <- parse_dose_finding_outcomes('1NTT 2T 2NTNNTN 3N')

  expect_true(is.list(x))
  expect_equal(length(x), 4)

  expect_equal(x[[1]]$dose, 1)
  expect_equal(x[[1]]$outcomes, 'NTT')
  expect_equal(x[[2]]$dose, 2)
  expect_equal(x[[2]]$outcomes, 'T')
  expect_equal(x[[3]]$dose, 2)
  expect_equal(x[[3]]$outcomes, 'NTNNTN')
  expect_equal(x[[4]]$dose, 3)
  expect_equal(x[[4]]$outcomes, 'N')
})

# Silly but valid dose-levels
test_that('Dose-finding outcomes "96NTT 40T 1NTNNTN 174N" parse correctly', {
  x <- parse_dose_finding_outcomes('96NTT 40T 1NTNNTN 174N')

  expect_true(is.list(x))
  expect_equal(length(x), 4)

  expect_equal(x[[1]]$dose, 96)
  expect_equal(x[[1]]$outcomes, 'NTT')
  expect_equal(x[[2]]$dose, 40)
  expect_equal(x[[2]]$outcomes, 'T')
  expect_equal(x[[3]]$dose, 1)
  expect_equal(x[[3]]$outcomes, 'NTNNTN')
  expect_equal(x[[4]]$dose, 174)
  expect_equal(x[[4]]$outcomes, 'N')
})

# Leading white-space
test_that('Dose-finding outcomes " \t1NTT 2T 2NTNNTN 2N" parse correctly', {
  x <- parse_dose_finding_outcomes(' \t1NTT 2T 2NTNNTN 2N')

  expect_true(is.list(x))
  expect_equal(length(x), 4)

  expect_equal(x[[1]]$dose, 1)
  expect_equal(x[[1]]$outcomes, 'NTT')
  expect_equal(x[[2]]$dose, 2)
  expect_equal(x[[2]]$outcomes, 'T')
  expect_equal(x[[3]]$dose, 2)
  expect_equal(x[[3]]$outcomes, 'NTNNTN')
  expect_equal(x[[4]]$dose, 2)
  expect_equal(x[[4]]$outcomes, 'N')
})

# Trailing white-space
test_that('Dose-finding outcomes "1NTT 2T 2NTNNTN 2N \t" parse correctly', {
  x <- parse_dose_finding_outcomes('1NTT 2T 2NTNNTN 2N \t')
  expect_true(is.list(x))
  expect_equal(length(x), 4)

  expect_equal(x[[1]]$dose, 1)
  expect_equal(x[[1]]$outcomes, 'NTT')
  expect_equal(x[[2]]$dose, 2)
  expect_equal(x[[2]]$outcomes, 'T')
  expect_equal(x[[3]]$dose, 2)
  expect_equal(x[[3]]$outcomes, 'NTNNTN')
  expect_equal(x[[4]]$dose, 2)
  expect_equal(x[[4]]$outcomes, 'N')
})


# Test some cases that should not work
# A string that is not welcome here
test_that('Dose-finding outcomes "12NTT Nigel Farage" fail to parse', {
  expect_error(parse_dose_finding_outcomes('12NTT Nigel Farage'))
})

# Decimal dose-levels
test_that('Dose-finding outcomes " 1NTT 2.0T 2NTNNTN 2N" fail to parse', {
  expect_error(parse_dose_finding_outcomes(' 1NTT 2.0T 2NTNNTN 2N'))
})

test_that('Dose-finding outcomes ".1NTT 2T 2NTNNTN 2N" fail to parse', {
  expect_error(parse_dose_finding_outcomes('.1NTT 2T 2NTNNTN 2N'))
})

# Negative dose-levels
test_that('Dose-finding outcomes "12NTT 2T 2NTNNTN -1N" fail to parse', {
  expect_error(parse_dose_finding_outcomes('12NTT 2T 2NTNNTN -1N'))
})

test_that('Dose-finding outcomes "-12NTT 2T 2NTNNTN 1N" fail to parse', {
  expect_error(parse_dose_finding_outcomes('-12NTT 2T 2NTNNTN 1N'))
})

test_that('Dose-finding outcomes "12NTT 2T -2NTNNTN 1N" fail to parse', {
  expect_error(parse_dose_finding_outcomes('12NTT 2T -2NTNNTN 1N'))
})

# Zero dose-level
test_that('Dose-finding outcomes "1T 0NN" fail to parse', {
  expect_error(parse_dose_finding_outcomes('1T 0NN'))
})

test_that('Dose-finding outcomes "0NNTTNNTT" fail to parse', {
  expect_error(parse_dose_finding_outcomes('0NNTTNNTT'))
})

# Nothing but white-space
test_that('Dose-finding outcomes " " fail to parse', {
  expect_error(parse_dose_finding_outcomes(' '))
})

# Looks plausible
test_that('Dose-finding outcomes "1NT TNT" fail to parse', {
  expect_error(df_parse_outcomes('1NT TNT', as.list = FALSE))
})

test_that('Dose-finding outcomes "1NT T3NT" fail to parse', {
  expect_error(df_parse_outcomes('1NT T3NT', as.list = FALSE))
})

test_that('Dose-finding outcomes "1NT 3TNT 4" fail to parse', {
  expect_error(df_parse_outcomes('1NT 3TNT 4', as.list = FALSE))
})


# Test df_parse_outcomes ----

# Test some cases that should work

# The empty string should not fail
test_that('Dose-finding outcomes "" parses correctly', {
  x <- df_parse_outcomes('', as.list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 0)
})

test_that('Dose-finding outcomes "" parses correctly to list', {
  x <- df_parse_outcomes('', as.list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 0)
  expect_equal(x$doses, integer(length = 0))
  expect_equal(x$tox, integer(length = 0))
})

# Regular example
test_that('Dose-finding outcomes "1NNN 3NTT" parse correctly', {
  x <- df_parse_outcomes('1NNN 3NTT', as.list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 6)
  expect_equal(x$doses, c(1, 1, 1, 3, 3, 3))
  expect_equal(x$tox, c(0, 0, 0, 0, 1, 1))
})

test_that('Dose-finding outcomes "1NNN 3NTT" parse correctly to list', {
  x <- df_parse_outcomes('1NNN 3NTT', as.list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 6)
  expect_equal(x$doses, c(1, 1, 1, 3, 3, 3))
  expect_equal(x$tox, c(0, 0, 0, 0, 1, 1))
})

# A regular case with no spaces; I do not recommend this because it is hard to
# read but it should work, nevertheless.
test_that('Dose-finding outcomes "1N2T2N2N2N" parse correctly', {
  x <- df_parse_outcomes('1N2T2N2N2N', as.list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 5)
  expect_equal(x$doses, c(1, 2, 2, 2, 2))
  expect_equal(x$tox, c(0, 1, 0, 0, 0))
})

test_that('Dose-finding outcomes "1N2T2N2N2N" parse correctly to list', {
  x <- df_parse_outcomes('1N2T2N2N2N', as.list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 5)
  expect_equal(x$doses, c(1, 2, 2, 2, 2))
  expect_equal(x$tox, c(0, 1, 0, 0, 0))
})

# n=1 example
test_that('Dose-finding outcomes "5T" parse correctly', {
  x <- df_parse_outcomes('5T', as.list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 1)
  expect_equal(x$doses, c(5))
  expect_equal(x$tox, c(1))
})

test_that('Dose-finding outcomes "5T" parse correctly to  list', {
  x <- df_parse_outcomes('5T', as.list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 1)
  expect_equal(x$doses, c(5))
  expect_equal(x$tox, c(1))
})

# Odd shaped cohorts
test_that('Dose-finding outcomes "1NTT 2T 2NTNNTN 3N" parse correctly', {
  x <- df_parse_outcomes('1NTT 2T 2NTNNTN 3N', as.list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 11)
  expect_equal(x$doses, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

test_that('Dose-finding outcomes "1NTT 2T 2NTNNTN 3N" parse correctly to list', {
  x <- df_parse_outcomes('1NTT 2T 2NTNNTN 3N', as.list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 11)
  expect_equal(x$doses, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

# Silly but valid dose-levels
test_that('Dose-finding outcomes "96NTT 40T 1NTNNTN 174N" parse correctly', {
  x <- df_parse_outcomes('96NTT 40T 1NTNNTN 174N', as.list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 11)
  expect_equal(x$doses, c(96, 96, 96, 40, 1, 1, 1, 1, 1, 1, 174))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

test_that('Dose-finding outcomes "96NTT 40T 1NTNNTN 174N" parse correctly to list', {
  x <- df_parse_outcomes('96NTT 40T 1NTNNTN 174N', as.list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 11)
  expect_equal(x$doses, c(96, 96, 96, 40, 1, 1, 1, 1, 1, 1, 174))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

# Leading white-space
test_that('Dose-finding outcomes " 1NTT 2T 2NTNNTN 2N" parse correctly', {
  x <- df_parse_outcomes(' 1NTT 2T 2NTNNTN 2N', as.list = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(nrow(x), 11)
  expect_equal(x$doses, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

test_that('Dose-finding outcomes " 1NTT 2T 2NTNNTN 2N" parse correctly to list', {
  x <- df_parse_outcomes(' 1NTT 2T 2NTNNTN 2N', as.list = TRUE)
  expect_true(is.list(x))
  expect_equal(x$num_patients, 11)
  expect_equal(x$doses, c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2))
  expect_equal(x$tox, c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0))
})

# Test some cases that should not work
# A string that is not welcome here
test_that('Dose-finding outcomes "12NTT Nigel Farage" fail to parse', {
  expect_error(df_parse_outcomes('12NTT Nigel Farage', as.list = FALSE))
})

test_that('Dose-finding outcomes "12NTT Nigel Farage" fail to parse to list', {
  expect_error(df_parse_outcomes('12NTT Nigel Farage', as.list = TRUE))
})

# Decimal dose-levels
test_that('Dose-finding outcomes " 1NTT 2.0T 2NTNNTN 2N" fail to parse', {
  expect_error(df_parse_outcomes(' 1NTT 2.0T 2NTNNTN 2N', as.list = FALSE))
})

test_that('Dose-finding outcomes " 1NTT 2.0T 2NTNNTN 2N" fail to parse to list', {
  expect_error(df_parse_outcomes(' 1NTT 2.0T 2NTNNTN 2N', as.list = TRUE))
})

test_that('Dose-finding outcomes ".1NTT 2T 2NTNNTN 2N" fail to parse', {
  expect_error(df_parse_outcomes('.1NTT 2T 2NTNNTN 2N', as.list = FALSE))
})

test_that('Dose-finding outcomes ".1NTT 2T 2NTNNTN 2N" fail to parse to list', {
  expect_error(df_parse_outcomes('.1NTT 2T 2NTNNTN 2N', as.list = TRUE))
})

# Negative dose-levels
test_that('Dose-finding outcomes "12NTT 2T 2NTNNTN -1N" fail to parse', {
  expect_error(df_parse_outcomes('12NTT 2T 2NTNNTN -1N', as.list = FALSE))
})

test_that('Dose-finding outcomes "12ETT 2T 2NTNNTN -1N" fail to parse to list', {
  expect_error(df_parse_outcomes('12ETT 2T 2NTNNTN -1N', as.list = TRUE))
})

test_that('Dose-finding outcomes "-12NTT 2T 2NTNNTN 1N" fail to parse', {
  expect_error(df_parse_outcomes('-12NTT 2T 2NTNNTN 1N', as.list = FALSE))
})

test_that('Dose-finding outcomes "-12NTT 2T 2NTNNTN 1N" fail to parse to list', {
  expect_error(df_parse_outcomes('-12NTT 2T 2NTNNTN 1N', as.list = TRUE))
})

test_that('Dose-finding outcomes "12NTT 2T -2NTNNTN 1N" fail to parse', {
  expect_error(df_parse_outcomes('12NTT 2T -2NTNNTN 1N', as.list = FALSE))
})

test_that('Dose-finding outcomes "12NTT 2T -2NTNNTN 1N" fail to parse to list', {
  expect_error(df_parse_outcomes('12NTT 2T -2NTNNTN 1N', as.list = TRUE))
})

# Zero dose-level
test_that('Dose-finding outcomes "1T 0NN" fail to parse', {
  expect_error(df_parse_outcomes('1T 0NN', as.list = FALSE))
})

test_that('Dose-finding outcomes "1T 0NN" fail to parse to list', {
  expect_error(df_parse_outcomes('1T 0NN', as.list = TRUE))
})

test_that('Dose-finding outcomes "0NNTTNNTT" fail to parse', {
  expect_error(df_parse_outcomes('0NNTTNNTT', as.list = FALSE))
})

test_that('Dose-finding outcomes "0NNTTNNTT" fail to parse to list', {
  expect_error(df_parse_outcomes('0NNTTNNTT', as.list = TRUE))
})

# Nothing but white-space
test_that('Dose-finding outcomes " " fail to parse', {
  expect_error(df_parse_outcomes(' ', as.list = FALSE))
})

test_that('Dose-finding outcomes " " fail to parse to list', {
  expect_error(df_parse_outcomes(' ', as.list = TRUE))
})

# Nothing
# test_that('Dose-finding outcomes "" fail to parse', {
#   expect_error(df_parse_outcomes('', as.list = FALSE))
# })
#
# test_that('Dose-finding outcomes "" fail to parse to list', {
#   expect_error(df_parse_outcomes('', as.list = TRUE))
# })

# Looks plausible
test_that('Dose-finding outcomes "1NT TNT" fail to parse', {
  expect_error(df_parse_outcomes('1NT TNT', as.list = FALSE))
})

test_that('Dose-finding outcomes "1NT TNT" fail to parse to list', {
  expect_error(df_parse_outcomes('1NT TNT', as.list = TRUE))
})

test_that('Dose-finding outcomes "1NT T3NT" fail to parse', {
  expect_error(df_parse_outcomes('1NT T3NT', as.list = FALSE))
})

test_that('Dose-finding outcomes "1NT T3NT" fail to parse to list', {
  expect_error(df_parse_outcomes('1NT T3NT', as.list = TRUE))
})

test_that('Dose-finding outcomes "1NT 3TNT 4" fail to parse', {
  expect_error(df_parse_outcomes('1NT 3TNT 4', as.list = FALSE))
})

test_that('Dose-finding outcomes "1NT 3TNT 4" fail to parse to list', {
  expect_error(df_parse_outcomes('1NT 3TNT 4', as.list = TRUE))
})
