context('errors')

test_that('checkData throws error when data contains infinite values', {
    df <- data.frame(
        x = c(0, 3, 5, 2, Inf)
    )
    expect_error(checkData(df, checkTypes$variable_contains_inf))
})

test_that('checkData throws error when data contains NaN values', {
    df <- data.frame(
        x = c(0, 3, 5, 2, NaN)
    )
    expect_error(checkData(df, checkTypes$variable_contains_missing))
})

test_that('checkData throws error when data contains missing values', {
    df <- data.frame(
        x = c(0, 3, 5, 2, NA)
    )
    expect_error(checkData(df, checkTypes$variable_contains_missing))
})

test_that('checkData throws error when data contains only missing values', {
    df <- data.frame(
        x = c(NA, NA, NA, NA, NA)
    )
    expect_error(checkData(df, checkTypes$variable_contains_only_missing))
})

test_that('checkData throws error when data contains only one unique numeric value', {
    df <- data.frame(
        x = c(1, 1, 1, 1, 1)
    )
    expect_error(checkData(df, checkTypes$variable_contains_one_unique_value))
})

test_that('checkData throws error when data contains only one unique factor level', {
    df <- data.frame(
        x = c('A', 'A', 'A', 'A', 'A'), stringsAsFactors = TRUE
    )
    expect_error(checkData(df, checkTypes$variable_contains_one_unique_value))
})
