test_that("check_lengths works on good inputs", {
    a <- 1:10
    b <- 1:10
    c <- 42
    d <- 3
    expect_invisible(check_lengths(a, b, c))
    expect_invisible(check_lengths(a, b))
    expect_invisible(check_lengths(a, c))
    expect_invisible(check_lengths(c, d))
})

test_that("check_lengths throws on bad inputs", {
    a <- 1:10
    b <- 1:3
    c <- 1:3
    d <- 42
    expect_error(check_lengths(a, b),
                 regexp = "^`b` must have 1 or 10 elements")
    expect_error(check_lengths(a, b, .len = 3),
                 regexp = "^`a` must have 1 or 3 elements")
    expect_error(check_lengths(a, b, c),
                 regexp = "^Some arguments have problematic lengths")
    expect_error(check_lengths(a, b, c, d),
                 regexp = "^Some arguments have problematic lengths")
    expect_error(check_lengths(a, b, c, d, .len = 3),
                 regexp = "^`a` must have 1 or 3 elements")
})

test_that("check_lengths shows message", {
    a <- 1:10
    b <- 1:3
    c <- 42
    expect_error(check_lengths(a, b, c, .msg = "ten"), regexp = "ten")
    expect_error(
        check_lengths(
            a, b, c, .len = 3,
            .msg = "length of smallest element longer than 1"
        ),
        regexp = paste0("^`a` must have 1 or 3 elements ",
                        "\\(length of smallest element longer than 1\\),",
                        " but has 10\\.$")
    )
})
