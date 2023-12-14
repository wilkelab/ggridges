library(signifinder)

test_that(".range01 works with NA", {
    rvector <- c(rnorm(10, 5), NA)
    expect_true(is.double(.range01(rvector)))
})
