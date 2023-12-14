library(signifinder)

test_that(".returnAsInput returns a S4 object", {
    rmatrix <- matrix(rpois(30, 100), ncol = 6)
    rresult <- rnorm(6)
    expect_type(.returnAsInput(rmatrix, rresult, "prova", rmatrix), "S4")
    expect_equal(
        .returnAsInput(rmatrix, rresult, "prova", rmatrix),
        .returnAsInput(data.frame(rmatrix), rresult, "prova", rmatrix)
    )
})
