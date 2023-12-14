library(signifinder)

test_that(".getMatrix returns a matrix", {
    rmatrix <- matrix(rpois(30, 100), ncol = 6)
    expect_true(is.matrix(.getMatrix(rmatrix)))
    expect_true(is.matrix(.getMatrix(data.frame(rmatrix))))
    expect_equal(typeof(rmatrix), typeof(.getMatrix(rmatrix)))
})

test_that(".getMatrix works with one column", {
    rmatrix <- matrix(rpois(20, 100), ncol = 1)
    expect_equal(ncol(.getMatrix(rmatrix)), 1)
    expect_equal(ncol(.getMatrix(data.frame(rmatrix))), 1)
})

test_that(".getMatrix works with one row", {
    rmatrix <- matrix(rpois(20, 100), nrow = 1)
    expect_equal(nrow(.getMatrix(rmatrix)), 1)
    expect_equal(nrow(.getMatrix(data.frame(rmatrix))), 1)
})
