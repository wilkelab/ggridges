phy = ape::rtree(n = 300)

test_that("vcv should have the same results with ape::vcv", {
    expect_equal(ape::vcv(phy, corr = FALSE), vcv2(phy, corr = FALSE))
    expect_equal(ape::vcv(phy, corr = TRUE), vcv2(phy, corr = TRUE))
})

# microbenchmark::microbenchmark(ape::vcv(phy, corr = FALSE), vcv2(phy, corr = FALSE),
# times = 10) microbenchmark::microbenchmark(ape::vcv(phy, corr = TRUE), vcv2(phy,
# corr = TRUE), times = 10)
