context("test phylogenetic community dissimilarity, pcd")

test_that("testing pcd_pred, which calculate expectation of conditional PSV", {
    x1 = pcd_pred(comm_a, comm_b, tree = phylotree, reps = 100)
    x2 = pcd_pred(comm_a, comm_b, tree = phylotree, reps = 100, cpp = FALSE)
    expect_type(x1, "list")
    expect_length(x1, 4)
    expect_type(x2, "list")
    expect_length(x2, 4)
    expect_equivalent(x1$nsp_pool, x2$nsp_pool)
    expect_equivalent(x1$psv_pool, x2$psv_pool)
    expect_equivalent(x1$nsr, x2$nsr)
    expect_equal(length(x1$psv_bar), length(x2$psv_bar))
})

test_that("testing pcd, which calculate pairwise site dissimilarity", {
    x1 = pcd_pred(comm_a, comm_b, tree = phylotree, reps = 100)
    x3 = pcd(comm = comm_a, tree = phylotree, expectation = x1)
    x4 = pcd(comm = comm_a, tree = phylotree, expectation = x1, cpp = FALSE)
    expect_type(x3, "list")
    expect_type(x4, "list")
    expect_equivalent(x3, x4)
})

test_that("testing pcd, without provide expectation", {
    x5 = pcd(comm = comm_a, tree = phylotree)
    x6 = pcd(comm = comm_a, tree = phylotree, cpp = FALSE)
    expect_type(x5, "list")
    expect_type(x6, "list")
})

test_that("testing pcd, expectation based on one community", {
    skip_if_not_installed("picante")
    x1 = pcd_pred(comm_a, tree = phylotree, reps = 100)
    x7 = pcd(comm = comm_a, tree = phylotree, expectation = x1)
    x8 = pcd(comm = comm_a, tree = phylotree, expectation = x1, cpp = FALSE)
    x9 = picante::pcd(comm_a, phylotree, reps = 1000)
    expect_type(x7, "list")
    expect_type(x8, "list")
    expect_equivalent(x7, x8)
    expect_equal(x7$PCDc, x9$PCDc)  # non-phy component should be all the same
    # the phy part may not, because of the randomness
})
