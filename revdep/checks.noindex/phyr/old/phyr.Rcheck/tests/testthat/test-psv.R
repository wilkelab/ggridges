context("test phylogenetic species varaition, psv")

test_that("psv should return the same results as picante::psv", {
    skip_if_not_installed("picante")
    expect_equal(psv(comm_a, phylotree), picante::psv(comm_a, phylotree))
})

test_that("psv should return the same results as picante::psr", {
    skip_if_not_installed("picante")
    expect_equal(psr(comm_a, phylotree), picante::psr(comm_a, phylotree))
})

test_that("psv should return the same results as picante::pse", {
    skip_if_not_installed("picante")
    expect_equal(pse(comm_a, phylotree), picante::pse(comm_a, phylotree))
})

test_that("psv should return the same results as picante::psc", {
    skip_if_not_installed("picante")
    x = psc(comm_a, phylotree)
    # x$PSCs = 1 - x$PSCs  # check with Matt, the CRAN and github version are different
    expect_equal(x, picante::psc(comm_a, phylotree))
})

test_that("psv should return the same results as picante::psd", {
    skip_if_not_installed("picante")
    x = psd(comm_a, phylotree)
    # x$PSCs = 1 - x$PSCs  # check with Matt, the CRAN and github version are different
    expect_equivalent(x, picante::psd(comm_a, phylotree))
})

test_that("psd should run when comm has only one row", {
    skip_if_not_installed("picante")
    x = psd(comm_a[2, ], phylotree)
    # x$PSCs = 1 - x$PSCs
    expect_equivalent(x, picante::psd(comm_a[2, ], phylotree))
})

nspp = 100
nsite = 50
tree_sim = ape::rtree(n = nspp)
comm_sim = matrix(rbinom(nspp * nsite, size = 1, prob = 0.6), nrow = nsite, ncol = nspp)
row.names(comm_sim) = paste0("site_", 1:nsite)
colnames(comm_sim) = paste0("t", 1:nspp)
comm_sim = comm_sim[, tree_sim$tip.label]

test_that("psv with cpp should have same results with r", {
    skip_if_not_installed("picante")
    a = psv(comm_sim, tree_sim, cpp = FALSE)
    b = psv(comm_sim, tree_sim, cpp = TRUE)
    c = picante::psv(comm_sim, tree_sim)
    expect_equal(a, b)
    expect_equal(a, c)
})

comm_sim = matrix(rpois(nspp * nsite, 3), nrow = nsite, ncol = nspp)
row.names(comm_sim) = paste0("site_", 1:nsite)
colnames(comm_sim) = paste0("t", 1:nspp)
comm_sim = comm_sim[, tree_sim$tip.label]

test_that("pse with cpp should have same results with r", {
    skip_if_not_installed("picante")
    ae = pse(comm_sim, tree_sim, cpp = FALSE)
    be = pse(comm_sim, tree_sim, cpp = TRUE)
    ce = picante::pse(comm_sim, tree_sim)
    expect_equal(ae, be)
    expect_equal(ae, ce)
})

