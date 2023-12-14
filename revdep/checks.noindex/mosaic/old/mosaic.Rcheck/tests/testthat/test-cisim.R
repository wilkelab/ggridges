# testthat::test_that("Clsim works", {
#   wrapped_expect_doppelganger("CIsim1", CIsim(n=10, samples=100, verbose = FALSE))
#   wrapped_expect_doppelganger("CIsim2", CIsim(n=10, samples=100, rdist=rexp, estimand=1, verbose = FALSE))
#   wrapped_expect_doppelganger("CIsim3", CIsim(n=30, samples=100, rdist=rbinom, args=list(size=1, prob=.7),
#                                              estimand = .7, method = binom.test, method.args=list(ci = "Plus4"), verbose = FALSE))
# })
