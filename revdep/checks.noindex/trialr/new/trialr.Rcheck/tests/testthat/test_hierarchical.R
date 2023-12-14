
# Test functions in hierarchical.R

# Test the hierarchical response model of Thall, Wathen, Bekele, Champlin,
# Baker & Benjamin (2003).
# The following scenarios are detailed in Table III, p.770
test_that('Thall et al. Scenario 1 decisions are replicated', {
  mod  <- stan_hierarchical_response_thall(
    group_responses = rep(0:1, each = 5),
    group_sizes = rep(8, 10),
    mu_mean = -1.3863,
    mu_sd = sqrt(1 / 0.1),
    tau_alpha = 2,
    tau_beta = 20,
    refresh = 0)
  continue <- colMeans(as.data.frame(mod, 'prob_response') > 0.3) > 0.005
  continue <- unname(continue)
  expect_false(continue[1])
  expect_true(continue[6])
})

# test_that('Thall et al. Scenario 2 decisions are replicated', {
#   mod  <- stan_hierarchical_response_thall(
#     group_responses = rep(0:2, times = c(3, 2, 5)),
#     group_sizes = rep(8, 10),
#     mu_mean = -1.3863,
#     mu_sd = sqrt(1 / 0.1),
#     tau_alpha = 2,
#     tau_beta = 20)
#   continue <- colMeans(as.data.frame(mod, 'prob_response') > 0.3) > 0.005
#   continue <- unname(continue)
#   expect_true(continue[1])
#   expect_true(continue[4])
#   expect_true(continue[6])
# })

# test_that('Thall et al. Scenario 3 decisions are replicated', {
#   mod  <- stan_hierarchical_response_thall(
#     group_responses = rep(c(1, 5, 7), times = c(2, 3, 5)),
#     group_sizes = rep(c(17, 17, 23), times = c(2, 3, 5)),
#     mu_mean = -1.3863,
#     mu_sd = sqrt(1 / 0.1),
#     tau_alpha = 2,
#     tau_beta = 20)
#   continue <- colMeans(as.data.frame(mod, 'prob_response') > 0.3) > 0.005
#   continue <- unname(continue)
#   expect_true(continue[1])
#   expect_true(continue[3])
#   expect_true(continue[6])
# })
#
# test_that('Thall et al. Scenario 4 decisions are replicated', {
#   mod  <- stan_hierarchical_response_thall(
#     group_responses = rep(c(0, 1, 2), times = c(3, 2, 5)),
#     group_sizes = rep(c(8, 8, 23), times = c(3, 2, 5)),
#     mu_mean = -1.3863,
#     mu_sd = sqrt(1 / 0.1),
#     tau_alpha = 2,
#     tau_beta = 20)
#   continue <- colMeans(as.data.frame(mod, 'prob_response') > 0.3) > 0.005
#   continue <- unname(continue)
#   expect_false(continue[1])
#   # expect_false(continue[4])
#   expect_false(continue[6])
# })
#
# test_that('Thall et al. Scenario 5 decisions are replicated', {
#   mod  <- stan_hierarchical_response_thall(
#     group_responses = rep(c(1, 2, 3), times = c(3, 2, 5)),
#     group_sizes = rep(c(8, 22, 30), times = c(3, 2, 5)),
#     mu_mean = -1.3863,
#     mu_sd = sqrt(1 / 0.1),
#     tau_alpha = 2,
#     tau_beta = 20)
#   continue <- colMeans(as.data.frame(mod, 'prob_response') > 0.3) > 0.005
#   continue <- unname(continue)
#   expect_true(continue[1])
#   expect_false(continue[4])
#   expect_false(continue[6])
# })
