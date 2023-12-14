
test_that('efftox_solve_p returns expected result', {
  p <- efftox_solve_p(eff0 = 0.5, tox1 = 0.65, eff_star = 0.7, tox_star = 0.25)
  expect_equal(round(p, 3), 0.977)
})

test_that('efftox_solve_p throws error on zero eff0', {
  expect_error(efftox_solve_p(eff0 = 0, tox1 = 0.65,
                              eff_star = 0.7, tox_star = 0.25))
})

test_that('efftox_solve_p throws error on zero tox1', {
  expect_error(efftox_solve_p(eff0 = 0.5, tox1 = 0,
                              eff_star = 0.7, tox_star = 0.25))
})

test_that('efftox_solve_p throws error on zero eff_star', {
  expect_error(efftox_solve_p(eff0 = 0.5, tox1 = 0.65,
                              eff_star = 0, tox_star = 0.25))
})

test_that('efftox_solve_p throws error on zero tox_star', {
  expect_error(efftox_solve_p(eff0 = 0.5, tox1 = 0.65,
                              eff_star = 0.7, tox_star = 0))
})

test_that('efftox_solve_p throws error on unit eff0', {
  expect_error(efftox_solve_p(eff0 = 1, tox1 = 0.65,
                              eff_star = 0.7, tox_star = 0.25))
})

test_that('efftox_solve_p throws error on unit tox1', {
  expect_error(efftox_solve_p(eff0 = 0.5, tox1 = 1,
                              eff_star = 0.7, tox_star = 0.25))
})

test_that('efftox_solve_p throws error on unit eff_star', {
  expect_error(efftox_solve_p(eff0 = 0.5, tox1 = 0.65,
                              eff_star = 1, tox_star = 0.25))
})

test_that('efftox_solve_p throws error on unit tox_star', {
  expect_error(efftox_solve_p(eff0 = 0.5, tox1 = 0.65,
                              eff_star = 0.7, tox_star = 1))
})

# TODO
# efftox_get_tox()
# efftox_superiority()
# efftox_analysis_to_df()
# efftox_utility()
