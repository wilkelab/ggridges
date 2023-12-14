
test_that('efftox_dtps_to_dataframe fails when cohort_sizes not vector of +ve integers', {

  dat <- efftox_parameters_demo()
  dat$doses = array(c(1,1,1))
  dat$eff = array(c(0,0,0))
  dat$tox = array(c(1,1,1))
  dat$num_patients = 3

  expect_error(
    efftox_dtps_to_dataframe(dat = dat, cohort_sizes = c(3, 0), next_dose = 1)
  )

  expect_error(
    efftox_dtps_to_dataframe(dat = dat, cohort_sizes = c(3, -1), next_dose = 1)
  )

  expect_error(
    efftox_dtps_to_dataframe(dat = dat, cohort_sizes = c(3, 2.3), next_dose = 1)
  )

  expect_error(
    efftox_dtps_to_dataframe(dat = dat, cohort_sizes = c(3, NA), next_dose = 1)
  )
})
