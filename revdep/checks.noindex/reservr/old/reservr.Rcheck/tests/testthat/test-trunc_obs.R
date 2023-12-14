test_that("test trunc_obs", {
  obs <- trunc_obs(1:3)
  expect_s3_class(obs, "trunc_obs")

  expect_identical(as_trunc_obs(1:3), obs)
  expect_identical(as_trunc_obs(data.frame(x = 1:3)), obs)
  expect_identical(as_trunc_obs(
    data.frame(x = 1:3, xmin = 1:3, xmax = 1:3, tmin = -Inf, tmax = Inf)
  ), obs)
  expect_identical(as_trunc_obs(
    data.frame(xmin = 1:3, xmax = 1:3, tmin = -Inf, tmax = Inf)
  ), obs)

  obs2 <- trunc_obs(
    1:3,
    tmin = 0,
    tmax = c(4, 4, 3)
  )
  expect_identical(repdel_obs(
    data.frame(
      a = c(0, 0, 1, 1),
      d = 1:4
    ),
    accident = a,
    delay = d,
    time = 4,
    .truncate = TRUE
  ), obs2)

  expect_error(repdel_obs(
    data.frame(
      a = c(0, 0, 1, 1),
      d = 1:4
    ),
    accident = a,
    delay = d,
    time = 4,
    .truncate = FALSE
  ), fixed = "must be TRUE")
})
