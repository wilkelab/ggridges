test_that("test distribution_class", {
  expect_identical(dist_dirac()$get_components(), list())
  expect_error(dist_dirac()$require_capability(c("foo", "bar")),
               fixed = "foo and bar")
  expect_identical(dist_erlangmix(list(1, 2, 3))$get_param_bounds(),
                   list(
                     shapes = list(),
                     scale = I_POSITIVE_REALS,
                     probs = rep_len(list(I_UNIT_INTERVAL), 3L)
                   ))

  export_env <- new.env()
  dist_exponential()$export_functions(
    name = "myexp",
    envir = export_env,
    with_params = list(rate = 3.0)
  )
  expect_setequal(ls(export_env), paste0(c("d", "p", "q", "r"), "myexp"))

  # Force make_params to broadcast unnamed parameters
  dist_erlangmix(list(1, 2, 4), scale = 1)$
    sample(3L, with_params = list(probs = list(1)))
})
