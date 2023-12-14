context("utilities")

test_that("prompts work", {
  yall <- "y'all"

  expect_error(glue_stop("howdy {yall}"), "howdy y'all")
  expect_warning(glue_warn("howdy {yall}"), "howdy y'all")
  expect_message(glue_message("howdy {yall}"), "howdy y'all")
})

test_that("generic checking utilities work", {
  expect_error(check_inherits("howdy", "numeric"), "`howdy` needs to inherit")
  expect_true(check_inherits("howdy", "character"))

  expect_error(
    check_column("focal_ID", "factor", focal_vs_comp_ex),
    '"focal_ID" column should inherit from factor, but its class is numeric.'
  )
})

test_that("object checking utilities work", {
  expect_true(
    check_focal_vs_comp(focal_vs_comp_ex) %>%
      unlist() %>%
      all()
  )

  expect_true(check_comp_bayes_lm(comp_bayes_lm_ex))

  comp_bayes_lm_ex_ <- comp_bayes_lm_ex
  comp_bayes_lm_ex_$post_params$b_star <- as.character(comp_bayes_lm_ex_$post_params$b_star)

  expect_error(
    check_comp_bayes_lm(comp_bayes_lm_ex_),
    "b_star element of `posterior` needs to inherit from class numeric, but its"
  )
})
