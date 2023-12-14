test_that("assert()", {
  expect_silent(assert(TRUE))
  expect_error(assert(FALSE), class = "brm_error")
  expect_silent(assert(c(2, 3), . > 1, . > 0))
  expect_error(assert(2, . < 1), class = "brm_error")
})

test_that("assert_chr_vec()", {
  expect_silent(assert_chr_vec(letters))
  expect_silent(assert_chr_vec(character(0L)))
  expect_error(assert_chr_vec(""), class = "brm_error")
  expect_error(assert_chr_vec(1), class = "brm_error")
  expect_error(assert_chr_vec(c(2L, 3L)), class = "brm_error")
})

test_that("assert_chr()", {
  expect_silent(assert_chr("abc"))
  expect_error(assert_chr(letters), class = "brm_error")
  expect_error(assert_chr(character(0L)), class = "brm_error")
  expect_error(assert_chr(""), class = "brm_error")
  expect_error(assert_chr(1), class = "brm_error")
  expect_error(assert_chr(c(2L, 3L)), class = "brm_error")
})

test_that("assert_col()", {
  data <- data.frame(x = 1)
  expect_silent(assert_col("x", data))
  expect_error(assert_col("y", data), class = "brm_error")
})

test_that("assert_lgl()", {
  expect_silent(assert_lgl(TRUE))
  expect_silent(assert_lgl(FALSE))
  expect_error(assert_lgl(c(TRUE, FALSE)), class = "brm_error")
  expect_error(assert_lgl(logical(0L)), class = "brm_error")
  expect_error(assert_lgl(123.321), class = "brm_error")
  expect_error(assert_lgl(c(1L, 2L)), class = "brm_error")
})

test_that("assert_machine_names()", {
  expect_silent(assert_machine_names("x"))
  expect_error(assert_machine_names("_x"), class = "brm_error")
})

test_that("assert_num()", {
  expect_silent(assert_num(1.1))
  expect_silent(assert_num(-1L))
  expect_error(assert_num(c(1.1, 2.2)), class = "brm_error")
  expect_error(assert_num(numeric(0L)), class = "brm_error")
  expect_error(assert_num(NA_real_), class = "brm_error")
  expect_error(assert_num("1"), class = "brm_error")
})

test_that("assert_pos()", {
  expect_silent(assert_pos(1.1))
  expect_error(assert_pos(-1L), class = "brm_error")
  expect_error(assert_pos(c(1.1, 2.2)), class = "brm_error")
  expect_error(assert_pos(numeric(0L)), class = "brm_error")
  expect_error(assert_pos(NA_real_), class = "brm_error")
  expect_error(assert_pos("1"), class = "brm_error")
})

test_that("brm_error()", {
  expect_error(brm_error("message"), class = "brm_error")
})

test_that("brm_warn()", {
  expect_warning(brm_warn("message"), class = "brm_warn")
})
