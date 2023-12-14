tmp <- tempdir()
if (dir.exists(file.path(tmp, "__pycache__"))) {
  unlink(file.path(tmp, "__pycache__"), recursive = TRUE, force = TRUE)
}
tmp_py_files <- list.files(tmp, pattern = "^(tmp|__autograph_generated_file).*\\.py$", full.names = TRUE)
file.remove(tmp_py_files)

test_that("test helpers", {
  expect_equal(NULL %||% 1, 1)
  expect_silent(muffle_nans_produced(warning("NaNs produced")))
  expect_silent(muffle_warning(warning("a"), "a"))
  expect_warning(muffle_warning(warning("a"), "b"), fixed = "a")

  expect_equal(
    enumerate_strings("a"),
    "a"
  )

  expect_equal(
    enumerate_strings(letters[1:2]),
    "a and b"
  )

  expect_equal(
    enumerate_strings(letters[1:3]),
    "a, b and c"
  )

  expect_equal(
    enumerate_strings(letters[1:3], quote = "'"),
    "'a', 'b' and 'c'"
  )
})
