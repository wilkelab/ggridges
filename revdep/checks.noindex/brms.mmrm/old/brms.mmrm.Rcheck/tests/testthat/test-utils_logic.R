test_that("%||%", {
  expect_equal("x" %||% "y", "x")
  expect_equal(character(0) %||% "y", "y")
  expect_equal(NA_character_ %||% "y", NA_character_)
  expect_equal(NULL %||% "y", "y")
})

test_that("%|||%", {
  expect_equal("x" %|||% "y", "x")
  expect_equal(character(0) %|||% "y", character(0))
  expect_equal(NA_character_ %|||% "y", NA_character_)
  expect_equal(NULL %|||% "y", "y")
})

test_that("if_any()", {
  expect_equal(if_any(TRUE, "x", "y"), "x")
  expect_equal(if_any(FALSE, "x", "y"), "y")
})
