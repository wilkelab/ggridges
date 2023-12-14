
test_that("check_ROPE_argument works", {
  
  expect_error(
    check_ROPE_argument(NULL), 
    NA
  )
  
  expect_error(
    check_ROPE_argument(c(0, 1)), 
    NA
  )
  
  expect_error(
    check_ROPE_argument(0), 
    "Invalid"
  )
  
  expect_error(
    check_ROPE_argument(c("a", "b")), 
    "Invalid"
  )
  
  # decreasing arguments
  expect_error(
    check_ROPE_argument(c(1, 0)), 
    "Invalid"
  )
  
})