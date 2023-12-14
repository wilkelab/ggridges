context("parse")

test_that("parse is expression", {
  expect_is(parse_exp("a"), "expression")
})

# test_that("eval parse is character",{
# expect_is(eval(parse_exp("a")), "chr")})
