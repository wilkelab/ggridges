
test_that("is_uri() works", {
    expect_true(is_uri('37i9dQZF1DX5GJ0BQvLMjM'))
    expect_false(is_uri("a"))
})


