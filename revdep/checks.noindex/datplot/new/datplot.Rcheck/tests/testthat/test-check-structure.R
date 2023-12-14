test <- matrix(data = c("ID1", "Type A", 100, 200,
                        "ID2", "Type A", -100, 100),
               byrow = TRUE, ncol = 4)

test_that("check.structure fails if not data.frame", {
  expect_error(check.structure(test), "data.frame")
})

test <- as.data.frame(test)
test$V3 <- as.numeric(test$V3)
test$V4 <- as.numeric(test$V4)

test_that("returns true if data.frame", {
  expect_true(check.structure(test))
})

test_that("check.structure issues message when verbose and columns
          do not conform to expected classes", {
    expect_message(check.structure(test, verbose = TRUE), regexp = "recommended")
    expect_failure(expect_message(check.structure(test, verbose = FALSE)))
})


test_that("check.structure issues no message when verbose and columns
          conform to expected classes", {
    test$V2 <- as.factor(test$V2)
    expect_true(check.structure(test, verbose = TRUE))
    expect_failure(expect_message(check.structure(test, verbose = TRUE)))
})

test_that("check.structure fails if column 3 or 4 are not numeric", {
  test$V4 <- as.character(test$V4)
  expect_error(check.structure(test), regexp = "numeric")
  test$V4 <- as.numeric(test$V4)
  test$V3 <- as.character(test$V3)
  expect_error(check.structure(test), regexp = "numeric")
  test$V4 <- as.character(test$V4)
  expect_error(check.structure(test), regexp = "numeric")
})
