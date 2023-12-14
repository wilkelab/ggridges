context("setup")

# Test 1:

test_that("setup returns an object of class specr.setup", {
  expect_is(specr::setup(data = example_data,
                  x = c("x1", "x2"),
                  y = c("y1", "y2"),
                  model = "lm",
                  controls = c("c1", "c2", "c3")), "specr.setup")
})

# Test 2:

test_that("setup requires a data set", {
  expect_error(specr::setup(x = c("x1", "x2"),
                     y = c("y1", "y2"),
                     model = "lm",
                     controls = c("c1", "c2", "c3")),
               "You must provide the data set that should be used in the analyses.")
})

# Test 3:

test_that("setup requires an x variable", {
  expect_error(specr::setup(data = example_data,
                     y = c("y1", "y2"),
                     model = "lm",
                     controls = c("c1", "c2", "c3"),
                     simplify = TRUE),
               "You must specify at least one independent variable `x`.")
})

# Test 4:

test_that("setup requires a y variable", {
  expect_error(specr::setup(data = example_data,
                     x = c("x1", "x2"),
                     model = "lm",
                     controls = c("c1", "c2", "c3"),
                     simplify = TRUE),
               "You must specify at least one dependent variable `y`.")
})

# Test 5:

test_that("setup requires a model function", {
  expect_error(specr::setup(data = example_data,
                     x = c("x1", "x2"),
                     y = c("y1", "y2"),
                     controls = c("c1", "c2", "c3")))
})

# Test 6a:

test_that("setup does not accept duplicates in x", {
  expect_error(specr::setup(data = example_data,
                     x = c("x1", "x2", "x2"),
                     y = c("y1", "y2"),
                     model = "lm",
                     controls = c("c1", "c2", "c3")),
               "Duplicate values in x, y, model, and controls are not allowed.")
})

# Test 6b:

test_that("setup does not accept duplicates in y", {
  expect_error(specr::setup(data = example_data,
                     x = c("x1", "x2"),
                     y = c("y1", "y2", "y1"),
                     model = "lm",
                     controls = c("c1", "c2", "c3")),
               "Duplicate values in x, y, model, and controls are not allowed.")
})

# Test6c:

test_that("setup does not accept duplicates in models", {
  expect_error(specr::setup(data = example_data,
                     x = c("x1", "x2"),
                     y = c("y1", "y2"),
                     model = c("lm", "lm", "glm"),
                     controls = c("c1", "c2", "c3")),
               "Duplicate values in x, y, model, and controls are not allowed.")
})

# Test6c:

test_that("setup does not accept duplicates in controls", {
  expect_error(specr::setup(data = example_data,
                     x = c("x1", "x2"),
                     y = c("y1", "y2"),
                     model = c("lm", "lm", "glm"),
                     controls = c("c1", "c2", "c3", "c2", "c3")),
               "Duplicate values in x, y, model, and controls are not allowed.")
})

# Test 7:

test_that("setup creates all combinations", {
  specs <- specr::setup(data = example_data,
                 x = c("x1", "x2"),
                 y = c("y1", "y2"),
                 model = "lm",
                 subsets = list(group1 = unique(example_data$group1),
                                group2 = unique(example_data$group2)))
  expect_equal(nrow(specs$specs),
               (nrow(dplyr::distinct(example_data, group1)) + 1) *
               (nrow(dplyr::distinct(example_data, group2)) + 1) *
               length(c("x1", "x2")) *
               length(c("y1", "y2")))
})

# Test 8:

test_that("setup creates a formula for each specification", {
  specs <- specr::setup(data = example_data,
                 x = c("x1", "x2"),
                 y = c("y1", "y2"),
                 model = "lm",
                 controls = c("c1", "c2", "c3"))
  expect_true(all(!is.na(specs$specs$formula)))
})

# Test 9:

test_that("setup creates a model function for each specification", {
  specs <- specr::setup(data = example_data,
                 x = c("x1", "x2"),
                 y = c("y1", "y2"),
                 model = "lm",
                 controls = c("c1", "c2", "c3"),
                 simplify = TRUE)
  expect_true(all(!is.na(specs$specs$model_function)))
})

# Test 10:

test_that("setup creates a subsets variable", {
  specs <- specr::setup(data = example_data,
                 x = c("x1", "x2"),
                 y = c("y1", "y2"),
                 model = "lm",
                 subsets = list(group1 = unique(example_data$group1)),
                 controls = c("c1", "c2", "c3"),
                 simplify = TRUE)
  expect_true(all(!is.na(specs$specs$subsets)))
})

# Test 11:

test_that("setup creates object of class `specr.setup`", {
  specs <- specr::setup(data = example_data,
                 x = c("x1", "x2"),
                 y = c("y1", "y2"),
                 model = "lm",
                 subsets = list(group1 = unique(example_data$group1)),
                 controls = c("c1", "c2", "c3"),
                 simplify = TRUE)
  expect_true(inherits(specs, "specr.setup"))
})
