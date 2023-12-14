context("generics")

specs <- specr::setup(data = example_data,
               x = c("x1", "x2"),
               y = "y1",
               model = "lm",
               subsets = list(group1 = unique(example_data$group1)),
               controls = c("c1", "c2"))
results <- specr(specs)

# Test 1
test_that("Function plot.specr.setup creates a ggplot", {
  p <- plot(specs)
  expect_is(p, "gg")
})


# Test 2
test_that("Function plot.specr.object creates a ggplot", {
  p <- plot(results)
  expect_is(p, "gg")
})


# Test 3
test_that("Function plot.specr.object with `type = 'curve'` creates a ggplot", {
  p <- plot(results, "curve")
  expect_is(p, "gg")
})

# Test 4
test_that("Function plot.specr.object with `type = 'choices'` creates a ggplot", {
  p <- plot(results, "choices")
  expect_is(p, "gg")
})


# Test 5
test_that("Function plot.specr.object with `type = 'choices'` creates a ggplot", {
  p <- plot(results, "boxplot")
  expect_is(p, "gg")
})


# Test 4
test_that("Function plot.specr.object with `type = 'choices'` creates a ggplot", {
  p <- plot(results, "choices")
  expect_is(p, "gg")
})



