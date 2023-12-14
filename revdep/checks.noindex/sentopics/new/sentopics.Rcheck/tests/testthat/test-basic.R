
context("Basic tests")
toks <- ECB_press_conferences_tokens[1:20]
sentopicmodel <- sentopicmodel(toks, lex = LoughranMcDonald)


test_that("creating a sentopicmodel works", {

  expect_true(check_integrity(sentopicmodel, fast = FALSE))
  expect_s3_class(sentopicmodel, "sentopicmodel")
})

sentopicmodel <- grow(sentopicmodel, 5, displayProgress = FALSE)

test_that("growing a sentopicmodel works", {
  expect_true(check_integrity(sentopicmodel, fast = FALSE))
  expect_s3_class(sentopicmodel, "sentopicmodel")
  expect_length(sentopicmodel$logLikelihood, 5)
  expect_equal(sentopicmodel$it, 5)
  expect_true(all(c("L1post", "L2post", "phi") %in% names(sentopicmodel)))
})

test_that("reset works", {
  res <- reset(sentopicmodel)
  expect_equal(res$it, 0)
  expect_null(res$L1post)
  expect_null(res$L2post)
  expect_false(isTRUE(all.equal(sentopicmodel$za, res$za)))
  expect_length(res$logLikelihood, 1)
})

test_that("print and plot methods work", {
  expect_output(print(sentopicmodel), "A sentopicmodel topic model with 5 topics and 3 sentiments. Currently grown by 5 Gibbs sampling iterations.")
})

test_that("output functions works", {
  expect_silent(tops <- topWords(sentopicmodel, output = "matrix"))
  expect_type(tops, "character")
  expect_false(anyNA(tops))
  txts <- getTexts(sentopicmodel, topic = "l1-1", "negative")
  expect_length(txts, 3)
  expect_type(unlist(txts), "character")
})

test_that("topWords subset works", {
  expect_silent(tops <- topWords(sentopicmodel, output = "matrix", subset = topic %in% 1))
  expect_equal(dim(tops), c(10L, 3L))
  expect_silent(tops <- topWords(sentopicmodel, output = "matrix",
                                 subset = topic %in% 1 & sentiment == 3L))
  expect_equal(dim(tops), c(10L, 1L))
  expect_silent(tops <- topWords(sentopicmodel, output = "matrix",
                                 subset = topic %in% 1 & sentiment == 9L))
  expect_equal(dim(tops), c(10L, 0L))
})


test_that("topWords methods works", {
  expect_silent(tops <- topWords(sentopicmodel, method = "probability"))
  expect_true(all(is.finite(tops$value)))
  expect_output(print(tops), "probability")
  
  expect_silent(tops <- topWords(sentopicmodel, method = "term-score"))
  expect_true(all(is.finite(tops$value)))
  expect_output(print(tops), "term-score")
  
  expect_silent(tops <- topWords(sentopicmodel, method = "FREX"))
  expect_true(all(is.finite(tops$value)))
  expect_output(print(tops), "FREX")
})

test_that("Updates works", {
  alphaU <- sentopicmodel(toks, L1cycle = 2)
  alphaU <- grow(alphaU, 5, displayProgress = FALSE)
  expect_false(isTRUE(all.equal(rep(1, 5), alphaU$alpha)))

  alphaU <- LDA(toks)
  alphaU$alphaCycle <- 2
  alphaU <- grow(alphaU, 5, displayProgress = FALSE)
  expect_false(isTRUE(all.equal(rep(1, 5), alphaU$alpha)))
})

### TODO: add test for utils functions such as getTheta()

