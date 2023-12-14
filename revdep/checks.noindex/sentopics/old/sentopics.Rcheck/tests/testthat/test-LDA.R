
context("testing LDA")
toks <- ECB_press_conferences_tokens[1:10]

LDA <- LDA(toks)

test_that("LDA works", {
  expect_output(print(LDA), "An LDA model with 5 topics. Currently grown by 0 Gibbs sampling iterations.")
  LDA <- grow(LDA, 2, displayProgress = FALSE)
  expect_true(check_integrity(LDA, fast = FALSE))
  expect_s3_class(LDA, "LDA")

  LDA2 <- grow(LDA, 2, nChains = 2, displayProgress = FALSE)
  LDA2 <- grow(LDA2, 2, displayProgress = FALSE)
  expect_identical(attr(LDA2, "containedClass"), "LDA")

  expect_true(all(sapply(LDA2, check_integrity, fast = FALSE)))
})

LDA <- grow(LDA, 2, displayProgress = FALSE)

test_that("functions works", {
  expect_s3_class(topWords(LDA), "data.table")
  expect_true(is.matrix(topWords(LDA, output = "matrix")))
  expect_silent(p <- topWords(LDA, output = "plot"))
  expect_s3_class(p, "ggplot")
  expect_silent(print(p))
  expect_silent(m <- melt(LDA))
  expect_s3_class(m, "data.table")
  # expect_equal(ncol(m), 6)
  expect_silent(p <- plot(LDA))
  expect_s3_class(p, "plotly")
  expect_silent(print(p))
})

test_that("test convergence", {
  vocab <- generateVocab(nTopics = 5, nSentiments = 1, nWords = 5, nCommonWords = 0, betaDirichlet = 10000000)
  toks <- generateDocuments(vocab, nDocs = 100, L1prior = 1, nWords = 100, nClass = 1)
  LDA <- LDA(toks)

  LDA <- grow(LDA, 100, nChains = 10, displayProgress = FALSE)
  expect_lte(mean(distToGenerative(LDA, vocab)), .15)
})

test_that("misc", {
  expect_length(dim(LDA$phi), 2)
  expect_length(dim(as.sentopicmodel(LDA)$phi), 3)
  expect_length(dim(as.LDA(as.sentopicmodel(LDA))$phi), 2)
})