
context("Tests for coherence metrics")
toks <- ECB_press_conferences_tokens[quanteda::ntoken(ECB_press_conferences_tokens) >= 110]
dfm <- quanteda::dfm(toks, tolower = FALSE)
dfm <- quanteda::dfm_trim(dfm, min_termfreq = 2)
dfm <- quanteda::dfm_remove(dfm, quanteda::stopwords("en"))
toks <- quanteda::tokens_keep(toks, colnames(dfm), padding = TRUE)
sentopicmodel <- sentopicmodel(toks, lexicon = LoughranMcDonald)
sentopicmodel <- grow(sentopicmodel, 100, displayProgress = FALSE)


test_that("basic metrics works", {
  expect_silent(coherence(sentopicmodel))
  expect_error(coherence(sentopicmodel, 1))
  expect_error(coherence(sentopicmodel, "C_V"))
  expect_silent(res <- coherence(sentopicmodel, method = "C_V"))
  expect_false(anyNA(res))
  expect_silent(res <- coherence(sentopicmodel, method = "C_NPMI"))
  expect_false(anyNA(res))
  # expect_silent(res <- coherence(sentopicmodel, method = "topics"))
  # expect_false(anyNA(res))
  # expect_silent(res <- coherence(sentopicmodel, method = "topicsScaled"))
  # expect_false(anyNA(res))
  # expect_silent(res <- coherence(sentopicmodel, method = "CLexicon"))
  # expect_false(anyNA(res[c(1,3), ]))
  # expect_silent(res <- coherence(sentopicmodel, method = "CLexiconScaled"))
  # expect_false(anyNA(res[c(1,3), ]))
  # expect_silent(res <- coherence(sentopicmodel, method = "CLexiconScaledNPMI"))
  # expect_false(anyNA(res[c(1,3), ]))
  # expect_silent(res <- coherence(sentopicmodel, method = "hierarchyTopics"))
  # expect_false(anyNA(res))
})

LDA <- LDA(toks)
LDA <- grow(LDA, 100, displayProgress = FALSE)

JST <- JST(toks)
JST <- grow(JST, 100, displayProgress = FALSE)

rJST <- rJST(toks)
rJST <- grow(rJST, 100, displayProgress = FALSE)

test_that("other models works", {
  expect_silent(res <- coherence(LDA))
  expect_false(anyNA(res))
  expect_silent(res <- coherence(JST))
  expect_false(anyNA(res))
  expect_silent(res <- coherence(rJST))
  expect_false(anyNA(res))

  # expect_identical(coherence(rJST), coherence.sentopicmodel(rJST))
  expect_length(coherence(LDA), 5)
})
