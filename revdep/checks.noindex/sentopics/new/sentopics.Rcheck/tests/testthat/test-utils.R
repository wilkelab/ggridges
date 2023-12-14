
context("Test utils")

test_that("virtualDocuments works", {
  toks <- ECB_press_conferences_tokens[1:10]
  expect_warning(virtualDocuments(vD <- toks, window = 1000), "Some documents")
  expect_equal(names(toks), unique(gsub("\\..*", "", names(vD))))
  expect_silent(vD <- virtualDocuments(toks, window = 5))
  expect_equal(names(toks), unique(gsub("\\..*", "", names(vD))))
  expect_true(all(lengths(vD) == 5))
  expect_silent(vD <- virtualDocuments(toks, window = "boolean"))
  expect_identical(c(unclass(toks)), c(unclass(toks)))
})

test_that("as.tokens.dfm works", {
  toks <- ECB_press_conferences_tokens[1:10]
  dfm <- quanteda::dfm(toks, tolower = FALSE)
  expect_silent(LDA <- LDA(dfm))
  expect_silent(JST <- JST(dfm))
  expect_silent(rJST <- rJST(dfm))
  expect_silent(sentopicmodel <- sentopicmodel(dfm))
  expect_identical(as.tokens(dfm, tokens = toks), toks)
  
  dfm <- quanteda::dfm(ECB_press_conferences_tokens)
  toks <- as.tokens.dfm(dfm)
  expect_identical(nrow(dfm), length(toks))
  expect_identical(colnames(dfm), quanteda::types(toks))
  expect_equal(quanteda::rowSums(dfm), quanteda::ntoken(toks))
  
  toks <- tokens(c(
    "This text will be broken down into pieces with the `tokens` function",
    "The function was re-exported from the quanteda package."))
  
  dfm <- quanteda::dfm(toks) |> 
    quanteda::dfm_remove(quanteda::stopwords("en")) |> 
    quanteda::dfm_trim(max_termfreq = 1)
  
  toks_recomposed <- as.tokens(dfm, tokens = toks)
  toks_processed <-
    quanteda::tokens_remove(toks, c(quanteda::stopwords("en"), "function", "`"), padding = TRUE)
  
  expect_identical(toks_recomposed, toks_processed)
})


test_that("melt works", {
  toks <- ECB_press_conferences_tokens[1:10]
  model <- sentopicmodel(toks)
  expect_error(melt.sentopicmodel(model), "Nothing to melt")
  model <- grow(model, 10, displayProgress = FALSE)
  melt.sentopicmodel(model, include_docvars = TRUE)
})

test_that("sunburst works", {
  toks <- ECB_press_conferences_tokens[1:10]
  model <- sentopicmodel(toks)
  model <- grow(model, 10, displayProgress = FALSE)
  expect_silent(p <- plot(model))
  expect_s3_class(p, "plotly")
  expect_silent(print(p))
})
2


test_that("R likelihood works", {
  toks <- ECB_press_conferences_tokens[1:10]
  model <- sentopicmodel(toks)
  model <- grow(model, 10, displayProgress = FALSE)
  logLik <- c(tail(model$logLikelihood, 1L), sapply(attr(model$logLikelihood, "components"), tail, 1L))
  RlogLik <- multLikelihood(model)
  expect_equivalent(logLik, RlogLik[, 1])

  JST <- grow(JST(toks), displayProgress = FALSE)
  logLik <- c(tail(JST$logLikelihood, 1L), sapply(attr(JST$logLikelihood, "components"), tail, 1L))
  RlogLik <- multLikelihood(JST)
  expect_equivalent(logLik, RlogLik[, 1])
})


test_that("recompile & reset works", {
  toks <- ECB_press_conferences_tokens[1:100]
  model <- JST(toks, lex = LoughranMcDonald)
  model$vocabulary[!is.na(lexicon)]
  model$vocabulary[word == "crisis"]
  model <- grow(model, 10, displayProgress = FALSE)
  expect_true(all(model$phi["crisis", ,-1] == 0))
  expect_true(all(model$phi["crisis", ,1] > 0))

  model$vocabulary[word == "crisis", lexicon := NA ]
  idx <- model$vocabulary[word == "crisis", index]
  expect_warning(model2 <- recompileVocabulary(model),
                 "The model will be reset")
  expect_silent(model2 <- recompileVocabulary(model2))
  expect_true(all(model2$beta[, idx] > 0))
  model2 <- grow(model2, 10, displayProgress = FALSE)
  expect_equal(model2$it, 10)
  expect_true(all(model2$phi["crisis", ,] > 0))
})

test_that("grow0 doesn't alter anything", {
  toks <- ECB_press_conferences_tokens[1:2]
  model <- LDA(toks)
  expect_identical(model, grow(model, 0, displayProgress = FALSE))
  model <- grow(model, 2, displayProgress = FALSE, computeLikelihood = FALSE)
  expect_identical(model, grow(model, 0, displayProgress = FALSE))
  model <- grow(model, 2, displayProgress = FALSE)
  expect_identical(model, grow(model, 0, displayProgress = FALSE))
})


test_that("mergeTopics works", {
  toks <- ECB_press_conferences_tokens[1:50]
  model <- LDA(toks)
  merged <- mergeTopics(model, as.list(1:5))
  sentopics_labels(merged)
  sentopics_labels(merged) <- NULL
  expect_identical(merged, model)
  
  model <- grow(model, 2, displayProgress = FALSE)
  merged <- mergeTopics(model, as.list(1:5))
  sentopics_labels(merged)
  sentopics_labels(merged) <- NULL
  expect_identical(merged, model)
  
  merged <- mergeTopics(model, list(1:4, 5))
  topWords(merged)
  plot(merged)
  sentiment_series(model, period = "day")
  sentiment_breakdown(merged, period = "day")
  sentiment_topics(merged, period = "day")
  proportion_topics(merged, period = "day")
  
  toks <- ECB_press_conferences_tokens[1:50]
  model <- rJST(toks, lexicon = LoughranMcDonald)
  merged <- mergeTopics(model, as.list(1:5))
  sentopics_labels(merged)
  sentopics_labels(merged) <- NULL
  expect_identical(merged, model)
  
  model <- grow(model, 2, displayProgress = FALSE)
  merged <- mergeTopics(model, as.list(1:5))
  sentopics_labels(merged)
  sentopics_labels(merged) <- NULL
  expect_identical(merged, model)
  
  sentopics_sentiment(model) <- NULL
  sentopics_sentiment(model, override = TRUE)
  merged <- mergeTopics(model, list(1:4, 5))
  sentopics_sentiment(model)
  topWords(merged)
  plot(merged)
  sentiment_series(merged, period = "day")
  sentiment_breakdown(merged, period = "day")
  sentiment_topics(merged, period = "day")
  proportion_topics(merged, period = "day")
})

test_that("rebuild counts from posterior works", {
  rjst <- rJST(ECB_press_conferences_tokens)
  rjst <- grow(rjst, 10, displayProgress = FALSE)
  rjst <- as.sentopicmodel(rjst)
  expect_equal(rebuild_zd_from_posterior(rjst), rebuild_zd(rjst))
  expect_equal(rebuild_zw_from_posterior(rjst), rebuild_zw(rjst))
  
  lda <- LDA(ECB_press_conferences_tokens)
  lda <- grow(lda, 10, displayProgress = FALSE)
  lda <- as.sentopicmodel(lda)
  expect_equal(rebuild_zd_from_posterior(lda), rebuild_zd(lda))
  expect_equal(rebuild_zw_from_posterior(lda), rebuild_zw(lda))
})

