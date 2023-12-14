

toks <- ECB_press_conferences_tokens

test_that("prior population works", {
  jst <- JST(toks)

  expect_message(sent <- sentopics_sentiment(jst), "'.sentiment' docvars found.")

  expect_identical(sentopics_sentiment(jst), data.table(.id = names(toks), .sentiment = ECB_press_conferences_tokens$.sentiment))
  expect_identical(sentopics_date(jst), data.table(.id = names(toks), .date = as.Date(ECB_press_conferences_tokens$.date)))
})

jst <- grow(JST(toks, lexicon = LoughranMcDonald), 1, displayProgress = FALSE)


sentopics_date(jst) <- NULL
sentopics_sentiment(jst) <- NULL

test_that("removal works", {
 expect_null(jst$tokens$.date)
 expect_null(jst$tokens$.sentiment)
})




test_that("sentiment works", {
  expect_message(sent <- sentopics_sentiment(jst, method = "proportionalPol"), "Sentiment computed and assigned")

  expect_true(all(is.finite(sent$.sentiment)))

  expect_message(sent <- sentopics_sentiment(jst, override = TRUE), "Sentiment computed and assigned")

  sentopics_sentiment(jst) <- ECB_press_conferences_tokens$.sentiment

  expect_identical(sentopics_sentiment(jst), data.table(.id = names(toks), .sentiment = ECB_press_conferences_tokens$.sentiment))
  
 
  
  jst <- JST(toks)
  expect_error(sentopics_sentiment(jst, override = TRUE))
  sentopics_sentiment(jst) <- NULL
  expect_error(sentopics_sentiment(jst))
  
  lda <- LDA(toks)
  sentopics_sentiment(lda) <- NULL
  expect_error(sentopics_sentiment(lda))
  
  rjst <- grow(rJST(toks, lexicon = LoughranMcDonald), 1, displayProgress = FALSE)
  expect_message(sent <- sentopics_sentiment(rjst, override = TRUE), "Sentiment computed and assigned")
  expect_true(all(is.finite(sent$.sentiment)))
})

test_that("date works", {
  expect_error(sentopics_date(jst), "No dates stored")

  sentopics_date(jst) <- ECB_press_conferences_tokens$.date
  expect_message(sentopics_date(jst) <- ECB_press_conferences_tokens$.date)

  expect_identical(sentopics_date(jst), data.table(.id = names(toks), .date = as.Date(ECB_press_conferences_tokens$.date)))
})

sentopics_sentiment(jst) <- ECB_press_conferences_tokens$.sentiment
sentopics_date(jst) <- ECB_press_conferences_tokens$.date

test_that("sentiment_series works", {
  
  jst <- grow(JST(toks, lexicon = LoughranMcDonald), 1, displayProgress = FALSE)
  s1_1 <- sentiment_series(jst)
  sentopics_sentiment(jst, override = TRUE)
  s2 <- sentiment_series(jst)
  expect_false(isTRUE(all.equal(s1_1, s2, check.attributes = FALSE)))
  
  
  rjst <- grow(rJST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald), 1, displayProgress = FALSE)
  sentopics_labels(jst) <- list(topic = paste0("superTopic", 1:jst$K))
  s1_2 <- sentiment_series(rjst)
  sentopics_sentiment(rjst, override = TRUE)
  s2 <- sentiment_series(rjst)
  expect_false(isTRUE(all.equal(s1_2, s2, check.attributes = FALSE)))
  
  lda <- grow(LDA(ECB_press_conferences_tokens), 1, displayProgress = FALSE)
  s1_3 <- sentiment_series(lda)
  
  expect_identical(s1_1, s1_2)
  expect_identical(s1_1, s1_3)
  
  expect_s3_class(s1_1, "xts")
  expect_s3_class(sentiment_series(lda, as.xts = FALSE), "data.frame")
  
  
  
  sentiment_series(lda, period = "day", rolling_window = 30, as.xts = FALSE)
  
  
  lda <- LDA(ECB_press_conferences_tokens[1:50])
  expect_error(sentiment_series(lda))
  expect_silent(sentiment_series(lda, period = "day"))
})


test_that("series functions works for LDA", {

  lda <- LDA(ECB_press_conferences_tokens)
  expect_silent(res <- sentiment_series(lda))
  expect_equal(mean(res), 0) 
  expect_equal(sd(res), 1) 
  lda <- grow(lda, 1, displayProgress = FALSE)
  
  expect_silent(sentiment_topics(lda))
  expect_silent(breakdown <- sentiment_breakdown(lda))
  expect_silent(proportion_topics(lda))
  
  expect_equal(as.matrix(res$sentiment), as.matrix(breakdown$sentiment))
  
  # check plots
  plot_sentiment_topics(lda, period = "day", rolling_window = 30)
  plot_sentiment_topics(lda, plot_ridgelines = FALSE)
  plot_sentiment_breakdown(lda, period = "day", rolling_window = 30)
  plot_proportion_topics(lda, period = "day", rolling_window = 30)
  plot_proportion_topics(lda, plot_ridgelines = FALSE)
})

test_that("series functions works for rJST", {
  
  rjst <- rJST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
  rjst <- grow(rjst, 10, displayProgress = FALSE)

  expect_warning(sentiment_breakdown(rjst))
  expect_warning(sentiment_topics(rjst))
  
  expect_silent(complete <- proportion_topics(rjst))
  expect_silent(not_complete <- proportion_topics(rjst, complete = FALSE))
  
  agg <- matrix(0, nrow(complete), rjst$K)
  for (i in seq_len(rjst$K)) {
    agg[, i] <- rowSums(complete[, seq_len(rjst$S) + (i - 1) * rjst$S])
  }
  expect_equal(agg, unclass(not_complete), check.attributes = FALSE)
  
  sentopics_sentiment(rjst, override = TRUE)
  sentnames <- names(sentopics_sentiment(rjst))
  sentnames <- sub("^\\.s_", "", sentnames[grepl("^\\.s_", sentnames)])
  expect_equal(
    sentopics_labels(rjst, flat = FALSE)$topic,
    sentnames
  )
  
  ## check if changing labels propagate to stored sentiment
  sentopics_labels(rjst) <- list(
    topic = paste0("superTopic", 1:jst$K),
    sentiment = c("negative", "neutral", "positive")
  )
  sentnames <- names(sentopics_sentiment(rjst))
  sentnames <- sub("^\\.s_", "", sentnames[grepl("^\\.s_", sentnames)])
  expect_equal(
    sentopics_labels(rjst, flat = FALSE)$topic,
    sentnames
  )
  
  expect_silent(sentiment_breakdown(rjst))
  plot_sentiment_breakdown(rjst, scale = FALSE)
  
  expect_silent(sentiment_topics(rjst))
  plot_sentiment_topics(rjst, period = "month", scale = FALSE)
  plot_sentiment_topics(rjst, period = "month", scale = FALSE, plot_ridgelines = FALSE)
})


test_that("series functions works for JST", {
  
  jst <- JST(ECB_press_conferences_tokens, lexicon = LoughranMcDonald)
  jst <- grow(jst, 10, displayProgress = FALSE)
  
  expect_error(sentiment_breakdown(jst))
  expect_error(sentiment_topics(jst))
  
  expect_silent(complete <- proportion_topics(jst))
  expect_silent(not_complete <- proportion_topics(jst, complete = FALSE))
  
  agg <- matrix(0, nrow(complete), jst$S)
  for (i in seq_len(jst$S)) {
    agg[, i] <- rowSums(complete[, seq_len(jst$K) + (i - 1) * jst$K])
  }
  expect_equal(agg, unclass(not_complete), check.attributes = FALSE)
  
  
  plot_proportion_topics(jst)
  plot_proportion_topics(jst, complete = FALSE)
  plot_proportion_topics(jst, plot_ridgelines = FALSE)
  plot_proportion_topics(jst, complete = FALSE, plot_ridgelines = FALSE)

})

