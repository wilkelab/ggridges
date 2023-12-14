

test_that("from STM", {
  skip_if_not_installed("stm")
  skip_if_not_installed("tm")
  library("stm")
  temp <- textProcessor(documents = gadarian$open.ended.response,
                             metadata = gadarian, verbose = FALSE)
  out <- prepDocuments(temp$documents, temp$vocab, temp$meta,
                            verbose = FALSE)
  # set.seed(02138)
  stm <- stm(out$documents, out$vocab, 3, verbose = FALSE,
                  prevalence = ~treatment + s(pid_rep), data = out$meta)

  lda <- as.LDA(stm, out$documents)

  expect_true(check_integrity(lda))
  expect_silent(topWords(lda))

  # check top words
  expect_identical(
    unname(topWords(lda, 7, out = "matrix", method = "probability")),
    t(labelTopics(stm)$prob)
  )

  # check top documents
  expect_identical(
    unname(apply(lda$theta, 2, function(x) head(order(-x)))),
    apply(stm$theta, 2, function(x) head(order(-x)))
  )

  expect_error(grow(lda), "Not possible for approximated")
  expect_error(mergeTopics(lda, as.list(1:3)), "Not possible for approximated")
  expect_error(rJST(lda), "Not possible for approximated")

  expect_silent(sentopics_sentiment(lda) <- rnorm(length(lda$tokens)))
  expect_error(sentopics_labels(lda) <- list(topic = 1:3), "Not possible for approximated")
})


test_that("from lda", {
  skip_if_not_installed("lda")
  library("lda")
  K <- 5 ## Num clusters
  data("cora.documents")
  data("cora.vocab")
  lda <- lda.collapsed.gibbs.sampler(cora.documents,
                                        K, ## Num clusters
                                        cora.vocab,
                                        100, ## Num iterations
                                        0.1,
                                        0.1)

  LDA <- as.LDA_lda(lda, cora.documents, alpha = .1, eta = .1)

  expect_equal(unname(lda$topics), rebuild_zw(as.sentopicmodel(LDA)))
  expect_equal(unname(lda$document_sums), rebuild_zd(as.sentopicmodel(LDA)))
  expect_equal(median(LDA$alpha), 0.1)
  expect_equal(median(LDA$beta), 0.1)

  # check top words
  topics <- lda$topics[, order(colnames(lda$topics))] # force correct order
  expect_equal(
    top.topic.words(topics),
    topWords(LDA, method = "frequency", 20, output = "matrix"),
    check.attributes = FALSE
  )

  # check top documents
  expect_identical(
    unname(apply(LDA$theta, 2, function(x) head(order(-x), 20))),
    top.topic.documents(lda$document_sums)
  )
})


test_that("from topicmodels", {
  skip_if_not_installed("topicmodels")
  library("topicmodels")
  data("AssociatedPress", package = "topicmodels")
  lda <- topicmodels::LDA(AssociatedPress, method = "Gibbs",
                          control = list(alpha = 0.1, iter = 100), k = 5)

  LDA <- as.LDA(lda, docs = AssociatedPress)

  # check posterior
  expect_equal(unname(LDA$phi[order(LDA$vocabulary$word), ]), exp(t(lda@beta)))
  expect_equal(unname(LDA$theta), lda@gamma)

  # check top words
  expect_equal(
    terms(lda, k = 10),
    topWords(LDA, output = "matrix"),
    check.attributes = FALSE
  )

  vem <- topicmodels::LDA(AssociatedPress, method = "VEM",
                          control = list(
                            alpha = 0.1,
                            var = list(iter.max = 10, tol = 0.01),
                            em = list(iter.max = 10, tol = 0.01)), k = 5)

  LDA <- as.LDA(vem, docs = AssociatedPress)

  # check posterior
  expect_equal(unname(LDA$phi[order(LDA$vocabulary$word), ]), exp(t(vem@beta)))
  expect_equal(unname(LDA$theta), vem@gamma)

  # check top words
  expect_equal(
    terms(vem, k = 10),
    topWords(LDA, output = "matrix", method = "probability"),
    check.attributes = FALSE
  )

})


test_that("from seededlda", {
  skip_if_not_installed("seededlda")
  library("seededlda")
  lda <- textmodel_lda(dfm(ECB_press_conferences_tokens),
                       k = 6, max_iter = 100)
  LDA <- as.LDA(lda)

  expect_true(check_integrity(LDA))
  expect_silent(topWords(LDA))

  # check top words
  lda$phi <- lda$phi[, order(colnames(lda$phi))] # force correct order
  expect_identical(
    topWords(LDA, output = "matrix", method = "probability"),
    terms(lda)
  )

  # check top documents
  expect_equal(
    apply(LDA$theta, 2, function(x) head(order(-x))),
    apply(lda$theta, 2, function(x) head(order(-x))),
    check.attributes = FALSE
  )


  dict <- quanteda::dictionary(list(
    inflation = c("inflation", "price", "hicp"),
    council = c("governing_council", "staff", "ecb"),
    credit = c("credit", "bank",  "loan"),
    growth = c("growth", "development", "increase"),
    `monetary policy` = c("monetary_policy", "monetary", "interest_rate")
  ))
  slda <- textmodel_seededlda(dfm(ECB_press_conferences_tokens),
                              dict, residual = TRUE,
                              k = 6, max_iter = 100)
  LDA <- as.LDA(slda)

  expect_true(check_integrity(LDA))
  expect_silent(topWords(LDA))

  # check top words
  slda$phi <- slda$phi[, order(colnames(slda$phi))] # force correct order
  expect_identical(
    topWords(LDA, output = "matrix", method = "probability"),
    terms(slda)
  )

  # check top documents
  expect_equal(
    apply(LDA$theta, 2, function(x) head(order(-x))),
    apply(slda$theta, 2, function(x) head(order(-x))),
    check.attributes = FALSE
  )

})


test_that("from keyATM", {
  skip_if_not_installed("keyATM")
  library("keyATM")
  library("quanteda")
  
  data(keyATM_data_bills)
  bills_keywords <- keyATM_data_bills$keywords
  bills_dfm <- keyATM_data_bills$doc_dfm  # quanteda dfm object
  keyATM_docs <- keyATM_read(bills_dfm)
  
  # keyATM Base
  out <- keyATM(
    docs = keyATM_docs,
    model = "base",
    no_keyword_topics = 5,
    keywords = bills_keywords,
    options = list(iterations = 100)
  )
  LDA <- as.LDA(out, keyATM_docs)
  
  expect_true(check_integrity(LDA))
  expect_silent(topWords(LDA))
  
  # check top words
  out$phi <- out$phi[, order(colnames(out$phi))] # force correct order
  expect_identical(
    topWords(LDA, output = "matrix", method = "probability"),
    as.matrix(top_words(out, show_keyword = FALSE))
  )
  
  # check top documents
  expect_equal(
    apply(LDA$theta, 2, function(x) head(order(-x))),
    apply(out$theta, 2, function(x) head(order(-x))),
    check.attributes = FALSE
  )
})

test_that("LDAvis", {
  skip_if_not_installed("LDAvis")
  skip_if_not_installed("servr")

  lda <- LDA(ECB_press_conferences_tokens)
  lda <- grow(lda, 10, displayProgress = FALSE)
  if (interactive())
    expect_message(LDAvis(lda), "To stop the server,")
  else
    expect_silent(LDAvis(lda))
  servr::daemon_stop()
})
