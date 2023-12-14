
context("Tests for generated data")
vocab <- generateVocab(nTopics = 5, nSentiments = 3, nWords = 5, nCommonWords = 2)
vocab2 <- vocabFromList(unname(list(
  topic1 = list(sent1 = c("tasty", "delicious"), sent2 = c("bad", "smelly")),
  topic2 = list(sent1 = c("surprising", "amazing"), sent2 = c("boring", "annoying")),
  topic3 = list(sent1 = "fearless", sent2 = "coward")
)))
vocab3 <- vocabFromList(unname(list(
  topic1 = "super",
  topic2 = "extra",
  topic3 = "top"
)))

test_that("vocab generation works", {
  expect_equal(dim(vocab), c(5 * 3 * 5 + 2 * (5 + 3), 3, 5))
  expect_equal(sum(vocab), 5 * 3)
  expect_equal(sum(vocab < 0), 0)
  
  expect_equal(sum(vocab2), 3 * 2)
  expect_equal(sum(vocab2 < 0), 0)
  expect_equal(sum(vocab3), 3 * 1)
  expect_equal(sum(vocab3 < 0), 0)
})

toks <- generateDocuments(vocab, nDocs = 20, L1prior = 5, L2prior = 5, nWords = 200, nClass = 2)
test_that("document generation works", {
  expect_length(toks, 20)
  expect_equal(sort(unique(quanteda::docvars(toks, "class"))), 1:2)
  expect_s3_class(toks, "tokens")
})

lex <- generatePartialLexicon(toks)
test_rJST <- rJST(toks, lexicon = lex)
test_JST <- JST(toks, lexicon = lex)
test_that("lexicon & class  generation works", {

  expect_s4_class(lex, "dictionary2")

  lexWords <- unique(unlist(lex))
  expect_length(lexWords, 4)

  expect_setequal(test_rJST$vocabulary[!is.na(lexicon), word], lexWords)
  expect_setequal(test_JST$vocabulary[!is.na(lexicon), word], lexWords)

  expect_lt(sum(test_rJST$beta), length(test_rJST$beta)*.01)
  expect_lt(sum(test_JST$beta), length(test_JST$beta)*.01)
})

generated_sentopicmodel <- sentopicmodel(toks)
generated_sentopicmodel <- grow(generated_sentopicmodel, 20, displayProgress = FALSE)
test_that("generated sentopicmodel works", {
  expect_true(check_integrity(generated_sentopicmodel))
  expect_s3_class(generated_sentopicmodel, "sentopicmodel")
  expect_length(generated_sentopicmodel$logLikelihood, 20)
  expect_equal(generated_sentopicmodel$it, 20)
  expect_true(all(c("L1post", "L2post", "phi") %in% names(generated_sentopicmodel)))
})

test_that("generateDocuments incorrect priors", {
  vocab <- generateVocab(nTopics = 2, nSentiments = 3, nWords = 5, nCommonWords = 2)
  expect_error(
    corpus <- generateDocuments(vocab, nClass = 2, L1prior = matrix(1:6, 3, 2), L2prior = array(1:12, dim = c(2, 3, 2)), nDocs = 20),
    "Please provide a valid L1prior"
  )
  expect_error(
    corpus <- generateDocuments(vocab, nClass = 2, L1prior = matrix(1:4, 2, 2), L2prior = array(1:18, dim = c(2, 3, 3)), nDocs = 20),
    "Please provide a valid L2prior"
  )
  expect_output(
    corpus <- generateDocuments(vocab, nClass = 2, L1prior = matrix(1:4, 2, 2), L2prior = array(1:12, dim = c(2, 3, 2)), nDocs = 20),
    NA
  )
})

