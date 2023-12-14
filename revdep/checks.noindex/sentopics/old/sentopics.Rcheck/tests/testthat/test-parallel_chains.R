
context("Tests for multiple and parallel chains")
library("future")
plan(sequential)

vocab <- generateVocab(nTopics = 5, nSentiments = 3, nWords = 5, nCommonWords = 2)
toks <- generateDocuments(vocab, nDocs = 10, L1prior = 5, L2prior = 5, nWords = 10, nClass = 2)
generated_sentopicmodel <- sentopicmodel(toks)
generated_sentopicmodel <- grow(generated_sentopicmodel, 20, displayProgress = FALSE, nChains = 2)


test_that("multiple chains works", {
  expect_true(all(sapply(generated_sentopicmodel, check_integrity)))
  expect_s3_class(generated_sentopicmodel, "multiChains")
  expect_message(generated_sentopicmodel <- grow(generated_sentopicmodel, 20, displayProgress = FALSE), NA)
  expect_true(all(sapply(generated_sentopicmodel, check_integrity)))
})

test_that("accessors works", {
  expect_s3_class(generated_sentopicmodel$tokens, "tokens")
  expect_s3_class(generated_sentopicmodel$vocabulary, "data.table")
  expect_true(check_integrity(generated_sentopicmodel$chain1))
  expect_true(check_integrity(generated_sentopicmodel[[1]]))

  expect_s3_class(tmp <- generated_sentopicmodel[1], "multiChains")
  expect_true(check_integrity(tmp[[1]]))
  expect_s3_class(head(generated_sentopicmodel), "multiChains")
  expect_s3_class(tail(generated_sentopicmodel), "multiChains")
})

if (Sys.getenv("R_COVR") != "true") {

  plan(multisession, workers = 2)
  generated_sentopicmodel <- sentopicmodel(toks)
  generated_sentopicmodel <- grow(generated_sentopicmodel, 20, displayProgress = TRUE, nChains = 10)


  test_that("parallel works", {
    expect_true(all(sapply(generated_sentopicmodel, check_integrity)))
    expect_s3_class(generated_sentopicmodel, "multiChains")
    expect_message(generated_sentopicmodel <- grow(generated_sentopicmodel, 20, displayProgress = FALSE), NA)
    expect_true(all(sapply(generated_sentopicmodel, check_integrity)))
  })

  test_that("seed works", {
    plan(sequential)
    generated_sentopicmodel <- sentopicmodel(toks)
    set.seed(123)
    sentopicmodel_1 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 1)
    set.seed(123)
    sentopicmodel_2 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 1)
    expect_identical(sentopicmodel_1, sentopicmodel_2)
    sentopicmodel_1 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 1, seed = 123)
    sentopicmodel_2 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 1, seed = 123)
    expect_identical(sentopicmodel_1, sentopicmodel_2)
    sentopicmodel_1 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 1, seed = 1234)
    expect_false(identical(sentopicmodel_1, sentopicmodel_2))
    sentopicmodel_1 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 1, seed = NULL)
    sentopicmodel_2 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 1, seed = NULL)
    expect_false(identical(sentopicmodel_1, sentopicmodel_2))

    sentopicmodel_1 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 2, seed = 123)
    sentopicmodel_2 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 2, seed = 123)
    expect_identical(sentopicmodel_1, sentopicmodel_2)
    sentopicmodel_1 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 2, seed = 1234)
    expect_false(identical(sentopicmodel_1, sentopicmodel_2))
    sentopicmodel_1 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 2, seed = NULL)
    sentopicmodel_2 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 2, seed = NULL)
    expect_false(identical(sentopicmodel_1, sentopicmodel_2))

    plan(multisession, workers = 2)
    set.seed(123)
    sentopicmodel_1 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 2)
    set.seed(123)
    sentopicmodel_2 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 2)
    expect_identical(sentopicmodel_1, sentopicmodel_2)
    sentopicmodel_1 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 2, seed = NULL)
    sentopicmodel_2 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 2, seed = NULL)
    expect_false(identical(sentopicmodel_1, sentopicmodel_2))
    sentopicmodel_1 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 2, seed = 1234)
    sentopicmodel_2 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 2, seed = 123)
    expect_false(identical(sentopicmodel_1, sentopicmodel_2))
    sentopicmodel_1 <- grow(generated_sentopicmodel, 2, displayProgress = FALSE, nChains = 2, seed = 123)
    expect_identical(sentopicmodel_1, sentopicmodel_2)

    sentopicmodel_11 <- grow(sentopicmodel_1, 2, displayProgress = FALSE, nChains = 2, seed = 1234)
    sentopicmodel_22 <- grow(sentopicmodel_2, 2, displayProgress = FALSE, nChains = 2, seed = 1234)
    expect_identical(sentopicmodel_11, sentopicmodel_22)
    sentopicmodel_22 <- grow(sentopicmodel_2, 2, displayProgress = FALSE, nChains = 2, seed = 123)
    expect_false(identical(sentopicmodel_11, sentopicmodel_22))
    sentopicmodel_11 <- grow(sentopicmodel_1, 2, displayProgress = FALSE, nChains = 2, seed = NULL)
    sentopicmodel_22 <- grow(sentopicmodel_2, 2, displayProgress = FALSE, nChains = 2, seed = NULL)
    expect_false(identical(sentopicmodel_11, sentopicmodel_22))

    
    # cran is annoyed by more than 2 cores => skip it
    skip_on_cran()
    # for CMD check
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN/Travis/AppVeyor
      num_workers <- 2L
    } else {
      # use all cores in devtools::test()
      num_workers <- min(parallel::detectCores(), 5L)
    }

    plan(multisession, workers = num_workers)
    ### this works !
    sentopicmodel_1 <- grow(generated_sentopicmodel, 5, displayProgress = FALSE, nChains = 3, seed = 1234)
    plan(multisession, workers = 2)
    sentopicmodel_2 <- grow(generated_sentopicmodel, 5, displayProgress = FALSE, nChains = 3, seed = 1234)
    plan(multisession, workers = 1)
    sentopicmodel_3 <- grow(generated_sentopicmodel, 5, displayProgress = FALSE, nChains = 3, seed = 1234)
    expect_identical(sentopicmodel_1, sentopicmodel_2)
    expect_identical(sentopicmodel_1, sentopicmodel_3)
    
    plan(multisession, workers = 2)
    sentopicmodel_3 <- grow(generated_sentopicmodel, 5, displayProgress = FALSE, nChains = 3, seed = 123)
    expect_false(identical(sentopicmodel_1, sentopicmodel_3))
  })
}

toks <- generateDocuments(vocab, nDocs = 10, L1prior = 5, L2prior = 5, nWords = 110, nClass = 2)
lex <- generatePartialLexicon(toks)
generated_sentopicmodel <- sentopicmodel(toks, lex)
generated_sentopicmodel <- grow(generated_sentopicmodel, 20, displayProgress = FALSE, nChains = 3)

test_that("scores & distances works", {
  plan(multisession, workers = if (Sys.getenv("R_COVR") != "true") 2 else 1)
  expect_silent(scores <- chainsScores(
    generated_sentopicmodel, nWords = 2,
    window = "boolean"))
  expect_false(anyNA(scores))
  expect_silent(distances <- chainsDistances(generated_sentopicmodel))
  expect_true(is.matrix(distances))
  expect_false(anyNA(distances))
  expect_silent(distances <- chainsDistances(generated_sentopicmodel, method = "cosine"))
  expect_false(anyNA(distances))
  expect_silent(distances <- chainsDistances(generated_sentopicmodel, method = "hellinger"))
  expect_false(anyNA(distances))
  expect_silent(distances <- chainsDistances(generated_sentopicmodel, method = "minMax"))
  expect_false(anyNA(distances))
  expect_silent(distances2 <- chainsDistances(generated_sentopicmodel, method = "invariantEuclidean"))
  expect_false(anyNA(distances))
  expect_true(all(distances <= distances2))
  expect_silent(distances <- chainsDistances(generated_sentopicmodel, method = "naiveEuclidean"))
  expect_false(anyNA(distances))
})


test_that("equality between euclidean distances", {
  toks <- ECB_press_conferences_tokens[1:10]
  LDA <- LDA(toks)
  LDAs <- grow(LDA, 10, nChains = 5)
  expect_equal(
    chainsDistances(LDAs),
    chainsDistances(LDAs, method = "invariantEuclidean"))
  rJST <- rJST(toks, S = 1)
  rJSTs <- grow(rJST, 10, nChains = 5)
  expect_equal(
    chainsDistances(rJSTs),
    chainsDistances(rJSTs, method = "invariantEuclidean"))
})



test_that("plot methods work", {

  expect_silent(plot(generated_sentopicmodel))

})

## fix detritus
plan(sequential)
