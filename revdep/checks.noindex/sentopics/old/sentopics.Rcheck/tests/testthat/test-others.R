test_that("get_ECB_conferences works", {
  res <- get_ECB_press_conferences(years = 1998)
  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) == 227)
  expect_length(res, 6)
})

test_that("compute_PicaultRenault_scores works", {
  
  docs <- quanteda::corpus_reshape(ECB_press_conferences, "documents")
  res1 <- compute_PicaultRenault_scores(docs)
  
  
  res2 <- compute_PicaultRenault_scores(ECB_press_conferences)
  res2 <- apply(res2, 2, function(x) aggregate(x, by = list(quanteda::docid(ECB_press_conferences)), mean)$x)
  
  expect_equal(res1, res2, check.attributes = FALSE)
})

