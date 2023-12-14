data(se)
metadata <- se@metadata$meta
N <- 50 # 50 samples
P <- 100 # 100 features
set.seed(1234)
data <- matrix(rnorm(N * P, N, P), ncol = N)
colnames(data) <- paste("subject", 1:ncol(data), sep = "_")
rownames(data) <- paste("feature", 1:nrow(data), sep = "_")
se <- SummarizedExperiment(assays = list(counts = data))
S4Vectors::metadata(se) <- list(meta = metadata)


test_that("deconv function gives error for invalid inputs", {
    expect_error(
        deconv(se, method = "nnls", use_refactor = NULL),
        "None of the feaures in 'signature matrix' exist in bulk
                 expression data."
    )
})