# Tests setup

library(SingleCellExperiment)

set.seed(1)

# Expression
ncells <- 75
ngenes <- 100
exp.vec <- rpois(ncells*ngenes, 5)
#add zeros
exp.vec[sample(seq_along(exp.vec), 4500)] <- 0
exp <- matrix(exp.vec,ncol=ncells)
rownames(exp) <- paste0("gene", seq_len(ngenes))
colnames(exp) <- demuxlet.example$BARCODE[seq_len(ncells)]
logexp <- log2(exp + 1)

# Dimensionality reduction
pca <- matrix(runif(ncells*5,-2,2), ncells)
tsne <- matrix(rnorm(ncells*2), ncells)
colnames(pca) <- paste0("PC_", 1:5)
colnames(tsne) <- paste0("TSNE_", 1:2)

# Metadata
l1 <- pca[,1]<0
l2 <- pca[,2]<0
clusters <- array(1, ncells)
clusters[l1&l2] <- 2
clusters[l1&!l2] <- 3
clusters[!l1&l2] <- 4
clusters <- factor(clusters)
groups <- sample(c("A","B","C","D","E"), ncells, replace = TRUE)
age <- sample(c("1","2","3","4"), ncells, replace = TRUE)
score <- logexp[5,]
score2 <- rnorm(ncells, 5, 2)
score3 <- rnorm(ncells, 10, 2)

# Make object
sce <- SingleCellExperiment(
    assays = list(counts = exp,
                  logcounts = logexp),
    reducedDims = list(PCA = pca, TSNE = tsne),
    colData = DataFrame(clusters = clusters,
                        groups = groups,
                        age = age,
                        score = score,
                        score2 = score2,
                        score3 = score3),
    rowData = DataFrame(
        rows = rownames(exp),
        symbol = paste(rownames(exp), "symb", sep = "_"))
)

# Manually make bulk
bulk <- sce
int_metadata(bulk) <- c(
    int_metadata(bulk),
    dittoSeqVersion = packageVersion("dittoSeq"),
    bulk = TRUE)

bulk_se <- SummarizedExperiment(
    assays = list(counts = exp,
                  logcounts = logexp),
    colData = DataFrame(clusters = clusters,
                        groups = groups,
                        age = age,
                        score = score,
                        score2 = score2,
                        score3 = score3),
    rowData = DataFrame(
        rows = rownames(exp),
        symbol = paste(rownames(exp), "symb", sep = "_"))
)

# Remove the unneeded external data
rm(exp,exp.vec,logexp,pca,tsne,l1,l2,clusters,groups,age,score,score2,score3)

