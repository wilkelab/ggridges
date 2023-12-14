context("test cor_phylo output")

test_that("cor_phylo produces proper output", {
  
  skip_on_cran()
  
  # ----------------------------*
  
  # Simulating data ----
  
  # ----------------------------*
  
  # Set up parameter values for simulating data
  n <- 50
  p <- 2
  Rs <- c(0.8)
  d <- c(0.3, 0.6)
  M <- matrix(c(0.25, 0.6), nrow = n, ncol = p, byrow = TRUE)
  X_means <- c(1, 2)
  X_sds <- c(1, 0.5)
  U_means <- list(NULL, 2)
  U_sds <- list(NULL, 10)
  B <- list(NULL, 0.1)
  # Simulate them using this internal function
  data_list <- phyr:::sim_cor_phylo_variates(n, Rs, d, M, X_means, X_sds, U_means, U_sds, B)
  
  # Converting to matrices for the call to ape::corphylo
  SeM <- as.matrix(data_list$data[, grepl("^se", colnames(data_list$data))])
  rownames(SeM) <- data_list$data$species
  X <- as.matrix(data_list$data[, grepl("^par", colnames(data_list$data))])
  rownames(X) <- data_list$data$species
  U <- lapply(1:p, function(i) {
    UM <- as.matrix(data_list$data[,grepl(paste0("^cov", i), colnames(data_list$data))])
    if (ncol(UM) == 0) return(NULL)
    rownames(UM) <- data_list$data$species
    return(UM)
  })
  
  
  # ----------------------------*
  
  # compare to corphylo ----
  
  # ----------------------------*
  
  
  phyr_cp <- cor_phylo(variates = ~ par1 + par2,
                       covariates = list(par2 ~ cov2a),
                       meas_errors = list(par1 ~ se1, par2 ~ se2),
                       data = data_list$data, phy = data_list$phy,
                       species = ~ species, method = "nelder-mead-r",
                       lower_d = 0)
  ape_cp <- ape::corphylo(X = X, SeM = SeM, U = U, phy = data_list$phy, 
                          method = "Nelder-Mead")
  
  
  # ----------------------------*
  
  # Test output
  
  # ----------------------------*
  
  expect_is(phyr_cp, "cor_phylo")
  expect_equivalent(names(phyr_cp), c("corrs", "d", "B", "B_cov", "logLik", "AIC",
                                      "BIC", "niter", "convcode", "rcond_vals",
                                      "bootstrap", "call"),
                    label = "Names not correct.")
  phyr_cp_names <- sapply(names(phyr_cp), function(x) class(phyr_cp[[x]]))
  expected_classes <- c(corrs = "matrix", d = "matrix", B = "matrix", B_cov = "matrix", 
                        logLik = "numeric", AIC = "numeric", BIC = "numeric", 
                        niter = "numeric", convcode = "integer", rcond_vals = "numeric",
                        bootstrap = "list", call = "call")
  expect_class_equal <- function(par_name) {
    eval(bquote(expect_equal(class(phyr_cp[[.(par_name)]])[1], 
                             expected_classes[[.(par_name)]])))
  }
  for (n_ in names(phyr_cp)) expect_class_equal(n_)
  
  
  # (Below, I'm intentionally not testing for equal `B` and `B_cov` fields bc cor_phylo
  # has a mistake fixed in it that results in slightly different values here.)
  
  expect_par_equal <- function(cp_par, ape_par = NULL) {
    if (is.null(ape_par)) ape_par <- cp_par
    eval(bquote(expect_equivalent(phyr_cp[[.(cp_par)]], ape_cp[[.(ape_par)]])))
  }
  expect_par_equal("corrs", "cor.matrix")
  expect_par_equal("logLik")
  expect_par_equal("d")
  expect_par_equal("AIC")
  expect_par_equal("BIC")
  expect_par_equal("convcode")
  
  
  # Test that not converging produces proper warning:
  phyr_cp$convcode <- 1
  expect_output(print(phyr_cp), regexp = "Warning: convergence in .* not reached")
  
  
  # ----------------------------*
  
  # different inputs -----
  
  # ----------------------------*
  
  Vphy <- ape::vcv(data_list$phy)
  
  
  phyr_cp <- cor_phylo(variates = ~ par1 + par2,
                       meas_errors = list(par1 ~ se1, par2 ~ se2),
                       data = data_list$data, phy = Vphy,
                       REML = FALSE,
                       species = ~ species, method = "subplex")
  
  expect_is(phyr_cp, "cor_phylo")
  expect_equivalent(names(phyr_cp), c("corrs", "d", "B", "B_cov", "logLik", "AIC",
                                      "BIC", "niter", "convcode", "rcond_vals",
                                      "bootstrap", "call"),
                    label = "Names not correct.")
  phyr_cp_names <- sapply(names(phyr_cp), function(x) class(phyr_cp[[x]]))
  expected_classes <- c(corrs = "matrix", d = "matrix", B = "matrix", B_cov = "matrix", 
                        logLik = "numeric", AIC = "numeric", BIC = "numeric", 
                        niter = "numeric", convcode = "integer", rcond_vals = "numeric",
                        bootstrap = "list", call = "call")
  for (n_ in names(phyr_cp)) expect_class_equal(n_)
  
  
  # Test that not converging produces proper warning:
  phyr_cp$convcode <- 1
  expect_output(print(phyr_cp), regexp = "Warning: convergence in .* not reached")
  
  # Inspecting verbose output and testing `sann_options`:
  cp_out <- capture.output({
    phyr_cp_sann <- cor_phylo(variates = ~ par1 + par2,
                              meas_errors = list(par1 ~ se1, par2 ~ se2),
                              data = data_list$data, phy = Vphy,
                              species = ~ species, method = "sann",
                              verbose = TRUE,
                              sann_options = list(maxit = 100, temp = 1.1, tmax = 2))
  })
  cp_out <- unique(sapply(strsplit(cp_out, split = "\\s+"), length))
  expect_identical(cp_out, 6L)
  
  # ----------------------------*
  
  # Test for errors
  
  # ----------------------------*
  
  
  
  expect_error(cor_phylo(variates = ~ par1 + par2,
                         covariates = list(par2 ~ cov2a),
                         meas_errors = list(par1 ~ se1, par2 ~ se2),
                         species = ~ species,
                         data = data_list$data, 
                         phy = ape::rtree(n, br = NULL)), 
               regexp = "The input phylogeny has no branch lengths")
  
  expect_error(cor_phylo(variates = ~ par1,
                         data = data_list$data,
                         phy = data_list$phy,
                         species = ~ species), 
               regexp = "argument `variates` should have >= 2 variables.")
  
  expect_error(cor_phylo(variates = c("par1", "par2"),
                         data = data_list$data, phy = data_list$phy,
                         species = ~ species), 
               regexp = "The `variates` argument to `cor_phylo` must be a formula or matrix")
  expect_error(cor_phylo(variates = ~ par1 + par2,
                         covariates = c("cov2a"),
                         data = data_list$data, phy = data_list$phy,
                         species = ~ species), 
               regexp = "arg `covariates` must be NULL or a list")
  expect_error(cor_phylo(variates = ~ par1 + par2,
                         meas_errors = c("se2"),
                         data = data_list$data, phy = data_list$phy,
                         species = ~ species), 
               regexp = paste("The `meas_errors` argument to `cor_phylo` must be NULL,",
                              "a list, or matrix."))
  
  # ---------*
  # Inserting NAs:
  # ---------*
  x <- data_list$data$par1[10]
  data_list$data$par1[10] <- NA
  expect_error(cor_phylo(variates = ~ par1 + par2,
                         covariates = list(par2 ~ cov2a),
                         meas_errors = list(par1 ~ se1, par2 ~ se2),
                         data = data_list$data, phy = data_list$phy,
                         species = ~ species), 
               regexp = "NAs are not allowed in `variates`")
  data_list$data$par1[10] <- x
  
  x <- data_list$data$cov2a[10]
  data_list$data$cov2a[10] <- NA
  expect_error(cor_phylo(variates = ~ par1 + par2,
                         covariates = list(par2 ~ cov2a),
                         meas_errors = list(par1 ~ se1, par2 ~ se2),
                         data = data_list$data, phy = data_list$phy,
                         species = ~ species), 
               regexp = "NAs are not allowed in `covariates`")
  data_list$data$cov2a[10] <- x
  
  x <- data_list$data$se1[10]
  data_list$data$se1[10] <- NA
  expect_error(cor_phylo(variates = ~ par1 + par2,
                         covariates = list(par2 ~ cov2a),
                         meas_errors = list(par1 ~ se1, par2 ~ se2),
                         data = data_list$data, phy = data_list$phy,
                         species = ~ species), 
               regexp = "NAs are not allowed in `meas_errors`")
  data_list$data$se1[10] <- x
  
  x <- data_list$data$species[10]
  data_list$data$species[10] <- NA
  expect_error(cor_phylo(variates = ~ par1 + par2,
                         covariates = list(par2 ~ cov2a),
                         meas_errors = list(par1 ~ se1, par2 ~ se2),
                         data = data_list$data, phy = data_list$phy,
                         species = ~ species), 
               regexp = "NAs are not allowed in `species`")
  data_list$data$species[10] <- x
  
  
  # ----------------------------*
  
  
  # To store output:
  cp_output_tests <- list(forms = NA,
                          matrices = NA)
  
  # Using formulas:
  cp_output_tests$forms <- 
    cor_phylo(variates = ~ par1 + par2,
              covariates = list(par2 ~ cov2a),
              meas_errors = list(par1 ~ se1, par2 ~ se2),
              species = ~ species,
              phy = data_list$phy, data = data_list$data)
  
  # Using matrices:
  
  X <- as.matrix(data_list$data[,c("par1", "par2")])
  U <- list(par2 = cbind(cov2a = data_list$data$cov2a))
  M <- cbind(par1 = data_list$data$se1, par2 = data_list$data$se2)
  
  cp_output_tests$matrices <- 
    cor_phylo(variates = X,
              covariates = U,
              meas_errors = M,
              species = data_list$data$species,
              phy = data_list$phy)
  
  for (i in 2:length(cp_output_tests)) {
    # I'm adding `-length(cp_output_tests[[<index>]])` to exclude the `call` field
    # from each `cor_phylo` object, because is not expected to be the same.
    expect_equal(cp_output_tests[[1]][-length(cp_output_tests[[1]])], 
                 cp_output_tests[[i]][-length(cp_output_tests[[i]])],
                 label = names(cp_output_tests)[1],
                 expected.label = names(cp_output_tests)[i])
  }
  
  # ==================================================================*
  # ==================================================================*
  
  # bootstrapping -----
  
  # ==================================================================*
  # ==================================================================*
  
  
  cp <- cor_phylo(variates = ~ par1 + par2,
                  covariates = list(par2 ~ cov2a),
                  meas_errors = list(par1 ~ se1, par2 ~ se2),
                  data = data_list$data, phy = data_list$phy,
                  species = ~ species, boot = 1, keep_boots = "all")
  
  # With matrices as input
  cp2 <- 
    cor_phylo(variates = X,
              covariates = U,
              meas_errors = M,
              species = data_list$data$species,
              phy = data_list$phy,
              boot = 1, keep_boots = "all")
  
  cp_bci <- boot_ci(cp)
  cp_bci2 <- boot_ci(cp2)
  
  expect_identical(names(cp_bci), c("corrs", "d", "B0", "B_cov"))
  expect_identical(names(cp_bci2), c("corrs", "d", "B0", "B_cov"))
  expect_identical(paste(sapply(cp_bci, function(i) class(i)[1])), rep("matrix", 4))
  expect_identical(paste(sapply(cp_bci2, function(i) class(i)[1])), rep("matrix", 4))
  
  expect_output(print(cp), regexp = "Bootstrapped 95\\% CIs \\(.* reps\\):")
  expect_output(print(cp2), regexp = "Bootstrapped 95\\% CIs \\(.* reps\\):")
  
  cp_refit <- refit_boots(cp)
  cp_refit2 <- refit_boots(cp2)
  
  
  expect_output(print(cp_refit), "< Refits to cor_phylo bootstraps >")
  expect_output(print(cp_refit2), "< Refits to cor_phylo bootstraps >")
 
  
  # ----------------------------*
  
  # no correlation -----
  
  # ----------------------------*
  
  
  data_list$data$par3 <- runif(nrow(data_list$data)) * data_list$data$par1
  
  phyr_cp_nc <- cor_phylo(variates = ~ par1 + par2 + par3,
                          data = data_list$data, phy = data_list$phy,
                          species = ~ species, method = "nelder-mead-r",
                          no_corr = TRUE)
  
  
  expect_equal(sum(phyr_cp_nc$corrs[lower.tri(phyr_cp_nc$corrs)]), 0)
  expect_equal(sum(phyr_cp_nc$corrs[upper.tri(phyr_cp_nc$corrs)]), 0)
 
  # ----------------------------*
  
  # Testing for fix of error in printing when using `T` or `F` instead of `TRUE` or `FALSE`
  
  # ----------------------------*
  
  
  phyr_cp <- cor_phylo(variates = ~ par1 + par2,
                       data = data_list$data, phy = data_list$phy,
                       species = ~ species, constrain_d = TRUE)
  
  
  expect_output(print(phyr_cp), "Call to cor_phylo:")
})
