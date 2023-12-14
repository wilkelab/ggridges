context("test phylogenetic GLMMs")

test_that("ignore these tests when on CRAN since they are time consuming", {
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  library(dplyr)
  comm = phyr::comm_a
  comm$site = row.names(comm)
  dat = tidyr::gather(comm, key = "sp", value = "freq", -site) %>% 
    left_join(phyr::envi, by = "site") %>% 
    left_join(phyr::traits, by = "sp")
  dat$pa = as.numeric(dat$freq > 0)
  dat$freq2 = 20 - dat$freq
  dat = arrange(dat, site, sp)
  # dat = sample_frac(dat, size = 0.8)
  dat = mutate(dat, Species = sp, Location = site) # can be names other than sp or site
  
  group_by(dat, site) %>% 
    summarise(nsp = n_distinct(sp)) # we now have diff sp in diff site
  
  dat = dplyr::filter(dat, sp %in% sample(unique(dat$sp), 5), site %in% sample(unique(dat$site), 8))
  
  test_fit_equal = function(m1, m2) {
    expect_equivalent(m1$B, m2$B)
    expect_equivalent(m1$B.se, m2$B.se)
    expect_equivalent(m1$B.pvalue, m2$B.pvalue)
    expect_equivalent(m1$ss, m2$ss)
    expect_equivalent(m1$AIC, m2$AIC)
  }
  
  test_fit_equal2 = function(m1, m2) {
    expect_equivalent(m1$B, m2$B)
    expect_equivalent(m1$B.se, m2$B.se)
    expect_equivalent(m1$B.pvalue, m2$B.pvalue)
    expect_equivalent(m1$ss, m2$ss)
  }
  
  # poisson plmm
  test_poisson_cpp = phyr::communityPGLMM(
    freq ~ 1 + shade + (1 | Species__) + (1 | site) + (1 | Species__@site), 
    dat, cov_ranef = list(Species = phylotree), family = 'poisson', REML = FALSE, cpp = TRUE)
  test_poisson_r = phyr::communityPGLMM(
    freq ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
    dat, cov_ranef = list(sp = phylotree), family = 'poisson', REML = FALSE, cpp = FALSE)
  test_fit_equal(test_poisson_cpp, test_poisson_r)
  
  # to add observation level random term, add (1 | Species@site)
  test_poisson_cpp2 = phyr::communityPGLMM(
    freq ~ 1 + shade + (1 | Species__) + (1 | Location) + (1 | Species__@site) + (1|Species@site), 
    dat, cov_ranef = list(Species = phylotree), family = 'poisson', REML = FALSE, cpp = TRUE)
  test_poisson_r2 = phyr::communityPGLMM(
    freq ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site) + (1|sp@site), 
    dat, cov_ranef = list(sp = phylotree), family = 'poisson', REML = FALSE, cpp = FALSE)
  test_fit_equal(test_poisson_cpp2, test_poisson_r2)
  test_fit_equal(test_poisson_cpp, test_poisson_cpp2)
  
  
  # binomial plmm
  test_binomial_cpp = phyr::communityPGLMM(
    cbind(freq, freq2) ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
    dat, cov_ranef = list(sp = phylotree), family = 'binomial', 
    REML = FALSE, cpp = TRUE, add.obs.re = FALSE)
  test_binomial_r = phyr::communityPGLMM(
    cbind(freq, freq2) ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
    dat, cov_ranef = list(sp = phylotree), family = 'binomial', 
    REML = FALSE, cpp = FALSE, add.obs.re = FALSE)
  test_fit_equal(test_binomial_cpp, test_binomial_r)
  
  # another form: provide prob
  dat$prob = dat$freq / 20
  test_binomial_cpp2 = phyr::communityPGLMM(
    prob ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
    dat, cov_ranef = list(sp = phylotree), family = 'binomial', REML = FALSE, cpp = TRUE)
  test_binomial_r2 = phyr::communityPGLMM(
    prob ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
    dat, cov_ranef = list(sp = phylotree), family = 'binomial', REML = FALSE, cpp = FALSE)
  test_fit_equal(test_binomial_cpp2, test_binomial_r2)
  
  # fit models
  test1_gaussian_cpp = phyr::communityPGLMM(
    freq ~ 1 + shade + (1 | Species__) + (1 | site) + (1 | Species__@site), 
    dat, cov_ranef = list(Species = phylotree), REML = FALSE, 
    cpp = TRUE, optimizer = "Nelder-Mead")
  
  expect_warning(phyr::communityPGLMM(
    freq ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
    dat, tree = phylotree, REML = FALSE, 
    cpp = TRUE, optimizer = "Nelder-Mead"))
  
  test1_gaussian_r = phyr::communityPGLMM(
    freq ~ 1 + shade + (1 | sp__) + (1 | Location) + (1 | sp__@site), 
    dat, cov_ranef = list(sp = phylotree), REML = FALSE, 
    cpp = FALSE, optimizer = "Nelder-Mead")
  
  test_fit_equal(test1_gaussian_cpp, test1_gaussian_r)
  
  test2_binary_cpp = phyr::communityPGLMM(
    pa ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
    dat, family = "binomial", cov_ranef = list(sp = phylotree), 
    REML = FALSE, cpp = TRUE, optimizer = "Nelder-Mead")
  
  test2_binary_r = phyr::communityPGLMM(
    pa ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
    dat, family = "binomial", cov_ranef = list(sp = phylotree), 
    REML = FALSE, cpp = FALSE, optimizer = "Nelder-Mead")
  
  if(requireNamespace("INLA", quietly = TRUE)){
    test1_gaussian_bayes = phyr::communityPGLMM(
      freq ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
      dat, cov_ranef = list(sp = phylotree), bayes = TRUE,
      prior = "pc.prior.auto")
    
    test1_binomial_bayes = phyr::communityPGLMM(
      pa ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
      dat, cov_ranef = list(sp = phylotree), ML.init = FALSE, bayes = TRUE,
      family = "binomial", prior = "pc.prior.auto")
    
    test1_poisson_bayes = phyr::communityPGLMM(
      freq ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
      dat, cov_ranef = list(sp = phylotree), bayes = TRUE, ML.init = FALSE, 
      family = "poisson", prior = "pc.prior.auto",
      prior_alpha = 0.01, prior_mu = 1)
    
    test2_binomial_bayes = phyr::communityPGLMM(
      cbind(freq, freq2) ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site),
      dat, cov_ranef = list(sp = phylotree), family = 'binomial', add.obs.re = FALSE, 
      bayes = TRUE, ML.init = FALSE, prior = "pc.prior.auto")
    
    test2_binomial_bayes_zi = phyr::communityPGLMM(
      cbind(freq, freq2) ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
      dat, cov_ranef = list(sp = phylotree), family = 'zeroinflated.binomial', 
      add.obs.re = FALSE, bayes = TRUE, ML.init = FALSE, prior = "pc.prior.auto")
    
    test2_poisson_bayes_zi = phyr::communityPGLMM(
      freq ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
      dat, cov_ranef = list(sp = phylotree), bayes = TRUE, ML.init = FALSE, 
      family = "zeroinflated.poisson", prior = "pc.prior.auto",
      prior_alpha = 0.01, prior_mu = 1)
    
    ## try a 'overdispersed' Poisson (e.g. add row random effect to account for
    ## variance in the lambda values)
    test1_poisson_bayes_overdispersed = phyr::communityPGLMM(
      freq ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site) + (1 | sp@site),
      dat, cov_ranef = list(sp = phylotree), 
      bayes = TRUE, ML.init = FALSE, family = "poisson")
    
    test_that("Bayesian communityPGLMM produced correct object", {
      expect_is(test1_gaussian_bayes, "communityPGLMM")
      # expect_is(test1_gaussian_bayes_noreml, 'communityPGLMM')
      expect_is(test1_binomial_bayes, "communityPGLMM")
      # expect_is(test1_binomial_bayes_noreml, 'communityPGLMM')
      expect_is(test1_gaussian_bayes$inla.model, "inla")
      expect_is(test1_binomial_bayes$inla.model, "inla")
      expect_equal(length(test1_gaussian_bayes$random.effects), 
                   length(c(test1_gaussian_bayes$s2n, test1_gaussian_bayes$s2r)))
      expect_equal(length(test1_binomial_bayes$random.effects), 
                   length(c(test1_binomial_bayes$s2n, test1_binomial_bayes$s2r)))
      expect_equal(length(test1_gaussian_r$B), length(test1_gaussian_bayes$B))
    })
  }
  
  test_that("cpp and r version phyr gave the same results: gaussian", {
    test_fit_equal(test1_gaussian_cpp, test1_gaussian_r)
  })
  
  test_that("cpp and r version phyr gave the same results: binomial", {
    expect_equivalent(test2_binary_cpp, test2_binary_r)
  })
  
  
  # test_that("test binary PGLMM random terms LRT", {
  #   for (i in 1:3) {
  #     expect_equal(phyr::communityPGLMM.profile.LRT(test2_binary_cpp, re.number = i), 
  #                  pez::communityPGLMM.binary.LRT(test2_binary_cpp, re.number = i), tolerance = 1e-04)
  #   }
  # })
  
  test_that("test predicted values of gaussian pglmm", {
    expect_equivalent(
      phyr::communityPGLMM.predicted.values(test1_gaussian_cpp, gaussian.pred = 'tip_rm')$Y_hat, 
      pez::communityPGLMM.predicted.values(test1_gaussian_cpp, show.plot = FALSE)[, 1])
  })
  
  # test_that("test predicted values of binary pglmm", {
  #   expect_equivalent(phyr::communityPGLMM.predicted.values(test2_binary_cpp)$Y_hat, 
  #                     pez::communityPGLMM.predicted.values(test2_binary_cpp, show.plot = FALSE))
  # }) # we changed the way to calculate predicted value for binary model in phyr (to mirror lme4)
  
    
  # data prep for pez::communityPGLMM
  nspp = n_distinct(dat$sp)
  nsite = n_distinct(dat$site)
  
  dat$site = as.factor(dat$site)
  dat$sp = as.factor(dat$sp)
  
  tree = ape::drop.tip(phylotree, setdiff(phylotree$tip.label, unique(dat$sp)))
  Vphy <- ape::vcv(tree)
  Vphy <- Vphy/max(Vphy)
  Vphy <- Vphy/exp(determinant(Vphy)$modulus[1]/nspp)
  Vphy = Vphy[levels(dat$sp), levels(dat$sp)]
  
  # prepare random effects
  re.site <- list(1, site = dat$site, covar = diag(nsite))
  re.sp <- list(1, sp = dat$sp, covar = diag(nspp))
  re.sp.phy <- list(1, sp = dat$sp, covar = Vphy)
  # sp is nested within site
  re.nested.phy <- list(1, sp = dat$sp, covar = Vphy, site = dat$site)
  re.nested.rep <- list(1, sp = dat$sp, covar = solve(Vphy), site = dat$site)
  # can be named
  re = list(re.sp = re.sp, re.sp.phy = re.sp.phy, 
            re.nested.phy = re.nested.phy, re.site = re.site)
  
  test1_gaussian_pez <- pez::communityPGLMM(
    freq ~ 1 + shade, data = dat, sp = dat$sp, 
    site = dat$site, random.effects = re, REML = FALSE)
  test_that("testing gaussian models: phyr == pez package", {
    test_fit_equal(test1_gaussian_cpp, test1_gaussian_pez)
  })
  
  test_that("phyr should be able to run in the format of pez: gaussian", {
    pglmm_phyr_pez = phyr::communityPGLMM(
      freq ~ 1 + shade, data = dat, sp = dat$sp, site = dat$site, 
      random.effects = re, REML = FALSE, optimizer = "Nelder-Mead")
    test_fit_equal(test1_gaussian_cpp, pglmm_phyr_pez)
  })
  
  test2_binary_pez <- pez::communityPGLMM(
    pa ~ 1 + shade, data = dat, family = "binomial", sp = dat$sp, 
    site = dat$site, random.effects = re, REML = FALSE)
  
  test_that("testing binomial models with pez package, should have same results", {
    test_fit_equal2(test2_binary_cpp, test2_binary_pez)
  })
  
  test_that("phyr should be able to run in the format of pez: binomial", {
    pglmm_phyr_pez = phyr::communityPGLMM(
      pa ~ 1 + shade, data = dat, family = "binomial", 
      sp = dat$sp, site = dat$site, random.effects = re, 
      REML = FALSE, optimizer = "Nelder-Mead")
    test_fit_equal2(test2_binary_pez, pglmm_phyr_pez)
  })
  
  # test NAs
  dat.na = dat
  dat.na$freq[dat.na$freq == 0] = NA
  dat.na.rm = dat.na[!is.na(dat.na$freq), ]
  
  test_that("testing data with NA, gaussian models", {
    z.na = phyr::communityPGLMM(
      freq ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
      dat.na, cov_ranef = list(sp = phylotree), REML = FALSE)
    z.na.rm = phyr::communityPGLMM(
      freq ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), 
      dat.na.rm, cov_ranef = list(sp = phylotree), REML = FALSE)
    # NOTE: freq = NA is DIFFERENT from freq = 0 !
    test_fit_equal(z.na, z.na.rm)
  })
  
  ina = sample(nrow(dat), 10)
  dat.na$pa[ina] = NA
  dat.na.rm = dat.na[!is.na(dat.na$pa), ]
  
  test_that("testing data with NA, binomial models", {
    z2.na = phyr::communityPGLMM(
      pa ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), dat.na, 
      family = "binomial", cov_ranef = list(sp = phylotree), REML = FALSE)
    z2.na.rm = phyr::communityPGLMM(
      pa ~ 1 + shade + (1 | sp__) + (1 | site) + (1 | sp__@site), dat.na.rm, 
      family = "binomial", cov_ranef = list(sp = phylotree), REML = FALSE)
    # NOTE: pa = NA is DIFFERENT from pa = 0 !
    test_fit_equal(z2.na, z2.na.rm)
  })
  
  # test communityPGLMM.profile.LRT
  test_that("testing communityPGLMM.profile.LRT", {
    expect_equal(
      communityPGLMM.profile.LRT(test2_binary_cpp, re.number = c(1, 3),  cpp = TRUE), 
      communityPGLMM.profile.LRT(test2_binary_cpp, re.number = c(1, 3),  cpp = FALSE), 
      tolerance = 1e-05)
  })
  
  # test bipartite
  tree_site = ape::rtree(n = n_distinct(dat$site), 
                         tip.label = sort(unique(dat$site)))
  
  expect_error(phyr::communityPGLMM(
    freq ~ 1 + shade + (1 | sp__) + (1 | site__) + (1 | sp__@site) + 
      (1 | sp@site__) + (1 | sp__@site__), 
    data = dat, family = "gaussian", 
    cov_ranef = list(spi = phylotree), REML = TRUE))
  
  z_bipartite = phyr::communityPGLMM(
    freq ~ 1 + shade + (1 | sp__) + (1 | site__) + (1 | sp__@site) + 
      (1 | sp@site__) + (1 | sp__@site__), 
    data = dat, family = "gaussian", 
    cov_ranef = list(sp = phylotree, site = tree_site), REML = TRUE)
  
  z_bipartite2 = phyr::communityPGLMM(
    freq ~ 1 + shade + (1 | Species__) + (1 | Location__) + (1 | Species__@Location) + 
      (1 | Species@Location__) + (1 | Species__@Location__), 
    data = dat, family = "gaussian", 
    cov_ranef = list(Species = phylotree, Location = tree_site), REML = TRUE)
  test_fit_equal(z_bipartite, z_bipartite2)
  
  if(requireNamespace("INLA", quietly = TRUE)){
    z_bipartite_bayes = phyr::communityPGLMM(
      freq ~ 1 + shade + (1 | sp__) + (1 | site__) + 
        (1 | sp__@site) + (1 | sp@site__) + (1 | sp__@site__), data = dat, 
      family = "gaussian", cov_ranef = list(sp = phylotree, site = tree_site),
      bayes = TRUE, ML.init = FALSE)
    
    z_bipartite_bayes_2 = phyr::communityPGLMM(
      freq ~ 1 + shade + (1 | sp__) + (1 | site__) + 
        (1 | sp__@site) + (1 | sp@site__) + (1 | sp__@site__), data = dat, 
      family = "gaussian",  cov_ranef = list(sp = phylotree, site = tree_site),
      bayes = TRUE, ML.init = FALSE, prior = "pc.prior.auto")
  }
  
  # # test tree and tree_site as cov matrix
  # test_that("testing tree and tree_site as cov matrix", {
  #     z_mat = phyr::communityPGLMM(freq ~ 1 + shade + (1 | sp__) + (1 | site__) + (1 | 
  #         sp__@site) + (1 | sp@site__) + (1 | sp__@site__), data = dat, family = "gaussian", 
  #         tree = vcv2(phylotree), tree_site = vcv2(tree_site), REML = TRUE)
  #     test_fit_equal(z_bipartite, z_mat)
  # })
  
  # testing selecting nested terms to be repulsive
  Vphy_site <- ape::vcv(tree_site)
  Vphy_site <- Vphy_site/max(Vphy_site)
  Vphy_site <- Vphy_site/exp(determinant(Vphy_site)$modulus[1]/nsite)
  sitel = levels(dat$site)
  Vphy_site = Vphy_site[sitel, sitel]  # same order as site levels
  
  re.site <- list(1, site = dat$site, covar = diag(nsite))
  re.site.phy <- list(1, site = dat$site, covar = Vphy_site)
  re.sp <- list(1, sp = dat$sp, covar = diag(nspp))
  re.sp.phy <- list(1, sp = dat$sp, covar = Vphy)
  # sp is nested within site
  re.sp__.site <- list(kronecker(diag(nsite), (Vphy)))  # repulsion F
  re.sp.site__ <- list(kronecker(solve(Vphy_site), diag(nspp)))  # repulsion T
  re.sp__.site__ <- list(kronecker(solve(Vphy_site), (Vphy)))  # repulsion F T
  repul_vec = c(FALSE, TRUE, FALSE, TRUE)
  
  # can be named
  re = list(re.sp = re.sp, re.sp.phy = re.sp.phy, re.site = re.site, 
            re.site.phy = re.site.phy, re.sp__.site = re.sp__.site, 
            re.sp.site__ = re.sp.site__, re.sp__.site__ = re.sp__.site__)
  
  test_that("testing repulsion as a vector", {
    z_repul1 = phyr::communityPGLMM(
      freq ~ 1 + shade, data = dat, family = "gaussian", random.effects = re)
    z_repul2 = phyr::communityPGLMM(
      freq ~ 1 + shade + (1 | sp__) + (1 | site__) + 
        (1 | sp__@site) + (1 | sp@site__) + (1 | sp__@site__), data = dat,
      family = "gaussian", cov_ranef = list(sp = phylotree, site = tree_site), 
      repulsion = repul_vec)
    test_fit_equal(z_repul1, z_repul2)
  })
  
})
