
# Some of the test files, e.g. test_mcmcAveProb manually manipulate "rjags"
# objects using coda::as.mcmc(). This requires R2jags:::as.mcmc.rjags() to 
# work properly, so load the package before running any test files. 

library(R2jags)  

# Maintainer note 11/2021: Need to check that the above is still true in mcmcAveProb().
# At first glance, coda does not seem to depend on R2jags, and check with 
# R_CHECK_DEPENDS_ONLY_="true")) passes. However, to avoid CRAN issues,
# R2jags changed to a depends on 11/2021. If the above isn't true and R2jags
# really is a suggests for tests only, might should change individual test files 
# to include r2jags namespace when needed and get rid of this file
# for greater clarity in future


