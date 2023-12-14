Testdata generators
===================

The scripts here generated the files in `tests/testdata`, akin to the relationship between `data` and `data-raw`. 

The reason to keep them here under tests and not as package data is that some of them (RStan, but also the others) are quite large and noticeably slow down package loading. They also cause an R check NOTE for going over 5MB installed size. 

In case it is needed, below is minimal data object roxygen documentation:

```r
# mcmcpack_logit ----------------------------------------------------------

#' Fitted MCMpack logit model
#' 
#' A fitted MCMCpack logit model generated with [MCMCpack::MCMClogit()]. 
#' Used in examples and for testing. 
#' 
#' @format A class "mcmc" object created by [MCMCpack::MCMClogit()]
#' 
#' @examples 
#' data("mcmcpack_logit")
#' summary(mcmcpack_logit)
#' 
#' @docType data
"mcmcpack_logit"

# rstan_logit -------------------------------------------------------------

#' Fitted RStan logit model
#' 
#' A fitted RStan logit model generated with [rstan::stan()]. 
#' Used in examples and for testing. 
#' 
#' @format A class "stanfit" object created by [rstan::stan()]
#' 
#' @examples 
#' data("rstan_logit")
#' rstan_logit
#' 
#' @docType data
"rstan_logit"

# rstanarm_logit ----------------------------------------------------------

#' Fitted rstanarm logit model
#' 
#' A fitted rstanarm logit model generated with [rstanarm::stan_glm()]. 
#' Used in examples and for testing. 
#' 
#' @format A class "stanreg" object created by [rstanarm::stan_glm()]
#' 
#' @examples 
#' data("rstanarm_logit")
#' summary(rstanarm_logit)
#' 
#' @docType data
"rstanarm_logit"
```