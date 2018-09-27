# bayesplot

Version: 1.6.0

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.4Mb
      sub-directories of 1Mb or more:
        R     1.8Mb
        doc   4.0Mb
    ```

# cRegulome

Version: 0.2.0

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Loading required package: IRanges
    Warning: package 'IRanges' was built under R version 3.5.1
    Loading required package: S4Vectors
    
    Attaching package: 'S4Vectors'
    
    The following object is masked from 'package:base':
    
        expand.grid
    
    
    Attaching package: 'IRanges'
    
    The following object is masked from 'package:R.oo':
    
        trim
    
    Quitting from lines 32-41 (case_study.Rmd) 
    Error: processing vignette 'case_study.Rmd' failed with diagnostics:
    there is no package called 'org.Hs.eg.db'
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘org.Hs.eg.db’
    ```

# dextergui

Version: 0.1.4

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    ...
    Error in re-building vignettes:
      ...
    Warning: package 'dplyr' was built under R version 3.5.1
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Loading required package: RSQLite
    no column `person_id` provided, automatically generating unique person id's
    pandoc: Could not fetch img/main_bar.PNG
    img/main_bar.PNG: openBinaryFile: does not exist (No such file or directory)
    Error: processing vignette 'dextergui.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 67
    Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Unexported objects imported by ':::' calls:
      ‘dexter:::get_resp_data’ ‘dexter:::qcolors’
      See the note in ?`:::` about the use of this operator.
    ```

# enrichplot

Version: 1.0.2

## In both

*   checking whether package ‘enrichplot’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/wilke/github/ggridges/revdep/checks.noindex/enrichplot/new/enrichplot.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘org.Hs.eg.db’
    ```

## Installation

### Devel

```
* installing *source* package ‘enrichplot’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘enrichplot’
* removing ‘/Users/wilke/github/ggridges/revdep/checks.noindex/enrichplot/new/enrichplot.Rcheck/enrichplot’

```
### CRAN

```
* installing *source* package ‘enrichplot’ ...
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  there is no package called ‘GO.db’
ERROR: lazy loading failed for package ‘enrichplot’
* removing ‘/Users/wilke/github/ggridges/revdep/checks.noindex/enrichplot/old/enrichplot.Rcheck/enrichplot’

```
# HistDAWass

Version: 1.0.1

## In both

*   checking whether package ‘HistDAWass’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/wilke/github/ggridges/revdep/checks.noindex/HistDAWass/new/HistDAWass.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘HistDAWass’ ...
** package ‘HistDAWass’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/wilke/github/ggridges/revdep/library.noindex/HistDAWass/Rcpp/include" -I"/Users/wilke/github/ggridges/revdep/library.noindex/HistDAWass/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘HistDAWass’
* removing ‘/Users/wilke/github/ggridges/revdep/checks.noindex/HistDAWass/new/HistDAWass.Rcheck/HistDAWass’

```
### CRAN

```
* installing *source* package ‘HistDAWass’ ...
** package ‘HistDAWass’ successfully unpacked and MD5 sums checked
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I"/Users/wilke/github/ggridges/revdep/library.noindex/HistDAWass/Rcpp/include" -I"/Users/wilke/github/ggridges/revdep/library.noindex/HistDAWass/RcppArmadillo/include" -I/usr/local/include  -fopenmp  -fPIC  -Wall -g -O2 -c RcppExports.cpp -o RcppExports.o
clang: error: unsupported option '-fopenmp'
make: *** [RcppExports.o] Error 1
ERROR: compilation failed for package ‘HistDAWass’
* removing ‘/Users/wilke/github/ggridges/revdep/checks.noindex/HistDAWass/old/HistDAWass.Rcheck/HistDAWass’

```
# scatr

Version: 1.0.1

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘R6’ ‘cowplot’ ‘ggplot2’ ‘ggridges’ ‘ggstance’ ‘jmvcore’
      All declared Imports should be used.
    ```

# Seurat

Version: 2.3.4

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘ape’
    
    Packages suggested but not available for checking: ‘loomR’ ‘phateR’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

# tidybayes

Version: 1.0.1

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             modules = modules, factories = factories, jags = jags, call.setup = TRUE, method = method, 
             mutate = mutate)
      10: setup.jags(model = outmodel, monitor = outmonitor, data = outdata, n.chains = n.chains, 
             inits = outinits, modules = modules, factories = factories, response = response, 
             fitted = fitted, residual = residual, jags = jags, method = method, mutate = mutate)
      11: loadandcheckrjags()
      12: stop("Loading the rjags package failed (diagnostics are given above this error message)", 
             call. = FALSE)
      
      ══ testthat results  ══════════════════════════════════════════════════════════
      OK: 235 SKIPPED: 2 FAILED: 1
      1. Error: tidy_draws works with runjags (@test.tidy_draws.R#83) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘jagsUI’
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘jagsUI’
    ```

# trackeR

Version: 1.1

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 13.7Mb
      sub-directories of 1Mb or more:
        data      2.1Mb
        doc       2.5Mb
        extdata   8.4Mb
    ```

# trialr

Version: 0.0.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        libs   6.3Mb
    ```

