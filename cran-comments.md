Apologies for submitting a bug-fix release 1 day after submitting the original package. The package currently on CRAN doesn't build on some architectures, due to failing visual tests: https://cran.r-project.org/web/checks/check_results_ggridges.html
Visual tests (via vdiffr) are known to be highly dependent on minor changes in the build platform and thus should be skipped whenever there are any changes. This should have happened automatically but apparently has not. So I have now manually disabled these tests when building on CRAN.

I have also fixed the problem with double spaces in the Description.

One data file contains UTF-8 marked strings. The file contains election results from Catalania,
and the municipality names contain non-ASCII characters. None of these names make it into the
example figure that is generated from this file, so it is best to leave the file as is
for completeness.

## Test environments
* R devel and R 3.4.1 on win-builder [ via devtools::build_win() ]
* local OS X install x86_64-apple-darwin15.6.0 (64-bit), R 3.4.1

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
None at this time.

