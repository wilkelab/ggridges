Apologies for submitting a bug-fix release 1 day after submitting the original package. The package currently on CRAN doesn't build on some architectures, due to visual tests that are likely to fail due to minor changes in the build platform: https://cran.r-project.org/web/checks/check_results_ggridges.html

These tests should be automatically skipped but apparently that isn't happening. So I'm now manually disabling them on CRAN.

I have also fixed the problem with double spaces in the Description.

One data file contains UTF-8 marked strings. The file contains election results from Catalania,
and the municipality names contain non-ASCII characters. None of these names make it into the
example figure that is generated from this file, so it is best to leave the file as is
for completeness.

## Test environments
* R devel and R 3.4.1 on win-builder [ via devtools::build_win() ]
* local OS X install x86_64-apple-darwin15.6.0 (64-bit), R 3.4.1

## R CMD check results
There were no ERRORs or WARNINGs.

There was one Note:
Possibly mis-spelled words in DESCRIPTION:
  Ridgeline (3:8, 8:14)

The spelling is correct.

## Downstream dependencies
None at this time.

