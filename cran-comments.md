One data file contains UTF-8 marked strings. The file contains election results from Catalania, and the municipality names contain non-ASCII characters. None of these names make it into the example figure that is generated from this file, so it is best to leave the file as is for completeness.

## Test environments
* R devel and R 3.5.1 on win-builder [ via devtools::build_win() ]
* local OS X install x86_64-apple-darwin15.6.0 (64-bit), R 3.5.0

## R CMD check results
There were no ERRORs or WARNINGs. There was one NOTE, about possible misspelling. The spelling is correct.

## Downstream dependencies
Downstream dependencies seem fine, according to revdepcheck::revdep_check().
There are no new problems caused by the new package version.

Full results available at https://github.com/clauswilke/ggridges/blob/master/revdep/
