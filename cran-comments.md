This is a minor version increase for ggjoy, providing numerous improvements and new features. It also adds
a missing import statement that wasn't flagged by CRAN check for some reason.

One data file contains UTF-8 marked strings. The file contains election results from Catalania,
and the municipality names contain non-ASCII characters. None of these names make it into the
example figure that is generated from this file, so I think it is best to leave the file as is
for completeness.

## Test environments
* R devel and R 3.4.1 on win-builder [ via devtools::build_win() ]
* local OS X install x86_64-apple-darwin15.6.0 (64-bit), R 3.4.1

## R CMD check results
There were no ERRORs or WARNINGs.

There is one NOTE, about spelling:
Possibly mis-spelled words in DESCRIPTION:
  Joyplots (3:8, 8:14)

The spelling is correct.

## Downstream dependencies
None at this time.

