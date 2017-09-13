This packages is meant as a replacement for the package 'ggjoy' currently on CRAN. I have changed the name because the term "joyplot" has an unfortunate history. It comes from the band Joy Division, which was named after Nazi concentration camp brothels (see here: https://en.wikipedia.org/wiki/House_of_Dolls ). In the 'ggridges' package, all references to "joyplots" have been changed into "ridgeline plots", and functions such as 'geom_joy' have been renamed into appropriate terms (e.g., 'geom_density_ridges').

Once 'ggridges' is on CRAN, I will replace the 'ggjoy' package with a placeholder package that simply exports the 'ggridges' functions under the old 'ggjoy' name and warns users that 'ggjoy' is deprecated.

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

