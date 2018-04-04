One data file contains UTF-8 marked strings. The file contains election results from Catalania, and the municipality names contain non-ASCII characters. None of these names make it into the example figure that is generated from this file, so it is best to leave the file as is for completeness.

## Test environments
* R devel and R 3.4.4 on win-builder [ via devtools::build_win() ]
* local OS X install x86_64-apple-darwin15.6.0 (64-bit), R devel (3.5.0)

## R CMD check results
There were no ERRORs or WARNINGs. There was one NOTE, about possible misspelling. The spelling is correct.

## Downstream dependencies
Downstream dependencies seem fine, according to devtools::revdep_check().
The packages with errors failed at the installation stage,
not because of ggridges.

Full results available at https://github.com/clauswilke/ggridges/blob/master/revdep/README.md

|package             |version | errors| warnings| notes|
|:-------------------|:-------|------:|--------:|-----:|
|bayesplot           |1.5.0   |      0|        0|     1|
|CAISEr              |0.2.1   |      0|        0|     0|
|cRegulome           |0.1.1   |      1|        0|     0|
|ggjoy               |0.4.0   |      0|        0|     0|
|HistDAWass          |1.0.1   |      1|        0|     0|
|jmv                 |0.8.6   |      0|        0|     0|
|scatr               |1.0.1   |      0|        0|     0|
|Seurat              |2.3.0   |      1|        0|     1|
|sjPlot              |2.4.1   |      0|        0|     1|
|userfriendlyscience |0.7.0   |      0|        0|     0|
