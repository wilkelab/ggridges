# Setup

## Platform

|setting  |value                                              |
|:--------|:--------------------------------------------------|
|version  |R Under development (unstable) (2018-03-23 r74448) |
|system   |x86_64, darwin15.6.0                               |
|ui       |RStudio (1.0.153)                                  |
|language |(EN)                                               |
|collate  |en_US.UTF-8                                        |
|tz       |America/Chicago                                    |
|date     |2018-04-04                                         |

## Packages

|package  |*  |version    |date       |source                             |
|:--------|:--|:----------|:----------|:----------------------------------|
|ggplot2  |*  |2.2.1.9000 |2018-04-04 |Github (thomasp85/ggplot2@f1ba983) |
|ggridges |*  |0.5.0      |2018-04-04 |local (clauswilke/ggridges@NA)     |
|scales   |   |0.5.0.9000 |2018-04-04 |Github (hadley/scales@d767915)     |
|withr    |   |2.1.2      |2018-04-04 |Github (jimhester/withr@79d7b0d)   |

# Check results

3 packages with problems

|package    |version | errors| warnings| notes|
|:----------|:-------|------:|--------:|-----:|
|cRegulome  |0.1.1   |      1|        0|     0|
|HistDAWass |1.0.1   |      1|        0|     0|
|Seurat     |2.3.0   |      1|        0|     1|

## cRegulome (0.1.1)
Maintainer: Mahmoud Ahmed <mahmoud.s.fahmy@students.kasralainy.edu.eg>  
Bug reports: https://github.com/MahShaaban/cRegulome/issues

1 error  | 0 warnings | 0 notes

```
checking package dependencies ... ERROR
Packages required but not available:
  ‘AnnotationDbi’ ‘org.Hs.eg.db’ ‘clusterProfiler’

See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
manual.
```

## HistDAWass (1.0.1)
Maintainer: Antonio Irpino <antonio.irpino@unicampania.it>

1 error  | 0 warnings | 0 notes

```
checking whether package ‘HistDAWass’ can be installed ... ERROR
Installation failed.
See ‘/Users/wilke/github/ggridges/revdep/checks/HistDAWass.Rcheck/00install.out’ for details.
```

## Seurat (2.3.0)
Maintainer: Paul Hoffman <seuratpackage@gmail.com>  
Bug reports: https://github.com/satijalab/seurat/issues

1 error  | 0 warnings | 1 note 

```
checking whether package ‘Seurat’ can be installed ... ERROR
Installation failed.
See ‘/Users/wilke/github/ggridges/revdep/checks/Seurat.Rcheck/00install.out’ for details.

checking package dependencies ... NOTE
Packages suggested but not available for checking:
  ‘SummarizedExperiment’ ‘MAST’ ‘DESeq2’
```

