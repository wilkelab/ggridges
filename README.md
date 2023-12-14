
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggridges: Ridgeline plots in ggplot2

<!-- badges: start -->

[![R build
status](https://github.com/wilkelab/ggridges/workflows/R-CMD-check/badge.svg)](https://github.com/wilkelab/ggridges/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/wilkelab/ggridges/master.svg)](https://app.codecov.io/github/wilkelab/ggridges?branch=master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ggridges)](https://CRAN.R-project.org/package=ggridges)
[![CRAN_Downloads_Badge](https://cranlogs.r-pkg.org/badges/ggridges)](https://cranlogs.r-pkg.org/downloads/total/last-month/ggridges)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
<!-- badges: end -->

Ridgeline plots are partially overlapping line plots that create the
impression of a mountain range. They can be quite useful for visualizing
changes in distributions over time or space.

## Installation

Please install the stable release from CRAN:

``` r
install.packages("ggridges")
```

Alternatively, you can install the latest development version from
github:

``` r
remotes::install_github("wilkelab/ggridges")
```

## Usage

``` r
library(ggplot2)
library(ggridges)
    
ggplot(diamonds, aes(x = price, y = cut)) +
  geom_density_ridges(scale = 4) + 
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
  theme_ridges()
#> Picking joint bandwidth of 458
```

![](man/figures/README-diamonds-1.png)<!-- -->

## Documentation and Examples

First read the [package
vignette.](https://wilkelab.org/ggridges/articles/introduction.html)
Then read the [reference
manual.](https://wilkelab.org/ggridges/reference/index.html)
