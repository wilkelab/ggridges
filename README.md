
<!-- README.md is generated from README.Rmd. Please edit that file -->
ggjoy
=====

[![Build Status](https://travis-ci.org/clauswilke/ggjoy.svg?branch=master)](https://travis-ci.org/clauswilke/ggjoy) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ggjoy)](https://CRAN.R-project.org/package=ggjoy) [![CRAN\_Downloads\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/ggjoy?color=brightgreen)](http://cranlogs.r-pkg.org/downloads/total/last-month/ggjoy)

Geoms to make joyplots using ggplot2, written by Claus O. Wilke

This package has now been officially relased on CRAN. Most things should work as expected, and the API should now be relatively stable. For feedback and feature requests, please open issues on github.

About joyplots
--------------

Joyplots are partially overlapping line plots that create the impression of a mountain range. They can be quite useful for visualizing changes in distributions over time or space. The name "joyplot" was [proposed](https://twitter.com/JennyBryan/status/856674638981550080) by Jenny Bryan on Twitter on April 24, 2017, in reference to the [iconic cover art](https://blogs.scientificamerican.com/sa-visual/pop-culture-pulsar-origin-story-of-joy-division-s-unknown-pleasures-album-cover-video/) for Joy Division's album *Unknown Pleasures*.

Installation
------------

Stable release:

``` r
install.packages("ggjoy")
```

Latest development version:

``` r
library(devtools)
install_github("clauswilke/ggjoy")
```

Usage
-----

``` r
library(ggplot2)
library(ggjoy)
    
ggplot(diamonds, aes(x = price, y = cut)) +
  geom_joy(scale = 4) + theme_joy() +
  scale_y_discrete(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0))      # for both axes to remove unneeded padding
#> Picking joint bandwidth of 458
```

![](man/figures/README-diamonds-1.png)

Documentation and Examples
--------------------------

First read the [package vignette.](https://cran.r-project.org/web/packages/ggjoy/vignettes/introduction.html) Then read the [reference manual.](https://cran.r-project.org/web/packages/ggjoy/ggjoy.pdf)
