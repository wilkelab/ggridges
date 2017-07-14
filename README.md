# ggjoy
Geoms to make joyplots using ggplot2, written by Claus O. Wilke

**Note:** This is early alpha software. As of today (July 13, 2017), the package is only 4 days old. While things generally work, you may encounter problems. Also, I make no guarantees at this time that the api is stable. For feedback and feature requests, please open issues on github.

## About joyplots

Joyplots are partially overlapping line plots that create the impression of a mountain range. They can be quite useful for visualizing changes in distributions over time or space. The name "joyplot" was [proposed](
https://twitter.com/JennyBryan/status/856674638981550080) by Jenny Bryan on Twitter on April 24, 2017, in reference to the [iconic cover art](https://blogs.scientificamerican.com/sa-visual/pop-culture-pulsar-origin-story-of-joy-division-s-unknown-pleasures-album-cover-video/) for Joy Division's album _Unknown Pleasures_.

## Installation

    library(devtools)
    install_github("clauswilke/ggjoy")

## Usage

    library(ggplot2)
    library(ggjoy)
    
    ggplot(diamonds, aes(x=price, y=cut, group=cut)) +
         geom_joy(scale=4) + theme_joy() +
         scale_y_discrete(expand=c(0.01, 0)) +   # will generally have to set the `expand` option
         scale_x_continuous(expand=c(0, 0))      # for both axes to remove unneeded padding
 
 ## Documentation and Examples
 
     library(ggjoy)
     ?geom_joy
     ?geom_ridgeline
     ?theme_joy
     
A vignette is [available here.](https://htmlpreview.github.io/?https://github.com/clauswilke/ggjoy/blob/master/inst/doc/introduction.html)
