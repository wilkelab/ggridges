# ggjoy
Geoms to make joyplots using ggplot2, written by Claus O. Wilke

**Note:** This is early alpha software. As of today (July 13, 2017), the package is only 4 days old. While things generally work, you may encounter problems. Also, I make no guarantees at this time that the api is stable. For feedback and feature requests, please open issues on github.

## About joyplots

Joyplots are partially overlapping line plots that create the impression of a mountain range. They can be quite useful for visualizing changes in distributions over time or space. The name "joyplot" was [proposed](
https://twitter.com/JennyBryan/status/856674638981550080) by Jenny Bryan on Twitter on April 24, 2017, in memory of the band Joy Division whose 1979 album Unknown Pleasures contained such a plot on its cover. 

## Installation

    library(devtools)
    install_github("clauswilke/ggjoy")

## Usage

    library(ggplot2)
    library(ggjoy)
    
    ggplot(diamonds, aes(x=price, y=cut, group=cut, height=..density..)) +
         geom_joy(scale=4) +
         scale_y_discrete(expand=c(0.01, 0)) +
         scale_x_continuous(expand=c(0, 0)) + theme_joy()
 
 ## Documentation and Examples
 
     library(ggjoy)
     ?geom_joy
     ?geom_ridgeline
     ?theme_joy
     
 More to come.
