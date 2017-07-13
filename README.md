# ggjoy
Geoms to make joy plots using ggplot2, written by Claus O. Wilke

**Note:** This is early alpha software. As of today (July 13, 2017), the package is only 4 days old. While things generally work, you may encounter problems. Also, I make no guarantees at this time that the api is stable. For feedback and feature requests, please open issues on github.

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
