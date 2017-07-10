# ggjoy
Geoms to make joy plots using ggplot2, written by Claus O. Wilke


## Installation

    library(devtools)
    install_github("clauswilke/ggjoy")

## Usage

    library(ggplot2)
    library(ggjoy)
    ggplot(diamonds, aes(x=price, y=cut, group=cut, height=..density..)) +
         geom_joy(scale=4) +
         scale_y_discrete(expand=c(0,0)) +
         scale_x_continuous(expand=c(0,0))
 
