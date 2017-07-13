## ------------------------------------------------------------------------
library(ggplot2)
library(ggjoy)

data <- data.frame(x = 1:5, y = rep(1, 5), height = c(0, 1, 3, 4, 2))
ggplot(data, aes(x, y, height = height)) + geom_ridgeline()


## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species, group=Species)) + geom_joy()

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species, group=Species)) + geom_joy2()

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species, group=Species)) + geom_joy() + theme_joy()

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species, group=Species)) + 
  geom_joy() + theme_joy() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0))

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species, group=Species)) + 
  geom_joy() + theme_joy(grid = FALSE) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0))

## ----message=FALSE, fig.width = 7.5, fig.height = 5----------------------
ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`)) +
  geom_joy(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  labs(title = 'Temperatures in Lincoln NE',
       subtitle = 'Mean temperatures (Fahrenheit) by month for 2016\nData: Original CSV from the Weather Underground') +
  theme_joy(font_size = 13, grid = T) + theme(axis.title.y = element_blank())

## ----message=FALSE, fig.width = 6, fig.height = 6------------------------
library(ggplot2movies)
ggplot(movies[movies$year>1912,], aes(x=length, y=year, group=year)) +
  geom_joy(scale=10, size=0.25, rel_min_height=0.03) +
  theme_joy() +
  scale_x_continuous(limits=c(1, 190), expand=c(0.01, 0)) +
  scale_y_reverse(breaks=c(2000, 1980, 1960, 1940, 1920, 1900), expand=c(0.01, 0))

