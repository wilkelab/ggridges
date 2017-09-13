## ------------------------------------------------------------------------
library(ggplot2)
library(ggridges)

data <- data.frame(x = 1:5, y = rep(1, 5), height = c(0, 1, 3, 4, 2))
ggplot(data, aes(x, y, height = height)) + geom_ridgeline()


## ----message=FALSE, fig.width=9, fig.height=3----------------------------
# for side-by-side plotting
library(gridExtra)

data <- data.frame(x = 1:5, y = rep(1, 5), height = c(0, 1, -1, 3, 2))
plot_base <- ggplot(data, aes(x, y, height = height))
grid.arrange(plot_base + geom_ridgeline(),
          plot_base + geom_ridgeline(min_height = -2), ncol = 2)

## ------------------------------------------------------------------------
d <- data.frame(x = rep(1:5, 3), y = c(rep(0, 5), rep(1, 5), rep(2, 5)),
                height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1))
ggplot(d, aes(x, y, height = height, group = y)) + geom_ridgeline(fill = "lightblue")

## ------------------------------------------------------------------------
ggplot(d, aes(x, y, height = height, group = y)) + 
  geom_density_ridges(stat = "identity", scale = 1)

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges()

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges2()

## ----message=FALSE-------------------------------------------------------
# modified dataset that represents species as a number
iris_num <- transform(iris, Species_num = as.numeric(Species))

# does not work, causes error
# ggplot(iris_num, aes(x = Sepal.Length, y = Species)) + geom_density_ridges()

# works 
ggplot(iris_num, aes(x = Sepal.Length, y = Species_num, group = Species_num)) + geom_density_ridges()

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges(rel_min_height = 0.01)

## ----message=FALSE-------------------------------------------------------
# scale = 0.9, not quite touching
ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges(scale = 0.9)
# scale = 1, exactly touching
ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges(scale = 1)
# scale = 5, substantial overlap
ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges(scale = 5)

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
  geom_density_ridges(scale = 1) + facet_wrap(~Species)

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges() + theme_ridges()

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
  geom_density_ridges() + theme_ridges() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0))

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
  geom_density_ridges() + theme_ridges(grid = FALSE) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0))

## ----message=FALSE-------------------------------------------------------
 ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
   geom_density_ridges() + 
   theme_minimal(base_size = 14) + theme(axis.text.y = element_text(vjust = 0)) +
   scale_x_continuous(expand = c(0.01, 0)) +
   scale_y_discrete(expand = c(0.01, 0))

## ----message=FALSE-------------------------------------------------------
 ggplot(diamonds, aes(x = price, y = cut, fill = cut)) + 
   geom_density_ridges(scale = 4) + 
   scale_fill_cyclical(values = c("blue", "green"))

## ----message=FALSE, fig.width = 5.5--------------------------------------
 ggplot(diamonds, aes(x = price, y = cut, fill = cut)) + 
   geom_density_ridges(scale = 4) + 
   scale_fill_cyclical(values = c("blue", "green"), guide = "legend")

## ----message=FALSE, fig.width = 5.5--------------------------------------
 ggplot(diamonds, aes(x = price, y = cut, fill = cut)) + 
   geom_density_ridges(scale = 4) + 
   scale_fill_cyclical(values = c("blue", "green"), guide = "legend",
                       labels = c("Fair" = "blue", "Good" = "green"),
                       name = "Fill colors")

## ----message=FALSE, fig.width = 6.5--------------------------------------
 ggplot(diamonds, aes(x = price, y = cut, fill = cut, color = cut)) + 
   geom_density_ridges(scale = 4, size = 1) + 
   scale_fill_cyclical(values = c("blue", "green"), guide = "legend",
                       labels = c("Fair" = "blue w/ black outline",
                                  "Good" = "green w/ yellow outline"),
                       name = "Color scheme") +
   scale_color_cyclical(values = c("black", "yellow"), guide = "legend",
                       labels = c("Fair" = "blue w/ black outline",
                                  "Good" = "green w/ yellow outline"),
                       name = "Color scheme")

## ----message=FALSE, fig.width = 6.5--------------------------------------
ggplot(mpg, aes(x = class, fill = class, color = class)) + 
  geom_bar(size = 1.5) +
  scale_fill_cyclical(values = c("blue", "green"), guide = "legend",
                      labels = c("blue w/ black outline", "green w/ yellow outline"),
                      name = "Color scheme") +
  scale_color_cyclical(values = c("black", "yellow"), guide = "legend",
                      labels = c("blue w/ black outline", "green w/ yellow outline"),
                      name = "Color scheme")

## ----message=FALSE, fig.width=5.5----------------------------------------
library(tidyverse)
mpg %>% group_by(class) %>% tally() %>% arrange(desc(n)) %>%
  mutate(class = factor(class, levels=class)) %>%
  ggplot(aes(x = class, y = n, fill = class)) + 
    geom_col() + theme_minimal() +
    scale_fill_cyclical(values = c("#4040B0", "#9090F0")) +
    scale_y_continuous(expand = c(0, 0))

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species, height = ..density..)) + 
  geom_density_ridges(stat = "density")

## ----message=FALSE-------------------------------------------------------
library(tidyverse)
iris %>% group_by(Species) %>%
  do(ggplot2:::compute_density(.$Sepal.Length, NULL)) %>%
  rename(Sepal.Length = x) -> iris_densities
head(iris_densities)

## ----message=FALSE-------------------------------------------------------
ggplot(iris_densities, aes(x = Sepal.Length, y = Species, height = density)) + 
  geom_density_ridges(stat = "identity")

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species, height = ..density..)) + 
  geom_density_ridges(stat = "binline", bins = 20, scale = 0.95, draw_baseline = FALSE)

