## ------------------------------------------------------------------------
library(ggplot2)
library(ggridges)

data <- data.frame(x = 1:5, y = rep(1, 5), height = c(0, 1, 3, 4, 2))
ggplot(data, aes(x, y, height = height)) + geom_ridgeline()


## ----message = FALSE, fig.width=9, fig.height=3--------------------------
# for side-by-side plotting
library(gridExtra)

data <- data.frame(x = 1:5, y = rep(1, 5), height = c(0, 1, -1, 3, 2))
plot_base <- ggplot(data, aes(x, y, height = height))
grid.arrange(plot_base + geom_ridgeline(),
          plot_base + geom_ridgeline(min_height = -2), ncol = 2)

## ----message = FALSE-----------------------------------------------------
d <- data.frame(x = rep(1:5, 3), y = c(rep(0, 5), rep(1, 5), rep(2, 5)),
                height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1))
ggplot(d, aes(x, y, height = height, group = y)) + 
  geom_ridgeline(fill = "lightblue")

## ----message = FALSE-----------------------------------------------------
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
ggplot(iris_num, aes(x = Sepal.Length, y = Species_num, group = Species_num)) + 
  geom_density_ridges()

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
  geom_density_ridges(rel_min_height = 0.01)

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

## ----message = FALSE-----------------------------------------------------
library(viridis)
d <- data.frame(x = rep(1:5, 3) + c(rep(0, 5), rep(0.3, 5), rep(0.6, 5)),
                y = c(rep(0, 5), rep(1, 5), rep(3, 5)),
                height = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1))
ggplot(d, aes(x, y, height = height, group = y, fill = factor(x+y))) +
  geom_ridgeline_gradient() +
  scale_fill_viridis(discrete = TRUE, direction = -1, guide = "none")

## ----message = FALSE-----------------------------------------------------
ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016')

## ----message = FALSE-----------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species)) +
  stat_density_ridges(quantile_lines = TRUE)

## ----message = FALSE-----------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)

## ----message = FALSE-----------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.025, 0.975), alpha = 0.7)

## ----message = FALSE-----------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = 4, quantile_lines = TRUE) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")

## ----message = FALSE-----------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.025, 0.975)) +
  scale_fill_manual(name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]"))

## ----message = FALSE-----------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species, fill=0.5 - abs(0.5-..ecdf..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis(name = "Tail probability", direction = -1)

## ----message = FALSE-----------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species)) +
  geom_density_ridges(jittered_points = TRUE)

## ----message = FALSE-----------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species)) +
  geom_density_ridges(jittered_points = TRUE, position = "raincloud",
                      alpha = 0.7, scale = 0.9)

## ----message = FALSE-----------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species)) +
  geom_density_ridges(jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|', point_size = 3, alpha = 0.7)

## ----message = FALSE-----------------------------------------------------
ggplot(iris, aes(x=Sepal.Length, y=Species, fill = Species)) +
  geom_density_ridges(aes(point_color = Species, point_fill = Species,
                          point_shape = Species),
                      alpha = .2, jittered_points = TRUE) +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23))

## ----message = FALSE, fig.width = 6, fig.height = 6----------------------
ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
  geom_density_ridges(aes(point_shape = Species, point_fill = Species,
                          point_size = Petal.Length), 
                      alpha = .2, jittered_points = TRUE) +
  scale_point_color_hue(l = 40) + scale_point_size_continuous(range = c(0.5, 4)) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(21, 22, 23))

## ----message = FALSE-----------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_density_ridges(jittered_points = TRUE, quantile_lines = TRUE, scale = 0.9, alpha = 0.7,
                      vline_size = 1, vline_color = "red", point_size = 0.4,
                      position = position_raincloud(adjust_vlines = TRUE))

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species, height = ..density..)) + 
  geom_density_ridges(stat = "density")

## ----message=FALSE-------------------------------------------------------
library(dplyr)
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

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges() + theme_ridges()

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
  geom_density_ridges() + theme_ridges() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0))

## ----message=FALSE-------------------------------------------------------
ggplot(iris, aes(x = Sepal.Length, y = Species)) + 
  geom_density_ridges() + theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
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
mpg %>% group_by(class) %>% tally() %>% arrange(desc(n)) %>%
  mutate(class = factor(class, levels=class)) %>%
  ggplot(aes(x = class, y = n, fill = class)) + 
    geom_col() + theme_minimal() +
    scale_fill_cyclical(values = c("#4040B0", "#9090F0")) +
    scale_y_continuous(expand = c(0, 0))

