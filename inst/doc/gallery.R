## ----echo=FALSE, include=FALSE-------------------------------------------
library(ggplot2)
library(ggjoy)

## ----message=FALSE, fig.width = 7.5, fig.height = 5----------------------
ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`)) +
  geom_joy(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  labs(title = 'Temperatures in Lincoln NE',
       subtitle = 'Mean temperatures (Fahrenheit) by month for 2016\nData: Original CSV from the Weather Underground') +
  theme_joy(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())

## ----message=FALSE, warning=FALSE, fig.width = 6, fig.height = 6---------
library(ggplot2movies)
ggplot(movies[movies$year>1912,], aes(x = length, y = year, group = year)) +
  geom_joy(scale = 10, size = 0.25, rel_min_height = 0.03) +
  theme_joy() +
  scale_x_continuous(limits=c(1, 200), expand = c(0.01, 0)) +
  scale_y_reverse(breaks=c(2000, 1980, 1960, 1940, 1920, 1900), expand = c(0.01, 0))

## ----message=FALSE, warning=FALSE, fig.width = 6, fig.height = 8---------
library(tidyverse)
library(forcats)
Catalan_elections %>%
  mutate(Alt = (as.numeric(as.factor(Year))-1) %% 2 + 1) %>%
  ggplot(aes(y = as.factor(Year) %>% fct_rev())) +
  geom_joy(aes(x = Percent, fill = paste(Option, Alt)), 
           alpha = .8, color = "white", from = 0, to = 100) +
  labs(x = "Vote (%)",
       y = "Election Year",
       title = "Indy vs Unionist vote in Catalan elections",
       subtitle = "Analysis unit: municipalities (n = 949)",
       caption = "Marc Belzunces (@marcbeldata) | Source: Idescat") +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_manual(breaks = c("Indy 1", "Unionist 1"),
                    labels = c(`Indy 1` = "Indy", `Unionist 1` = "Unionist"),
                    values = c("#ff0000", "#ff8080", "#0000ff", "#8080ff"),
                    name = "Option") +
  theme_joy(grid = FALSE)

