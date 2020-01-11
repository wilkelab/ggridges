library(dplyr)
library(here)

# originally taken drom DAAG package, which has been archived on CRAN
# as of 2020-01-11
ais <- read.csv(here("data-raw", "Aus_athletes.csv"))

ais %>% mutate(sp = as.character(sport)) %>%
  mutate(sport = case_when(sp=="B_Ball" ~ "basketball",
                           sp=="Field" ~ "field",
                           sp=="Gym" ~ "gymnastics",
                           sp=="Netball" ~ "netball",
                           sp=="Row" ~ "rowing",
                           sp=="Swim" ~ "swimming",
                           sp=="T_400m" ~ "track (400m)",
                           sp=="T_Sprnt" ~ "track (sprint)",
                           sp=="Tennis" ~ "tennis",
                           sp=="W_Polo" ~ "water polo")) %>%
  select(-sp) %>%
  rename(height = ht,
         weight = wt) -> Aus_athletes

usethis::use_data(Aus_athletes, overwrite = TRUE)
