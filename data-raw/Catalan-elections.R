library(tidyverse)
library(stringr)

# Data obtained and processed from Idescat.cat
# by Marc Belzunces (Twitter: @marcbeldata)
df <- read_csv("data-raw/Catalan_elections.csv")

Catalan_elections <- gather(df, key=Option, value=Percent, -Municipality, -Year) %>%
  mutate(Option = str_replace(Option, "_percent", "")) %>%
  mutate(Option = str_replace(Option, "Unio", "Unionist"))

devtools::use_data(Catalan_elections, overwrite = TRUE)
