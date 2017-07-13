require(readr)

lincoln_weather <- read_csv("data-raw/lincoln-weather.csv")
lincoln_weather$Month<-months(as.Date(lincoln_weather$CST))
lincoln_weather$Month<-factor(lincoln_weather$Month, levels=rev(unique(lincoln_weather$Month)))

devtools::use_data(lincoln_weather, overwrite = TRUE)
