#' Weather in Lincoln, Nebraska in 2016.
#'
#' A dataset containing weather information from Lincoln, Nebraska, from 2016.
#' Originally downloaded from Weather Underground by Austin Wehrwein, http://austinwehrwein.com/.
#' The variables are listed below. Most are self-explanatory. Max, mean, and min measurements are
#' calculated relative to the specific day of measurement.
#'
#' @format A tibble with 366 rows and 24 variables:
#' \describe{
#'   \item{`CST`}{Day of the measurement}
#'   \item{`Max Temperature [F]`}{}
#'   \item{`Mean Temperature [F]`}{}
#'   \item{`Min Temperature [F]`}{}
#'   \item{`Max Dew Point [F]`}{}
#'   \item{`Mean Dew Point [F]`}{}
#'   \item{`Min Dewpoint [F]`}{}
#'   \item{`Max Humidity`}{}
#'   \item{`Mean Humidity`}{}
#'   \item{`Min Humidity`}{}
#'   \item{`Max Sea Level Pressure [In]`}{}
#'   \item{`Mean Sea Level Pressure [In]`}{}
#'   \item{`Min Sea Level Pressure [In]`}{}
#'   \item{`Max Visibility [Miles]`}{}
#'   \item{`Mean Visibility [Miles]`}{}
#'   \item{`Min Visibility [Miles]`}{}
#'   \item{`Max Wind Speed [MPH]`}{}
#'   \item{`Mean Wind Speed[MPH]`}{}
#'   \item{`Max Gust Speed [MPH]`}{}
#'   \item{`Precipitation [In]`}{}
#'   \item{`CloudCover`}{}
#'   \item{`Events`}{Specific weather events, such as rain, snow, or fog}
#'   \item{`WindDir [Degrees]`}{}
#'   \item{`Month`}{The month in which the measurement was taken}
#' }
"lincoln_weather"

#' Results from Catalan regional elections (1980-2015)
#'
#' Data from Catalan regional elections for 949 municipalities, from 11 elections spanning the years
#' 1980-2015. The data was obtained and processed from Idescat.cat by Marc Belzunces (Twitter: @marcbeldata).
#' @format A tibble with 20764 rows and 4 variables:
#' \describe{
#'     \item{`Municipality`}{}
#'     \item{`Year`}{}
#'     \item{`Option`}{The voter option; either "Indy" or "Unionist"}
#'     \item{`Percent`}{The percentage of the voters choosing the given option}
#' }
"Catalan_elections"

#' Australian athletes
#'
#' This dataset is equivalent to `ais` from the `DAAG` package.
#'
#' @references
#' Telford, R.D. and Cunningham, R.B. 1991. Sex, sport and body-size dependency of hematology in
#' highly trained athletes. Medicine and Science in Sports and Exercise 23: 788-794.
#'
#' @examples
#' # none yet
"Aus_athletes"
