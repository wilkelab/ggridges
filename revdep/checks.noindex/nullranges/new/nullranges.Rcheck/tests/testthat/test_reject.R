library(nullranges)
test_that("rejection sampling works as expected", {

  suppressPackageStartupMessages(library(data.table))
  set.seed(123)
  dt <- data.table(
    looped = c(rep(TRUE, 500), rep(FALSE, 10000)),
    covar = c(rnorm(500, rep(c(1,4),each=250), 0.8),
              rnorm(10000, 2.5, 1.5))
  )

  ## library(ggplot2)
  ## ggplot(dt, aes(covar, after_stat(density), fill=looped)) +
  ##   geom_histogram(position="dodge")
  
  set.seed(123)
  m <- matchRanges(focal = dt[dt$looped],
                   pool = dt[!dt$looped],
                   covar = ~covar,
                   method = "rejection",
                   replace = FALSE)

  ## plotCovariate(m)

  expect_true(nrow(m) == sum(dt$looped))
  
})

test_that("zero probability issue solved", {

  suppressPackageStartupMessages(library(data.table))
  set.seed(123)
  dt <- data.table(
    looped = c(rep(TRUE, 500), rep(FALSE, 10000)),
    covar1 = c(rgamma(500, 2), abs(rnorm(10000, mean = 4, sd = 2))),
    covar2 = c(sample(letters[1:5],
                      size = 500,
                      replace = TRUE,
                      prob = c(0.1, 0.3, 0.4, 0.1, 0.05)),
               sample(letters[1:5],
                      size = 10000,
                      replace = TRUE,
                      prob = c(0.4, 0.3, 0.1, 0.1, 0.05)))
  )

  ## library(ggplot2)
  ## ggplot(dt, aes(covar1, fill=looped)) +
  ##   geom_histogram() +
  ##   facet_wrap(~covar2)
  
  set.seed(123)
  expect_error(matchRanges(focal = dt[dt$looped],
                           pool = dt[!dt$looped],
                           covar = ~covar1 + covar2,
                           method = "rejection",
                           replace = FALSE))

})
