# context("plotDist()")
require(mosaicData)
testthat::test_that("plotDist works", {
  wrapped_expect_doppelganger("plotDist1", plotDist('norm'))
  wrapped_expect_doppelganger("plotDist2", plotDist('norm', type='h'))
  wrapped_expect_doppelganger("plotDist3", plotDist('norm', kind='cdf'))
  wrapped_expect_doppelganger("plotDist4", plotDist('exp',  kind='histogram'))
  wrapped_expect_doppelganger("plotDist5", plotDist('binom', params=list( 25, .25)))
  wrapped_expect_doppelganger("plotDist6", plotDist('binom', 25, .25))
  wrapped_expect_doppelganger("plotDist7", plotDist('norm', mean=100, sd=10, kind='cdf'))
  wrapped_expect_doppelganger("plotDist8", plotDist('binom', params=list( 25, .25), kind='cdf'))
  wrapped_expect_doppelganger("plotDist9", plotDist('beta', params=list( 3, 10), kind='density'))
  wrapped_expect_doppelganger("plotDist10", plotDist('beta', params=list( 3, 10), kind='cdf'))
  wrapped_expect_doppelganger("plotDist11", plotDist( "binom", params=list(35,.25),
                                                     groups= y < dbinom(qbinom(0.05, 35, .25), 35,.25) ))
  wrapped_expect_doppelganger("plotDist12", plotDist( "binom", params=list(35,.25),
                                                     groups= y < dbinom(qbinom(0.05, 35, .25), 35,.25),
                                                     kind='hist'))
  wrapped_expect_doppelganger("plotDist13", plotDist("norm", mean=10, sd=2, col="blue", type="h"))
  wrapped_expect_doppelganger("plotDist14", plotDist("norm", mean=12, sd=2, col="red", type="h", under=TRUE))
  wrapped_expect_doppelganger("plotDist15", plotDist("binom", size=100, prob=.30) +
                                plotDist("norm", mean=30, sd=sqrt(100 * .3 * .7)))
  
  
  
  wrapped_expect_doppelganger("plotDist16", plotDist("chisq", df=4, groups = x > 6, type="h"))
  wrapped_expect_doppelganger("plotDist17", plotDist("f", df1=1, df2 = 99))
  
  histogram( ~age|sex, data=HELPrct)
  m <- mean( ~age|sex, data=HELPrct)
  s <- sd(~age|sex, data=HELPrct)
  
  wrapped_expect_doppelganger("plotDist18", plotDist( "norm", mean=m[1], sd=s[1], col="red", add=TRUE, packets=1))
  wrapped_expect_doppelganger("plotDist19", plotDist( "norm", mean=m[2], sd=s[2], col="blue", under=TRUE, packets=2))
})


















