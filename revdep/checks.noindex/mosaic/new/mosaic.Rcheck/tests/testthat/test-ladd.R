# context("ladd()")

testthat::test_that("basic ladd works", {
  set.seed(5)
  p <- xyplot(rnorm(100) ~rnorm(100))
  p
  wrapped_expect_doppelganger("ladd1", ladd(panel.abline(a=0,b=1)))
  wrapped_expect_doppelganger("ladd2", ladd(panel.abline(h=0,col='blue')))
  wrapped_expect_doppelganger("ladd3", ladd(grid.text('Hello')))
  wrapped_expect_doppelganger("ladd4", ladd(grid.text(x=.95,y=.05,'text here',just=c('right','bottom'))))
})



testthat::test_that("ladd with factors works", {
  set.seed(6)
  q <- xyplot(rnorm(100) ~rnorm(100)|factor(rbinom(100,4,.5)))
  q <- update(q, layout=c(3,2))
  q
  wrapped_expect_doppelganger("ladd5", ladd(panel.abline(a=0,b=1), plot=q))
  wrapped_expect_doppelganger("ladd6", ladd(panel.abline(h=0,col='blue')))
  wrapped_expect_doppelganger("ladd7", ladd( grid.text("(2,1)",gp=gpar(cex=3,alpha=.5)), columns=2, rows=1))
  wrapped_expect_doppelganger("ladd8", ladd( grid.text("p5",gp=gpar(cex=3,alpha=.5)), packets=5))
})

# Refresh the plot
q
ladd( grid.text(paste(current.column(), current.row(),sep=','), gp=gpar(cex=3,alpha=.5)) )
histogram( ~eruptions, data=faithful )
testthat::test_that("ladd under works", {
  # over would probably be better here, but the demonstrates what under=TRUE does.
  wrapped_expect_doppelganger("ladd9", ladd(panel.densityplot(faithful$eruptions, lwd=4), under=TRUE))
})
