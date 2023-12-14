# context("rfun()")

testthat::test_that("rfun works", {
  f <- rfun( ~ u & v)
  testcase <- function (u, v) 
  {
    x <- eval(parse(text = varnames[1]))
    y <- eval(parse(text = varnames[2]))
    res <- 0
    for (k in 1:nmaxes) {
      res <- res + signsmax[k] * exp(-(xscales[k] * (x - locs[[1]][k])^2 + 
                                         (y - locs[[2]][k])^2)/9)
    }
    return(res)
  }
  
  wrapped_expect_doppelganger("rfun1", plotFun(f(u,v)~u&v,u=range(-5,5),v=range(-5,5)))
  
  myfun <- rfun( ~ u & v, seed = 1959)
  g <- rpoly2( ~ x & y & z, seed = 1964)
  
  wrapped_expect_doppelganger("rfun2", plotFun(g(x,y,z=2)~x&y,xlim=range(-5,5),ylim=range(-5,5)))
})






