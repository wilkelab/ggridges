
# context('dpqrdist()')

test_that("dpqrdist works for normal dist", {
  expect_equal(ignore_attr = TRUE, 
    dpqrdist("norm", "d", x = c(0,1,2)), 
    dnorm(c(0,1,2))
  )
  expect_equal(ignore_attr = TRUE, 
    dpqrdist("norm", "d", x = c(0,1,2), mean=10, sd=2),
    dnorm(c(0,1,2), mean=10, sd=2) 
  )
  expect_equal(ignore_attr = TRUE, 
    dpqrdist("norm", "p", q = c(0,1,2)), 
    pnorm(c(0,1,2))
  )
  expect_equal(ignore_attr = TRUE, 
    dpqrdist("norm", "p", q = c(0,1,2), mean=10, sd=2),
    pnorm(c(0,1,2), mean=10, sd=2) 
  )
  expect_equal(ignore_attr = TRUE, 
    dpqrdist("norm", "q", p = c(.1,.2,.3)),
    qnorm(c(.1,.2,.3))
  ) 
  expect_equal(ignore_attr = TRUE, 
    dpqrdist("norm", "q", p = c(.1,.2,.3), mean=10, sd=2),
    qnorm(c(.1,.2,.3), mean=10, sd=2) 
  )    
})

test_that("dpqrdist works for t dist", {
   expect_equal(ignore_attr = TRUE, 
     dpqrdist("t", "d", x = c(0,1,2), df=10), 
     dt(c(0,1,2), df=10)
   )
  expect_equal(ignore_attr = TRUE, 
    dpqrdist("t", "p", q = c(0,1,2), df=10), 
    pt(c(0,1,2), df=10)
  )
  expect_equal(ignore_attr = TRUE, 
    dpqrdist("t", "q", p = c(.1,.2,.3), df=10), 
    qt(c(.1,.2,.3), df=10)
  )
})

test_that("dpqrdist works for binomial dist", {
  expect_equal(ignore_attr = TRUE, 
    dpqrdist("binom", "d", x = c(0,1,2), size=10, prob=0.4), 
    dbinom(c(0,1,2), size=10, prob=0.4)
  )
  expect_equal(ignore_attr = TRUE, 
    dpqrdist("binom", "p", q = c(0,1,2), size=10, prob=0.4), 
    pbinom(c(0,1,2), size=10, prob=0.4)
  )
  expect_equal(ignore_attr = TRUE, 
    dpqrdist("binom", "q", p = c(.25, .5, .75), size=10, prob=0.4), 
    qbinom(c(.25, .5, .75), size=10, prob=0.4)
  )
})

# test_that("pdist works", {
#   #pdist("norm", -2:2) |> dput()
#   # testcase <- c(0.0227501319481792, 0.158655253931457, 0.5, 0.841344746068543, 
#   #               0.977249868051821)
#   # test <- pdist("norm", -2:2)
#   # expect_equal(ignore_attr = TRUE, test, testcase)
#   #wrapped_expect_doppelganger("pdist1", test <- pdist("norm", -2:2))
#   #pdist("norm", seq(80,120, by = 10), mean = 100, sd = 10)
#   # pdist("chisq", 2:4, df = 3)
#   # pdist("f", 1, df1 = 2, df2 = 10)
#   # pdist("gamma", 2, shape = 3, rate = 4)
# })
