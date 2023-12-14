# context('Finding Zeros')

test_that("zeros are found", {
  expect_equal(ignore_attr = TRUE,  round(findZeros(x^2 - 2 ~x)[,"x"],4), round(c(-sqrt(2), sqrt(2)),4) )
  expect_equal(ignore_attr = TRUE,  round(findZeros(x^2 - 3 ~x)[,"x"],4), round(c(-sqrt(3), sqrt(3)),4) )
})

test_that("zeros are within search interval", {
  expect_true( all( abs(findZeros(sin(1/x) ~ x, near=0, within=4)) < 4) )
})

test_that("Can find zeros in two variables",{
  Z = findZerosMult(a*x^2-v~a&x, v=8)
  expect_equal(Z[1,]$a*Z[1,]$x^2-8, 0, tolerance = 0.001)
  
  Z = findZerosMult(a^2+x^2-8~a&x, npts = 1000, sortBy='radial')
  expect_equal(Z[46,]$a^2+Z[46,]$x^2-8, 0, tolerance = 0.001)
})

test_that("Works with Broyden",{
  Z = findZeros(x*y+z^2~z&y&z, z+y~x&y&z, npts=10)
  x = Z[,"x"]
  y = Z[,"y"]
  z = Z[,"z"]
  expect_equal(x*y+z^2, rep(0, 10), tolerance = 0.001)
  expect_equal(z+y, rep(0, 10), tolerance = 0.001)
  
  Z = findZeros(x*y+z^2+z^2*w~w&x&y&z, w*z+y~w&x&y&z, npts=10)
  w = Z[,"w"]
  x = Z[,"x"]
  y = Z[,"y"]
  z = Z[,"z"]
  expect_equal(x*y+z^2+z^2*w, rep(0,10), tolerance = 0.001)
  expect_equal(w*z+y, rep(0,10), tolerance = 0.001)
  
  Z = findZeros(x*y^2-9+z~x&y&z, x*y*z+z*w~x&y&z, w=10)
  x = Z[,"x"]
  y = Z[,"y"]
  z = Z[,"z"]
  w = 10
  expect_equal(x*y^2-9+z, rep(0, 10), tolerance = 0.001)
  expect_equal(x*y*z+z*w, rep(0,10), tolerance = 10)
  
})

# These were taken out by DTK on Aug 10, since they are somewhat stochastic.
# test_that("Works properly with different centers", {
#   Z = findZeros((x-500)^2+ (y-200)^2 + (z-300)^2 - 25~x&y&z, near = c(x= 500, y=200, z=300), nearest = 1000, within = 25)
#   x = Z[, "x"]
#   y = Z[, "y"]
#   z = Z[, "z"]
#   expect_that((x-500)^2+ (y-200)^2 + (z-300)^2 - 25, equals(rep(0, 1000), tol=0.01))
# })
# 
# test_that("Solve function works properly", {
#   sphere = solve(x^2+y^2+z^2==5~x&y&z, within=5, nearest=1000)
#   x = sphere[,"x"]
#   y = sphere[,"y"]
#   z = sphere[,"z"]
#   expect_that(x^2+y^2+z^2, equals(rep(5, 1000), tol=.001))
# })

