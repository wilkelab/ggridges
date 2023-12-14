
# context("t.test()")

Boys <- filter(mosaicData::Galton, sex=="M")
y <- Galton$height
a <- Galton$sex

test_that("2-sample tests give same results as stats::t.test", {
  
  expect_equal(ignore_attr = TRUE,  
    confint(stats::t.test(y ~ a)),
    confint(t.test(y ~ a))
  )
  
  expect_equal(ignore_attr = TRUE,  
    confint(stats::t.test(height ~ sex, data=mosaicData::Galton)),
    confint(t.test(height ~ sex, data=mosaicData::Galton))
  )
  
  expect_equal(ignore_attr = TRUE,  
    with(mosaicData::Galton, confint(stats::t.test(height ~ sex))),
    with(mosaicData::Galton, confint(t.test(height ~ sex)))
  )
  
  expect_equal(ignore_attr = TRUE,  
    confint(stats::t.test(Boys$father, Boys$height)),
    confint(t.test(Boys$father, Boys$height))
  )
  
  expect_equal(ignore_attr = TRUE,  
    confint(stats::t.test(Boys$father, Boys$height)),
    confint(with(Boys, t.test(father, height)))
  )
  
  # expect_equal(ignore_attr = TRUE,  
  #   confint(stats::t.test(Boys$father, Boys$height)),
  #   confint(t.test(father, height, data=Boys))
  # )
})

test_that("paired tests give same results as stats::t.test", {
  
  # expect_equal(ignore_attr = TRUE,  
  #   confint(stats::t.test(Boys$father, Boys$height, paired=TRUE)),
  #   confint(t.test(father, height, data=Boys, paired=TRUE))
  # )
   
  expect_equal(ignore_attr = TRUE,  
    confint(stats::t.test(Boys$father, Boys$height, paired=TRUE)),
    confint(t.test(Boys$father, Boys$height, paired=TRUE))
  )
  
  expect_equal(ignore_attr = TRUE,  
    confint(stats::t.test(Boys$father, Boys$height, paired=TRUE)),
    confint(t.test(~ (father - height), data=Boys))
  )
  
  expect_equal(ignore_attr = TRUE,  
    confint(stats::t.test(Boys$father, Boys$height, paired=TRUE)),
    confint(with(Boys, t.test(father, height, paired=TRUE)))
  )
})

test_that("1-sample tests give same results as stats::t.test", {
  
  expect_equal(ignore_attr = TRUE, 
    confint(stats::t.test(Boys$height)),
    confint(t.test(~ height, data=Boys))
  )
  
  # expect_equal(ignore_attr = TRUE, 
  #   confint(stats::t.test(Boys$height)),
  #   confint(t.test(height, data=Boys))
  # )
  
  expect_equal(ignore_attr = TRUE, 
    confint(stats::t.test(Boys$height)),
    confint(t.test(Boys$height))
  )
  
  expect_equal(ignore_attr = TRUE, 
    confint(stats::t.test(Boys$height)),
    confint(with(Boys, t.test(height)))
  )
})
