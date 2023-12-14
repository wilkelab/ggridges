
# context('pdata(), qdata(), and rdata()')


test_that("formula interface works.", {
	expect_equal(ignore_attr = TRUE,  length(rdata( ~age, 10, data = mosaicData::HELPrct)), 10 )
	expect_equal(ignore_attr = TRUE,  pdata( ~age, 30, data = mosaicData::HELPrct), prop( ~ mosaicData::HELPrct$age <= 30) )
	expect_equal(ignore_attr = TRUE,  qdata( ~age, .75, mosaicData::HELPrct), quantile(mosaicData::HELPrct$age, .75) )
	expect_equal(ignore_attr = TRUE,  qdata(~ age, c(.25,.75), mosaicData::HELPrct), quantile(mosaicData::HELPrct$age, c(.25,.75)) )
})

test_that("vector/data interface works.", {
	expect_equal(ignore_attr = TRUE,  length(rdata( mosaicData::HELPrct$age, 10, mosaicData::HELPrct)), 10 )
	expect_equal(ignore_attr = TRUE,  pdata(mosaicData::HELPrct$age, 30, data = mosaicData::HELPrct), prop(~ mosaicData::HELPrct$age <= 30) )
	expect_equal(ignore_attr = TRUE,  qdata(mosaicData::HELPrct$age, .75, data = mosaicData::HELPrct), quantile(mosaicData::HELPrct$age, .75) )
	expect_equal(ignore_attr = TRUE,  qdata(mosaicData::HELPrct$age, c(.25,.75), data = mosaicData::HELPrct), quantile(mosaicData::HELPrct$age, c(.25,.75)) )
})


test_that("vector (no data) interface works.", {
	expect_equal(ignore_attr = TRUE,  length(rdata( n = 10, mosaicData::HELPrct$age)), 10 )
	expect_equal(ignore_attr = TRUE,  pdata(mosaicData::HELPrct$age, 30), prop( ~ mosaicData::HELPrct$age <= 30) )
	expect_equal(ignore_attr = TRUE,  qdata(mosaicData::HELPrct$age, .75), quantile(mosaicData::HELPrct$age, .75) )
	expect_equal(ignore_attr = TRUE,  qdata( p = c(.25,.75), mosaicData::HELPrct$age), 
	                   quantile(mosaicData::HELPrct$age, c(.25,.75)) )
})

#test_that("error messages generated", {
#	expect_error( pdata( 30, ~age | sex, mosaicData::HELPrct) )
#	expect_error( qdata( .75, ~age | sex, mosaicData::HELPrct) )
#	expect_error( rdata( 30, ~age | sex, mosaicData::HELPrct) )
#	expect_error( qdata( 30, ~age, mosaicData::HELPrct) )
#})

