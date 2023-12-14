

context('Test manipulation functions')


test_that( 'manip_factor_2_numeric'
  ,{

    fac_num = factor( c(1,3,8) )
    fac_chr = factor( c('foo','bar') )
    fac_chr_ordered = factor( c('a','b','c'), ordered = T )

    expect_identical( manip_factor_2_numeric( fac_num ), c(1,3,8) )
    expect_identical( manip_factor_2_numeric( fac_chr ), c(2,1) )
    expect_identical( manip_factor_2_numeric( fac_chr_ordered ), c(1,2,3) )

})


test_that('manip_bin_numerics'
  ,{

  categoricals = c('cyl', 'vs', 'am', 'gear', 'carb')

  data = mtcars2

  data_new = manip_bin_numerics(data)

  numerics = data_new %>%
    select_if( is.numeric ) %>%
    names()

  expect_true( is_empty(numerics) )
  expect_true( ! is_empty(data_new) )
  expect_identical( names(data_new) , names(data) )
  expect_true( ! 'easyalluvialid' %in% names(data_new) )
  
  bins_from_vec = manip_bin_numerics(data$disp)
  expect_equal( levels(bins_from_vec), c("LL", "ML", "M",  "MH", "HH") )
  
  data_new_cuts = manip_bin_numerics(data, bin_labels = 'cuts')
  
  data_new_median = manip_bin_numerics(data, bin_labels = 'median')
  
  data_new_mean = manip_bin_numerics(data, bin_labels = 'mean')
  
  data_new_min_max = manip_bin_numerics(data, bin_labels = 'min_max')
  
  expect_false( identical(data_new_cuts, data_new_median) )
  
  expect_false( identical(data_new_mean, data_new_median) )
  
})


test_that('manip_bin_numerics no numerics in data'
          ,{

  data = mtcars %>%
    mutate_all( as.factor )

  data_new = manip_bin_numerics(data)

  expect_identical(data, data_new)

})

test_that('manip_bin_numerics zero variance columns'
          ,{

  data = mtcars %>%
    as_tibble() %>%
    mutate( zero_var = 1
            , zero = 0
            , near_zero_var = c( rep(1,nrow(.)-1), 0.9 ) )

  expect_warning(data_new <- manip_bin_numerics(data))

  expect_identical( select(data, zero_var, zero)
                    , select(data_new, zero_var, zero) )

  expect_true( is.factor(data_new$near_zero_var) )

})

test_that('manip_bin_numerics with vector'
          , {

  vec = manip_bin_numerics(mtcars$mpg)
  expect_true( is.factor(vec) )

  vec = manip_bin_numerics( as.factor(mtcars$cyl) )
  expect_identical( vec, as.factor(mtcars$cyl) )
  
  df = tibble( a = rnorm(50), b = rnorm(50), c = seq(1:50) ) %>%
    mutate( c = as.character(c) )
  
  df_v1 = manip_bin_numerics(df)
  
  df_v2 = df %>%
    mutate( a = manip_bin_numerics(a)
            , b = manip_bin_numerics(b)
            , c = as.character( seq(1:50) ) )
  
  expect_identical( df_v1, df_v2)
  
  df_v1 = manip_bin_numerics(df, bin_labels = 'median')
  
  df_v2 = df %>%
    mutate( a = manip_bin_numerics(a, bin_labels = 'median')
            , b = manip_bin_numerics(b, bin_labels = 'median')
            , c = as.character( seq(1:50) ) )
  
  expect_identical( df_v1, df_v2)
  
  df_v1 = manip_bin_numerics(df, bin_labels = 'min_max')
  
  df_v2 = df %>%
    mutate( a = manip_bin_numerics(a, bin_labels = 'min_max')
            , b = manip_bin_numerics(b, bin_labels = 'min_max')
            , c = as.character( seq(1:50) ) )
  
  expect_identical( df_v1, df_v2 )
  
})
 
test_that('manip_bin_numerics_NA',{
  
  v =  rnorm(10)
  x = manip_bin_numerics( c( v, NA) )
  expect_true( is.factor(x) )
  expect_true( 'NA' %in% levels(x) )
  
  test_bin_labels = function( bin_label){
    x = manip_bin_numerics( c( v, NA), bin_labels = bin_label )
    expect_true( 'NA' %in% levels(x) )
    y = manip_bin_numerics( v, bin_labels = bin_label )
    expect_true( all( levels(y) %in% levels(x) ) )
    expect_false( all( levels(x) %in% levels(y) ) )
  }
  
  test_bin_labels('median')
  test_bin_labels('mean')
  test_bin_labels('min_max')
  test_bin_labels('cuts')
  
  df = mtcars2 %>%
    bind_rows( mtcars2['cyl'][1,] ) %>%
    manip_bin_numerics() %>%
    select( - cyl ) %>%
    select_if( is.factor) %>%
    summarise_all( function(x) list(levels(x)) ) %>%
    summarise_all( function(x) 'NA' %in% unlist(x) )
  
  expect_true( all( as.matrix( df[1,] ) ) )
  
})

test_that("most_frequent_lvl", {
  
  lvls <- as.factor(LETTERS[c(7, 7, 7, 1, 4, 8, 9, 14, 20)])
  lvl <- get_most_frequent_lvl(lvls)
  expect_true(lvl == "G")
  
})

test_that("manip_bin_numerics_warning",{
  df <- data.frame(x = c(1,1,2,2,9,9,10),
                   y = c(1,2,3,4,5,6,7))
  expect_warning(manip_bin_numerics(df))
})
