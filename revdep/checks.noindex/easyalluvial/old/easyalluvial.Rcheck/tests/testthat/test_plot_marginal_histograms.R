

context('marginal histograms')

test_that('plot_hist_as_margins',{
  
  # wide----------------------------------------------
  
  p_wide = alluvial_wide( data = mtcars2, id = ids
                     , max_variables = 5                 
                     , fill_by = 'first_variable'
                     , colorful_fill_variable_stratum = T)
  
  set.seed(1)
  p = plot_hist('cyl', p_wide, mtcars2)
  expect_doppelganger('plot_hist_wide_cat', p)

  set.seed(1)
  p = plot_hist('disp',p_wide, mtcars2)
  expect_doppelganger('plot_hist_wide_num', p)
  
  set.seed(1)
  p = add_marginal_histograms(p_wide, mtcars2, plot = F)
  
  # gtables not yet supported by vdiffr
  # expect_doppelganger('marg_hist_wide', p)
  
  
  # long numeric---------------------------------------
  
  p_long = alluvial_long(quarterly_sunspots, key = qu, value = spots
                    , id = year)
  
  set.seed(1)
  p = plot_hist('Q1',p_long, quarterly_sunspots)
  expect_doppelganger('plot_hist_long_num', p)
  
  set.seed(1)
  p = add_marginal_histograms(p_long, quarterly_sunspots, plot = F)
  
  # gtables not yet supported by vdiffr
  # expect_doppelganger('marg_hist_long', p)
  
  p_long = alluvial_long(quarterly_sunspots, key = qu, value = spots
                     , id = year, fill = mean_spots_per_year)
  set.seed(1)
  p = plot_hist('Q1',p_long, quarterly_sunspots)
  expect_doppelganger('plot_hist_long_num_has_fill', p)
  
  set.seed(1)
  p = plot_hist('mean_spots_per_year',p_long, quarterly_sunspots)
  expect_doppelganger('plot_hist_long_num_is_fill', p)
  
  set.seed(1)
  p = add_marginal_histograms(p_long, quarterly_sunspots, plot = F)
  
  # gtables not yet supported by vdiffr
  # expect_doppelganger('marg_hist_long_num_fill', p)
  
  
  # long categoric --------------------------------------
  
  p_long = alluvial_long(quarterly_flights, key = qu, value = mean_arr_delay
                    , id = tailnum, fill = carrier)
  
  set.seed(1)
  p = plot_hist('Q1', p_long, quarterly_flights)
  expect_doppelganger('plot_hist_long_cat', p)
  
  set.seed(1)
  p = plot_hist('carrier', p_long, quarterly_flights)
  expect_doppelganger('plot_hist_long_cat_fill', p)
  
  set.seed(1)
  p = add_marginal_histograms(p_long, quarterly_flights, plot = F)
  
  # gtables not yet supported by vdiffr
  # expect_doppelganger('marg_hist_long_cat-fill', p)
})

test_that("plot_hist_margins_model_respons" ,{
  
  skip_if_not_installed("caret")
  
  # model response numeric ----------------------------------
  set.seed(1)
  df = select(mtcars2, -ids)
  train = caret::train( disp ~ .
                        , df, method = 'lm'
                        ,trControl = caret::trainControl(method = 'none') )
  
  p_mod_num = alluvial_model_response_caret(train, df, degree = 3)
  
  p = plot_hist('pred', p_mod_num, df)
  expect_doppelganger('mod_num_pred', p)
  
  p = plot_hist('carb', p_mod_num, df)
  expect_doppelganger('mod_num_cat', p)

  p = plot_hist('wt', p_mod_num, df)
  expect_doppelganger('mod_num_num', p)
  
  p_grid = add_marginal_histograms(p_mod_num, df, plot = F)
  
  # gtables not yet supported by vdiffr
  # expect_doppelganger('marg_hist_mod_num', p_grid)
  
  p_grid = add_marginal_histograms(p_mod_num, df, keep_labels = T, plot = F)
  
  # gtables not yet supported by vdiffr
  # expect_doppelganger('marg_hist_mod_num_labels', p_grid)
  
  p_mod_num = alluvial_model_response_caret(train, df, degree = 3
                                    , pred_train = predict(train, mtcars2))
  
  p = plot_hist('pred', p_mod_num, df)
  expect_doppelganger('mod_num_pred_train', p)
  
  p_grid = add_marginal_histograms(p_mod_num, df, keep_labels = T, plot = F)
  
  # gtables not yet supported by vdiffr
  # expect_doppelganger('marg_hist_mod_num_pred_train', p_grid)

  # model response categoric --------------------------------
  set.seed(1)
  df = select(mtcars2, -ids)
  train = caret::train( cyl ~ .
                        , df, method = 'rf'
                        , trControl = caret::trainControl(method = 'none')
                        , importance = T)
  
  p_mod_cat = alluvial_model_response_caret(train, df, degree = 3)
  
  p = plot_hist('pred', p_mod_cat, df, pred_train = predict(train, mtcars2))
  # not stable across R and package versions
  # expect_doppelganger('mod_cat_pred_train', p)
  
  expect_true("ggplot" %in% class(p))
  
  p = plot_hist('pred', p_mod_cat, df )
  # not stable across R and package versions
  # expect_doppelganger('mod_cat_pred', p)
  
  expect_true("ggplot" %in% class(p))
  
  p_grid = add_marginal_histograms(p_mod_cat, df, keep_labels = T, pred_train = predict(train, mtcars2), plot = F )
  
  expect_true("gtable" %in% class(p_grid))
  
  # gtables not yet supported by vdiffr
  # expect_doppelganger('marg_hist_mod_cat_pred_train', p_grid)
  
})

test_that('model response marginal hists, extra columns in df',{
  
  skip_if_not_installed("caret")
  
  set.seed(1)
  df = select(mtcars2, -ids)
  train = caret::train( disp ~ mpg + wt + cyl + qsec + carb
                        , df, method = 'lm'
                        ,trControl = caret::trainControl(method = 'none') )
  
  expect_error(alluvial_model_response_caret(train, df, degree = 3))
  
  p_mod_num = alluvial_model_response_caret(train, df, degree = 3, resp_var = "disp")
  
  p = plot_hist('pred', p_mod_num, df)
  

})


