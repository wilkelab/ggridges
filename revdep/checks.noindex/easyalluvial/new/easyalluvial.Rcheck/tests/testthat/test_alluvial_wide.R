
context('alluvial wide')

test_that('alluvial_wide'
  ,{

    data = mtcars2

    max_variables = 5

    p = alluvial_wide( data = data
                    , max_variables = max_variables
                    , fill_by = 'first_variable' )
    
    expect_doppelganger('wide_first', p)

    p = alluvial_wide( data = data
                    , max_variables = max_variables
                    , fill_by = 'last_variable' )
    
    expect_doppelganger('wide_last', p)
    
    p = alluvial_wide( data = data
                    , max_variables = max_variables
                    , fill_by = 'all_flows' )

    expect_doppelganger('wide_all_flows', p)
    
    p = alluvial_wide( data = data
                    , max_variables = max_variables
                    , fill_by = 'values' )

    expect_doppelganger('wide_values', p)
    
    # manually order variable values

    p = alluvial_wide( data = data
                    , max_variables = max_variables
                    , fill_by = 'values'
                    , order_levels = c('8', '4', '6') )
    
    expect_doppelganger('wide_reorder_y_levels', p)
    
    #check integritiy of returned dataframe
    expect_equal( nrow(data), nrow(p$data_key) )

    # id

    p = alluvial_wide(data, id = ids )
    expect_true( ! 'ID' %in% names(p$data_key) )
    expect_true( length(unique(p$data_key$ids) ) == nrow(p$data_key)  )

    p = alluvial_wide(data, id = 'ids' )
    expect_true( ! 'ID' %in% names(p$data_key) )
    expect_true( length(unique(p$data_key$ids) ) == nrow(p$data_key)  )

    p = alluvial_wide(data, id = NULL)
    expect_true( 'ID' %in% names(p$data_key) )
    expect_true( length(unique(p$data_key$ID) ) == nrow(p$data_key)  )

    #check automatic angling of x axis labels

    data = ISLR::Auto %>%
      as_tibble() %>%
      mutate( name_x = row_number()
              , name_x = paste( name, name_x ) ) %>%
      select( - name ) %>%
      mutate_at( vars( c('cylinders', 'year', 'origin' ) ), as.factor  )

    p = alluvial_wide( data, id = name_x, max_variables = 5 ) 
    
    expect_doppelganger('wide_ISLR_cars', p)
    
    p = alluvial_wide( data, id = name_x, max_variables = 5, auto_rotate_xlabs = F )
    
    expect_doppelganger('wide_ISLR_cars_rotate_labels', p)
    
    # check NA behavoir, rename label ando order to front

    data$cylinders[1:4] = NA

    p = alluvial_wide( data = data
                         , max_variables = max_variables
                         , fill_by = 'first_variable'
                         , NA_label = 'none'
                         , order_levels = 'none' )
    
    #vdiffr detects difference when rendered with different OS
    #expect_doppelganger('wide_NA_label', p)
    
    # test statum options

    p = alluvial_wide( data = data
                       , max_variables = max_variables
                       , fill_by = 'first_variable'
                       , stratum_labels = F
                       , stratum_label_type = "none"
                       , stratum_width = 1/20 )
    
    #vdiffr detects difference when rendered with different OS
    #expect_doppelganger('wide_Strat_width', p)

    # test warning for high flow numbers

    expect_warning( alluvial_wide( data = ggplot2::diamonds) )
    
    #gouped df
    
    p = alluvial_wide( group_by(mtcars2, cyl), max_variables = 3 )
    
    # plot attachments
    
    expect_true( all( c('data_key', 'alluvial_type', 'alluvial_params') %in% names(p) ) )
    
    # color of stratum same as fill variable
    
    p = alluvial_wide( data = data
                       , max_variables = max_variables
                       , fill_by = 'first_variable' 
                       , colorful_fill_variable_stratum = T)
    
    # renders differently on mac
    # expect_doppelganger('colorful_fill_variable_stratum', p)
    
    p = alluvial_wide( data = data
                       , max_variables = max_variables
                       , fill_by = 'last_variable' 
                       , colorful_fill_variable_stratum = T)
    
    p = alluvial_wide( data = data
                       , max_variables = max_variables
                       , fill_by = 'all_flows' 
                       , colorful_fill_variable_stratum = T)
    
    p = alluvial_wide( data = data
                       , max_variables = max_variables
                       , fill_by = 'values' 
                       , colorful_fill_variable_stratum = T
                       , col_vector_value = palette_qualitative() %>% palette_filter( greys = F) 
                       )
    
    
  })


test_that('alluvial_wide_all_char_cols',{
  p = mtcars2 %>%
    select_if(is.factor) %>%
    mutate_all(as.character) %>%
    alluvial_wide()  
  
  expect_true("ggplot" %in% class(p))
})
