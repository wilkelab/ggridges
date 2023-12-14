
context('plot condensation')


test_that('plot condensation'
          ,{


    df = mtcars %>%
      mutate( cyl = as.factor(cyl)
              , gear = as.factor(gear)
              , vs = as.factor(vs)
              , am = as.factor(am))

    p = plot_condensation(df)
    
    expect_doppelganger('cond', p)
    
    p = plot_condensation(df, first = 'disp')
    
    expect_doppelganger('cond_with_first', p)
    
    p = plot_condensation(ggplot2::diamonds, first = 'price')
    
    expect_doppelganger('cond_price_first', p)
    
    # compare values from alluvial_wide and plot_condensation

    values = p$data %>%
      mutate( test1 = map_lgl(comb, function(x) all( c('price', 'carat', 'x', 'y', 'z', 'depth', 'cut') %in% x ) )
              , test2 = map_lgl(comb, function(x) all( x %in% c('price', 'carat', 'x', 'y', 'z', 'depth', 'cut') ) )) %>%
      filter( test1 & test2 ) %>%
      select( value, key ) %>%
      spread( value = value, key = key )

    n_flows_p = values$`number of flows`
    condens_p = round( values$`percent condensation` * 100, 1 )

    p_alluv = alluvial_wide( select(ggplot2::diamonds, price, carat, x, y, z, depth, cut) )

    caption = p_alluv$labels$caption %>%
      stringr::str_split('\n')

    n_flows_alluv = as.double( stringr::str_extract( caption[[1]][1], '[0-9.]+' ) )
    condens_alluv = as.double( stringr::str_extract( caption[[1]][2], '[0-9.]+' ) )

    expect_equal( n_flows_p, n_flows_alluv )
    expect_equal( condens_p, condens_alluv )

    # test unquoted expression

    p = plot_condensation(ggplot2::diamonds, first = price)


})
