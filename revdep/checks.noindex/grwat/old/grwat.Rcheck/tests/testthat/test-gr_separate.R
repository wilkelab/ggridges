data(spas)

test_that('Separation with various filters works', {
  params = gr_get_params(reg = 'center')
  filters = c('kudelin', 'boughton', 'chapman', 'jakeman', 'lynehollick', 'maxwell')
  
  for (flt in filters) {
    params$filter = flt
    sep = expect_message(gr_separate(spas, params))
    expect_gt(sep$Quick[115], 0)
  }
  
})

test_that('Debug separation works', {
  
  params = gr_get_params(reg = 'center')
  params$filter = 'kudelin'
  
  sep_debug = expect_warning(gr_separate(spas, params, debug = TRUE))
  expect_gt(sep_debug$Quick[115], 0)
  
  # a vector of years with jittered params
  jit = attributes(sep_debug)$jittered
  
  # actual params used for each year
  parlist = attributes(sep_debug)$params
  
  # extract and tweak parameters for selected year
  p = parlist[['1989']]
  p$grad1 = 1
  p$grad2 = 2.5
  p$floodprec = 0.1
  p$precdays = 5
  p$sprecdays = 15
  p$spcomp = 1.5
  
  # use tweaked parameters for all years
  sep_debug = suppressWarnings(gr_separate(spas, params = p, debug = TRUE)) # TODO: expect warning, but unstable
  jit = attributes(sep_debug)$jittered
  
  # actual params used for each year
  parlist = attributes(sep_debug)$params
  
  # tweak parameters for selected year
  parlist[['1989']]$grad1 = 3
  parlist[['1989']]$grad2 = 6
  
  # set the sprecdays parameter for multiple years
  parlist = gr_set_param(parlist, sprecdays, 
                         years = c(1978, 1989:1995), 
                         value = 15)
  
  # set the spcomp parameter for all years
  parlist = gr_set_param(parlist, spcomp, value = 2.5)
  
  # use the list of parameters for separation
  sep_debug = suppressWarnings(gr_separate(spas, params = parlist, debug = TRUE)) # TODO: expect warning, but unstable
  jit = attributes(sep_debug)$jittered
  expect_equal(length(jit), 0)
})