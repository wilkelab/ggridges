context('palette functions')

test_that('palettes'
  ,{

    expect_equal( length( palette_increase_length() ), 100)

    p = palette_qualitative() %>%
      palette_filter() %>%
      palette_plot_intensity()

    p = palette_qualitative() %>%
      palette_plot_rgp()

    p = palette_qualitative() %>%
      palette_filter( reds = F, thresh_similar = 0) %>%
      palette_plot_intensity()
    
    p = palette_qualitative() %>%
      palette_filter( reds = F) %>%
      palette_plot_intensity()
    
    p = palette_qualitative() %>%
      palette_filter( blues = F, thresh_similar = 0) %>%
      palette_plot_intensity()

    p = palette_qualitative() %>%
      palette_filter( greens = F, thresh_similar = 0) %>%
      palette_plot_intensity()

    p = palette_qualitative() %>%
      palette_filter( greys = F, thresh_similar = 0) %>%
      palette_plot_intensity()

    p = palette_qualitative() %>%
      palette_filter( dark = F, thresh_similar = 0) %>%
      palette_plot_intensity()

    p = palette_qualitative() %>%
      palette_filter( medium = F, thresh_similar = 0) %>%
      palette_plot_intensity()

    p = palette_qualitative() %>%
      palette_filter( bright = F, thresh_similar = 0) %>%
      palette_plot_intensity()

})
