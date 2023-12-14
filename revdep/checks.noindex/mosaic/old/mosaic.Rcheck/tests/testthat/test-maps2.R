# context("maps2")

# turning off because CIAdata load seems to be causing trouble for CRAN
# should replace this with some other data

if (FALSE) {
gdpData <- CIAdata("GDP")      # load some world data
gdpData <- gdpData |> mutate(GDP5 = ntiles(-GDP, 5, format="rank"))


testthat::test_that("World Maps work", {
  

  wrapped_expect_doppelganger("worldmap1", mWorldMap(gdpData, key="country", fill="GDP")) 
  wrapped_expect_doppelganger("worldmap2", mWorldMap(gdpData, key="country", fill="GDP5")) 
  wrapped_expect_doppelganger("worldmap3", mWorldMap(gdpData, key="country", plot="frame") +
                                geom_point()) 
  
  mergedData <- mWorldMap(gdpData, key="country", plot="none")
  wrapped_expect_doppelganger("worldmap4", ggplot(mergedData, aes(x=long, y=lat, group=group, order=order)) +
                                geom_polygon(aes(fill=GDP5), color="gray70", size=.5) + guides(fill=FALSE)) 
  
})

}  # end if(FALSE)


USArrests2 <- USArrests |> tibble::rownames_to_column("state")
testthat::test_that("US Maps work", {
  wrapped_expect_doppelganger("usmaps1", mUSMap(USArrests2, key = "state", fill = "UrbanPop"))
  
})
