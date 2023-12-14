library(iNZightTools)

cas_raw <- smart_read("cas500_coded.csv")
cas_dict <- read_dictionary("casdict.csv",
    name = "variable",
    title = "friendly_name"
)

cas <- apply_dictionary(cas_raw, cas_dict)

skip_if_not_installed("ggmosaic")
skip_if_not_installed("waffle")

test_that("Variable lables used if present", {
    # devtools::load_all()
    expect_is(inzplot(~height, cas), "inzplotoutput")
    expect_is(inzplot(~height, cas, plottype = "gg_barcode"), "gg")

    expect_is(inzplot(~travel, cas, plottype = "gg_column"), "gg")
})
