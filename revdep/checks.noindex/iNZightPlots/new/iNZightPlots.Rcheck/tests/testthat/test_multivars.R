# testing of multiple variables
cas5k <- iNZightMR::census.at.school.5000

test_that("Basic multi-variable plots work", {
    expect_s3_class(
        suppressWarnings(
            inzplot(~ techtv + techmp3 + techinternet + techmobinternet + techfacebook +
                techtwitter + techbebo + techmyspace + techskype + technone, data = cas5k)
        ),
        "gg_multi_binary"
    )

    expect_s3_class(
        suppressWarnings(
            inzplot(~ techtv + techmp3 + techinternet + techmobinternet + techfacebook +
                techtwitter + techbebo + techmyspace + techskype + technone,
            data = cas5k,
            keep_missing = TRUE
            )
        ),
        "gg_multi_stack"
    )

    expect_s3_class(
        inzplot(~ techtv + techmp3 + techinternet + techmobinternet + techfacebook +
            techtwitter + techbebo + techmyspace + techskype + technone,
            data = cas5k,
            g1 = importenergy
        ),
        "gg_multi_binary"
    )

    expect_s3_class(
        inzplot(~ techtv + techmp3 + techinternet + techmobinternet + techfacebook +
            techtwitter + techbebo + techmyspace + techskype + technone,
        data = cas5k,
        g1 = gender, g1.level = "female"
        ),
        "gg_multi_binary"
    )

    cas5k2 <- cas5k
    attr(cas5k2$techmp3, "table") <- "one"
    attr(cas5k2$techinternet, "table") <- "one"
    attr(cas5k2$techskype, "table") <- "two"
    attr(cas5k2$techfacebook, "table") <- "two"

    expect_s3_class(
        inzplot(~ techmp3 + techinternet + techfacebook + techskype, data = cas5k2),
        "gg_multi_binary"
    )

    expect_s3_class(
        inzplot(~ country_en + country_mi,
            data = cas5k2,
            outcome_value = "NZL"
        ),
        "gg_multi_binary"
    )
})

test_that("Multi-variable summaries work", {
    expect_s3_class(
        inzsummary(~ techtv + techmp3 + techinternet, data = cas5k),
        "knitr_kable"
    )
})

test_that("Non-cat vars are removed is option set", {
    expect_error(
        inzsummary(~ techtv + techmp3 + techinternet + height, data = cas5k)
    )
    op <- options(inzight.auto.remove.noncatvars = TRUE)
    on.exit(options(op))
    expect_warning(
        inzsummary(~ techtv + techmp3 + techinternet + height, data = cas5k)
    )
})

skip("Remaining testing needs to be completed")

dd <- data.frame(
    q1 = sample(c("never", "sometimes", "often", "always", "don't know"), 100, replace = TRUE),
    q2 = sample(c("never", "sometimes", "often", "always", "don't know"), 100, replace = TRUE),
    stringsAsFactors = TRUE
)

inzplot(~ q1 + q2, data = dd)
inzplot(~ q1 + q2, data = dd, plottype = "gg_multi_col")


cas_raw <- iNZightTools::smart_read("cas500_coded.csv")
cas_dict <- iNZightTools::read_dictionary("casdict.csv",
    name = "variable",
    title = "friendly_name"
)
cas <- iNZightTools::apply_dictionary(cas_raw, cas_dict)

levels(cas$getlunch) <- levels(cas$travel)

for (c in c("bike", "bus", "motor", "other", "train", "walk")) {
    cas[[c]] <- expss::set_var_lab(
        ifelse(cas$travel == c, "yes", "no"),
        sprintf(
            "What ways do you travel to school? %s%s",
            toupper(substr(c, 1, 1)),
            substr(c, 2, 100)
        )
    )
}

inzsummary(~ getlunch + travel, data = cas)

inzsummary(~ getlunch + travel, data = cas, g1 = gender)

inzsummary(~ bike + bus + motor + train + walk + other, data = cas)

inzplot(~ bus + train + bike, data = cas, g1 = gender)
inzsummary(~ bus + train + bike, data = cas, g1 = gender)

inzsummary(~ bike + bus + motor + train + walk + other, data = cas, g1 = gender)


# inzplot(~eth5_e_y8c+eth5_m_y8c+eth5_p_y8c, data = d)
# inzplot(~tu82_0_y8c+tu82_1_y8c+tu82_2_y8c+tu82_3_y8c+tu82_4_y8c+tu82_5_y8c+tu82_6_y8c+tu82_7_y8c+tu82_8_y8c, data = d)


## linked multi-vars
dummy_df <- data.frame(
    d1_varx_1 = structure(factor(sample(c("yes", "no"), 100, TRUE, c(0.2, 0.8))), table = "d1"),
    d1_varx_2 = structure(factor(sample(c("yes", "no"), 100, TRUE, c(0.4, 0.6))), table = "d1"),
    d1_varx_3 = structure(factor(sample(c("yes", "no"), 100, TRUE, c(0.9, 0.1))), table = "d1"),
    d2_varx_1 = structure(factor(sample(c("yes", "no"), 100, TRUE, c(0.5, 0.5))), table = "d2"),
    d2_varx_2 = structure(factor(sample(c("yes", "no"), 100, TRUE, c(0.3, 0.7))), table = "d2"),
    d2_varx_3 = structure(factor(sample(c("yes", "no"), 100, TRUE, c(0.8, 0.2))), table = "d2")
)
dummy_df2 <- data.frame(
    varx_1 = factor(sample(c("yes", "no"), 100, TRUE, c(0.2, 0.8))),
    varx_2 = factor(sample(c("yes", "no"), 100, TRUE, c(0.4, 0.6))),
    varx_3 = factor(sample(c("yes", "no"), 100, TRUE, c(0.9, 0.1)))
)

inzplot(~ d1_varx_1 + d1_varx_2 + d1_varx_3 + d2_varx_1 + d2_varx_2 + d2_varx_3, data = dummy_df)
inzplot(~ varx_1 + varx_2 + varx_3, data = dummy_df2)


# guinz
guinz <- iNZightTools::load_linked("~/terourou/guinz/data/guinz8y2.inzlnk")

inzplot(~fn6_y8c, g1 = cself_proeth_y8c, data = guinz, plottype = "gg_stackedcolumn")


inzplot(~ eth5_e_y8c + eth5_m_y8c + eth5_p_y8c + eth5_a_y8c, data = guinz, outcome_value = "Yes")

guinz <- iNZightTools::load_linked("~/terourou/guinz/data/guinz_full.inzlnk")
inzplot(~ eth5_e_am + eth5_m_am + eth5_p_am + eth5_a_am + eth5_mela_am +
    eth5_o_am + eth5_nzder_am + eth5_e_m9m + eth5_m_m9m + eth5_p_m9m + eth5_a_m9m +
    eth5_mela_m9m + eth5_o_m9m + eth5_nzder_m9m + eth5_e_y8c + eth5_m_y8c +
    eth5_p_y8c + eth5_a_y8c + eth5_mela_y8c + eth5_o_y8c + eth5_nzder_y8c,
outcome_value = "Yes",
rotation = TRUE,
# x_groups = list(
#     European = c("European", "New Zealand European"),
#     Pacific = c("Pacific", "Pacific people", "pacific")
# ),
data = guinz
)

inzplot(~ eth5_e_am + eth5_m_am + eth5_p_am + eth5_a_am + eth5_mela_am +
    eth5_o_am + eth5_nzder_am + eth5_e_m9m + eth5_m_m9m + eth5_p_m9m +
    eth5_a_m9m + eth5_mela_m9m + eth5_o_m9m + eth5_nzder_m9m + eth5_e_m54cm +
    eth5_m_m54cm + eth5_p_m54cm + eth5_a_m54cm + eth5_mela_m54cm +
    eth5_o_m54cm + eth5_nzder_m54cm + eth5_e_y8c + eth5_m_y8c +
    eth5_p_y8c + eth5_a_y8c + eth5_mela_y8c + eth5_o_y8c + eth5_nzder_y8c,
data = guinz,
# x_groups = list(
#     European = c("European", "New Zealand European", "European?"),
#     Pacific = c("Pacific", "Pacific people", "pacific", "Pacific?"),
#     Maori = c("Maori", "Maori?"),
#     Asian = c("Asian", "Asian?"),
#     MELAA = c("MELAA", "MELAA?"),
#     Other = c("Other", "Other?"),
#     "New Zealander" = c("New Zealander", "New Zealander?")
# ),
outcome_value = "Yes",
rotation = TRUE,
full_cases = TRUE
)


inzsummary(~ eth5_e_am + eth5_m_am + eth5_p_am + eth5_a_am + eth5_mela_am +
    eth5_o_am + eth5_nzder_am + eth5_e_m9m + eth5_m_m9m + eth5_p_m9m +
    eth5_a_m9m + eth5_mela_m9m + eth5_o_m9m + eth5_nzder_m9m + eth5_e_m54cm +
    eth5_m_m54cm + eth5_p_m54cm + eth5_a_m54cm + eth5_mela_m54cm +
    eth5_o_m54cm + eth5_nzder_m54cm + eth5_e_y8c + eth5_m_y8c +
    eth5_p_y8c + eth5_a_y8c + eth5_mela_y8c + eth5_o_y8c + eth5_nzder_y8c,
data = guinz,
# x_groups = list(
#     European = c("European", "New Zealand European", "European?"),
#     Pacific = c("Pacific", "Pacific people", "pacific", "Pacific?"),
#     Maori = c("Maori", "Maori?"),
#     Asian = c("Asian", "Asian?"),
#     MELAA = c("MELAA", "MELAA?"),
#     Other = c("Other", "Other?"),
#     "New Zealander" = c("New Zealander", "New Zealander?")
# ),
outcome_value = "Yes",
rotation = TRUE,
full_cases = TRUE
)



inzplot(~ eth5_e_am + eth5_m_am + eth5_p_am + eth5_a_am + eth5_mela_am +
    eth5_o_am + eth5_nzder_am + eth5_e_m9m + eth5_m_m9m + eth5_p_m9m +
    eth5_a_m9m + eth5_mela_m9m + eth5_o_m9m + eth5_nzder_m9m + eth5_e_m54cm +
    eth5_m_m54cm + eth5_p_m54cm + eth5_a_m54cm + eth5_mela_m54cm +
    eth5_o_m54cm + eth5_nzder_m54cm + eth5_e_y8c + eth5_m_y8c +
    eth5_p_y8c + eth5_a_y8c + eth5_mela_y8c + eth5_o_y8c + eth5_nzder_y8c,
data = guinz,
# x_groups = list(
#     European = c("European", "New Zealand European", "European?"),
#     Pacific = c("Pacific", "Pacific people", "pacific", "Pacific?"),
#     Maori = c("Maori", "Maori?"),
#     Asian = c("Asian", "Asian?"),
#     MELAA = c("MELAA", "MELAA?"),
#     Other = c("Other", "Other?"),
#     "New Zealander" = c("New Zealander", "New Zealander?")
# ),
# outcome_value = "Yes",
# rotation = TRUE,
full_cases = TRUE
)

inzplot(~ eth5_e_am + eth5_m_am + eth5_p_am + eth5_a_am,
    g1 = cself_proeth_y8c,
    data = guinz,
    outcome_value = "Yes"
)


## themeing
guinz_palette <- list(
    primary = c("#004775", "#00a9e9", "#39b54a"),
    secondary = c("#6f818e", "#fdb913", "#d2232a")
)

theme_guinz <- function() {
    ggplot2::theme_classic(
        base_family = "Myriad Pro"
    ) +
        ggplot2::theme(
            legend.position = "bottom",
            axis.line = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_line(
                size = 0.5,
                color = "gray80"
            ),
            panel.background = ggplot2::element_rect(fill = "white")
        )
}
ggplot2::update_geom_defaults("bar", list(fill = guinz_palette$primary[2]))
ggplot2::theme_set(theme_guinz())

options(
    inzight.default.palette.cat = c(guinz_palette$primary, guinz_palette$secondary),
    ggplot2.discrete.fill = c(guinz_palette$primary, guinz_palette$secondary)
)

inzplot(~ techbebo + techfacebook + techinternet,
    data = iNZightMR::census.at.school.5000,
    gg_theme = theme_guinz()
)
inzplot(~ sport + sport_en, data = iNZightMR::census.at.school.5000)
