skip_if_not_installed("RSQLite")
skip_if_not_installed("dbplyr")

iris_species <- data.frame(
    species_id = 1:3,
    species_name = levels(iris$Species),
    type_id = c(1L, 1L, 2L)
)
iris_data <- iris %>%
    dplyr::mutate(
        id = seq_len(dplyr::n()),
        species_id = as.integer(iris$Species),
        Species = NULL
    )

db <- tempfile(fileext = ".db")
con <- DBI::dbConnect(RSQLite::SQLite(), db)
on.exit({
    DBI::dbDisconnect(con)
    unlink(db)
})
DBI::dbWriteTable(con, "iris_species", iris_species)
DBI::dbWriteTable(con, "iris_data", iris_data)
DBI::dbWriteTable(
    con, "iris_extra",
    data.frame(
        id = 1:2,
        type = c("Fluffy", "Hard")
    )
)

d <- inzdf(con,
    "iris_linked",
    schema = list(
        iris_data = list(
            links_to = list(
                iris_species = "species_id"
            )
        ),
        iris_species = list(
            links_to = list(
                iris_extra = c("type_id" = "id")
            )
        )
    ),
    keep_con = TRUE
)

test_that("Basic plots work", {
    p <- inzplot(~Sepal.Width, data = d)
    expect_is(p, "inzplotoutput")
})
