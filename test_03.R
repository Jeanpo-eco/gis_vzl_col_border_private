fs::dir_create(
    here("Output/Bases/border_crossings")
)

crossings <-
    read_csv(
        here(
            "data/raw/border_crossings",
            "border_crossings.csv"
        ),
        show_col_types = FALSE
    )

crossings <-
    st_as_sf(
        crossings,
        coords = c("longitude", "latitude"),
        crs = 4326
    )

study_area <- st_read(
  here(
    "Output/maps/boundries/study_area.gpkg")
)

target_crs <- st_crs(study_area)

crossings <-
    st_transform(
        crossings,
        target_crs
    )

ven_roads <-
    oe_read(
        here(
            "Data/raw/OSM/venezuela.osm.pbf"
        ),
        layer = "lines"
    )

col_roads <-
    oe_read(
        here(
            "Data/raw/OSM/colombia.osm.pbf"
        ),
        layer = "lines"
    )

ven_roads <-
    ven_roads |>
    filter(!is.na(highway))

col_roads <-
    col_roads |>
    filter(!is.na(highway))

roads <-
    bind_rows(
        ven_roads,
        col_roads
    ) |>
    st_transform(target_crs)

plot(
    st_geometry(roads),
    col = "grey70"
)

plot(
    st_geometry(crossings),
    add = TRUE,
    pch = 19,
    col = "red"
)

text(
    st_coordinates(crossings),
    labels = crossings$crossing_id,
    pos = 4,
    cex = 0.8
)