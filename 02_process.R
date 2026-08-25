# Script to process the raw data downloaded in 01_raw_data.R

# GADM data -------------------------------------------------
ven <- terra::vect(
  here("Data/maps/gadm/gadm/gadm41_VEN_2_pk.rds")
)

col <- terra::vect(
  here("Data/maps/gadm/gadm/gadm41_COL_2_pk.rds")
)

ven <- st_as_sf(ven)
col <- st_as_sf(col)

# Validate
ven <- st_make_valid(ven)

col <- st_make_valid(col)

# Transform to proper CRS
target_crs <- 9377

ven <- st_transform(ven, target_crs)

col <- st_transform(col, target_crs)

ven <- ven |>
  mutate(
    municipality_id = GID_2
  )

col <- col |>
  mutate(
    municipality_id = GID_2
  )

# Save the process
st_write(
    ven,
    here(
      "Output/maps/boundries/venezuela_municipalities.gpkg"
    ),
    delete_dsn = TRUE
)
saveRDS(
    ven,
    here(
        "Output/maps/boundries/venezuela_municipalities.rds"
    )
)

st_write(
    col,
    here(
      "Output/maps/boundries/colombia_municipalities.gpkg"
    ),
    delete_dsn = TRUE
)
saveRDS(
    col,
    here(
        "Output/maps/boundries/colombia_municipalities.rds"
    )
)

# names(ven)
# names(col)

# nrow(ven)
# nrow(col)

# Boundries of Venezuela and Colombia ------------------------------

study_area <-
  bind_rows(
    ven,
    col
  )

municipality_metadata <-
    study_area %>%
    st_drop_geometry() %>%
    select(
        municipality_id,
        COUNTRY,
        NAME_1,
        NAME_2,
        GID_1,
        GID_2
    )

countries <-readRDS(here(
            "Data/raw/countries.rds"
  )
)

ven_country <-
    countries %>%
    filter(admin == "Venezuela") %>%
    st_transform(
        target_crs
    )

col_country <-
    countries %>%
    filter(admin == "Colombia") %>%
    st_transform(
        target_crs
    )

border <-
    st_intersection(
        st_boundary(ven_country),
        st_boundary(col_country)
    )

border_buffer <-

    border |>

    st_transform(3857) |>

    st_buffer(100000) |>

    st_transform(target_crs)

analysis_area <- study_area %>%
  st_filter(
        border_buffer
    )

# Check
plot(st_geometry(ven_country))

plot(
    st_geometry(border),
    add = TRUE,
    col = "red",
    lwd = 3
) # The boundry seems nice

plot(st_geometry(border_buffer))

plot(
    st_geometry(analysis_area),
    add = TRUE,
    col = "lightblue"
)

# Save the relevant data ---------------------------------------------

readr::write_csv(
  municipality_metadata,
  here(
    "Output/maps/boundries",
    "municipality_metadata.csv"
  )
)

# Study area

saveRDS(
  study_area,
  here(
    "Output/maps/boundries/study_area.rds"
  )
)

st_write(
  study_area,
  here(
    "Output/maps/boundries/study_area.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

# Border

saveRDS(
  border,
  here(
    "Output/maps/boundries/border.rds"
  )
)

st_write(
  border,
  here(
    "Output/maps/boundries/border.gpkg"),
  delete_dsn = TRUE,
  quiet = TRUE
)

saveRDS(

    analysis_area,

    here(
        "Output/maps/boundries/",
        "analysis_area.rds"
    )

)

saveRDS(

    border_buffer,

    here(
        "Output/maps/boundries/",
        "border_buffer_100km.rds"
    )

)

rm(
  ven,
  col,
  study_area,
  ven_country,
  col_country,
  countries,
  municipality_metadata,
  ven_centroids,
  col_centroids,
  border
)

gc()
