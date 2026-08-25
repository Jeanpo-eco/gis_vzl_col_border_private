# ==============================================================================
# Script: 08_border_accessibility.R
#
# Description:
#   1. Loads the municipality-to-crossing road-network distances from Script 07.
#   2. Identifies the five road border crossings used in the analysis.
#   3. Extracts OSM settlement polygons associated with each crossing.
#   4. Calculates settlement nighttime-light intensity using annual VIIRS
#      luminosity for 2013 and 2014.
#   5. Calculates crossing weights following Behrens (2024):
#
#         weight_j =
#         (NTL_col * NTL_ven) / distance_col_ven^2
#
#      The weights are normalized to [0,1].
#
#   6. Combines the crossing weights with network distances to construct
#      weighted border-crossing distance measures.
#
#   7. Saves the crossing weights and final municipality-level accessibility
#      dataset.
#
# ==============================================================================

# ==============================================================================
# 1. Paths and settings
# ==============================================================================

target_crs <- 9377

viirs_dir <- here("Data/viirs")

osm_dir <- here("data/raw/OSM")

output_dir <- here("Output/Bases/access")

fs::dir_create(output_dir)


# ==============================================================================
# 2. Load border crossings
# ==============================================================================

crossings <-

    readRDS(
        here(
            "Output/Bases/border_crossings",
            "border_crossings.rds"
        )
    ) |>

    st_transform(target_crs)


# ------------------------------------------------------------------------------
# Keep only road crossings
#
# Puerto Carreño and Puerto Inírida are river crossings and are therefore
# excluded from the road-network accessibility measure.
# ------------------------------------------------------------------------------

crossings <-

    crossings |>

    filter(
        crossing_name %in% c(
            "Puente Internacional Simón Bolívar",
            "Puente Internacional Francisco de Paula Santander",
            "Puente Unión",
            "Paraguachón",
            "Puente Internacional José Antonio Páez"
        )
    )


# Check
print(crossings)


# ==============================================================================
# 3. Load network distances from Script 07
# ==============================================================================

network_access <-

    readRDS(
        here(
            "Output/Bases/access",
            "network_access.rds"
        )
    )


# ------------------------------------------------------------------------------
# Keep only the five crossings
# ------------------------------------------------------------------------------

network_access <-

    network_access |>

    filter(
        crossing_id %in% crossings$crossing_id
    )


# Check dimensions
message(
    "Number of municipalities: ",
    n_distinct(network_access$municipality_id)
)

message(
    "Number of crossings: ",
    n_distinct(network_access$crossing_id)
)


# ==============================================================================
# 4. Define the settlements associated with each crossing
# ==============================================================================

# ------------------------------------------------------------------------------
# These are the main settlements on each side of the five road crossings.
#
# We use the names already identified in the border-crossing dataset.
# ------------------------------------------------------------------------------

crossing_settlements <-

    tribble(

        ~crossing_id, ~side, ~country,      ~settlement,

        crossings$crossing_id[1],
        "Colombia",
        "Colombia",
        "Cúcuta",

        crossings$crossing_id[1],
        "Venezuela",
        "Venezuela",
        "San Antonio del Táchira",


        crossings$crossing_id[2],
        "Colombia",
        "Colombia",
        "Villa del Rosario",

        crossings$crossing_id[2],
        "Venezuela",
        "Venezuela",
        "Ureña",


        crossings$crossing_id[3],
        "Colombia",
        "Colombia",
        "Puerto Santander",

        crossings$crossing_id[3],
        "Venezuela",
        "Venezuela",
        "Boca de Grita",


        crossings$crossing_id[4],
        "Colombia",
        "Colombia",
        "Maicao",

        crossings$crossing_id[4],
        "Venezuela",
        "Venezuela",
        "Paraguachón",


        crossings$crossing_id[5],
        "Colombia",
        "Colombia",
        "Arauca",

        crossings$crossing_id[5],
        "Venezuela",
        "Venezuela",
        "El Amparo"

    )


# ==============================================================================
# 5. Load OSM settlement polygons
# ==============================================================================

# ------------------------------------------------------------------------------
# Behrens uses settlement polygons from OpenStreetMap.
#
# We extract the multipolygon layer from the two PBF files and retain
# polygons tagged as settlements.
# ------------------------------------------------------------------------------

message("Loading OSM settlement polygons...")


ven_settlements_raw <-

    oe_read(
        here(
            osm_dir,
            "venezuela.osm.pbf"
        ),
        layer = "multipolygons"
    )


col_settlements_raw <-

    oe_read(
        here(
            osm_dir,
            "colombia.osm.pbf"
        ),
        layer = "multipolygons"
    )


# ------------------------------------------------------------------------------
# Keep objects tagged as settlements
# ------------------------------------------------------------------------------

settlement_types <-

    c(
        "city",
        "town",
        "village",
        "hamlet"
    )


ven_settlements <-

    ven_settlements_raw |>

    filter(
        place %in% settlement_types
    ) |>

    select(
        name,
        place,
        geometry
    )


col_settlements <-

    col_settlements_raw |>

    filter(
        place %in% settlement_types
    ) |>

    select(
        name,
        place,
        geometry
    )


rm(
    ven_settlements_raw,
    col_settlements_raw
)


# ==============================================================================
# 6. Match our crossing settlements to OSM polygons
# ==============================================================================

ven_names <-

    crossing_settlements |>
    filter(country == "Venezuela") |>
    pull(settlement)


col_names <-

    crossing_settlements |>
    filter(country == "Colombia") |>
    pull(settlement)


# ------------------------------------------------------------------------------
# Inspect possible matches before filtering
# ------------------------------------------------------------------------------

message("Venezuela settlement matches:")

print(
    ven_settlements |>
        filter(
            str_detect(
                str_to_lower(name),
                str_c(
                    str_to_lower(ven_names),
                    collapse = "|"
                )
            )
        ) |>
        select(name, place)
)


message("Colombia settlement matches:")

print(
    col_settlements |>
        filter(
            str_detect(
                str_to_lower(name),
                str_c(
                    str_to_lower(col_names),
                    collapse = "|"
                )
            )
        ) |>
        select(name, place)
)


# ------------------------------------------------------------------------------
# IMPORTANT:
#
# At this point inspect the printed matches.
#
# OSM sometimes uses slightly different names, accents, or administrative
# suffixes. The exact names below therefore need to correspond to the polygons
# actually returned by your OSM data.
# ------------------------------------------------------------------------------


# ==============================================================================
# 7. Select the associated settlement polygons
# ==============================================================================

# ------------------------------------------------------------------------------
# Normalize names for matching.
# ------------------------------------------------------------------------------

normalize_name <- function(x) {

    x |>
        stringi::stri_trans_general("Latin-ASCII") |>
        str_to_lower() |>
        str_replace_all("[^a-z0-9]", "")

}


ven_settlements <-

    ven_settlements |>

    mutate(
        name_clean = normalize_name(name)
    )


col_settlements <-

    col_settlements |>

    mutate(
        name_clean = normalize_name(name)
    )


crossing_settlements <-

    crossing_settlements |>

    mutate(
        settlement_clean = normalize_name(settlement)
    )


# ------------------------------------------------------------------------------
# Match settlements
# ------------------------------------------------------------------------------

ven_crossing_settlements <-

    crossing_settlements |>

    filter(country == "Venezuela") |>

    left_join(
        ven_settlements,
        by = c(
            "settlement_clean" = "name_clean"
        )
    )


col_crossing_settlements <-

    crossing_settlements |>

    filter(country == "Colombia") |>

    left_join(
        col_settlements,
        by = c(
            "settlement_clean" = "name_clean"
        )
    )


# ------------------------------------------------------------------------------
# Check whether every settlement was matched
# ------------------------------------------------------------------------------

message(
    "Unmatched Venezuelan settlements: ",
    sum(is.na(ven_crossing_settlements$geometry))
)

message(
    "Unmatched Colombian settlements: ",
    sum(is.na(col_crossing_settlements$geometry))
)


print(
    ven_crossing_settlements |>
        st_drop_geometry() |>
        select(
            crossing_id,
            settlement,
            name
        )
)


print(
    col_crossing_settlements |>
        st_drop_geometry() |>
        select(
            crossing_id,
            settlement,
            name
        )
)


# ------------------------------------------------------------------------------
# Stop if a settlement could not be matched.
#
# We do NOT want to silently calculate incorrect weights.
# ------------------------------------------------------------------------------

if (
    any(is.na(ven_crossing_settlements$geometry)) |
    any(is.na(col_crossing_settlements$geometry))
) {

    stop(
        paste0(
            "\n",
            "At least one associated settlement could not be matched ",
            "to an OSM polygon.\n",
            "Inspect the matching output above and adjust the settlement ",
            "name in crossing_settlements."
        )
    )

}


# ==============================================================================
# 8. Prepare settlement polygons
# ==============================================================================

ven_crossing_settlements <-

    ven_crossing_settlements |>

    st_as_sf() |>

    st_transform(4326)


col_crossing_settlements <-

    col_crossing_settlements |>

    st_as_sf() |>

    st_transform(4326)


# ==============================================================================
# 9. Find the 2013 and 2014 annual VIIRS rasters
# ==============================================================================

# ------------------------------------------------------------------------------
# We use the annual VIIRS data for 2013 and 2014.
#
# Behrens uses the average settlement luminosity for these two years.
# ------------------------------------------------------------------------------

viirs_files <-

    fs::dir_ls(
        viirs_dir,
        regexp = "\\.tif$|\\.tif\\.gz$",
        type = "file"
    )


viirs_2013 <-

    viirs_files[
        str_detect(
            basename(viirs_files),
            "2013"
        )
    ]


viirs_2014 <-

    viirs_files[
        str_detect(
            basename(viirs_files),
            "2014"
        )
    ]


if (length(viirs_2013) != 1) {

    stop(
        "Expected exactly one 2013 VIIRS raster, found: ",
        length(viirs_2013)
    )

}


if (length(viirs_2014) != 1) {

    stop(
        "Expected exactly one 2014 VIIRS raster, found: ",
        length(viirs_2014)
    )

}


message(
    "2013 VIIRS: ",
    basename(viirs_2013)
)

message(
    "2014 VIIRS: ",
    basename(viirs_2014)
)


# ==============================================================================
# 10. Load VIIRS rasters
# ==============================================================================

ntl_2013 <-

    rast(
        viirs_2013
    )


ntl_2014 <-

    rast(
        viirs_2014
    )


# ------------------------------------------------------------------------------
# Check CRS
# ------------------------------------------------------------------------------

print(crs(ntl_2013))
print(crs(ntl_2014))


# ==============================================================================
# 11. Function to calculate settlement luminosity
# ==============================================================================

# ------------------------------------------------------------------------------
# For each settlement polygon:
#
#   1. Extract all VIIRS cells intersecting the settlement.
#   2. Sum their luminosity.
#
# This follows the paper's description of calculating the sum of luminosity
# for the cells associated with each settlement.
# ------------------------------------------------------------------------------

settlement_ntl <- function(settlements, raster, year) {

    settlements_vect <-

        vect(
            settlements
        )

    extracted <-

        terra::extract(
            raster,
            settlements_vect,
            fun = sum,
            na.rm = TRUE
        )

    tibble(
        settlement = settlements$settlement,
        crossing_id = settlements$crossing_id,
        country = settlements$country,
        year = year,
        ntl = extracted[[2]]
    )

}


# ==============================================================================
# 12. Calculate 2013 settlement luminosity
# ==============================================================================

ven_ntl_2013 <-

    settlement_ntl(
        ven_crossing_settlements,
        ntl_2013,
        2013
    )


col_ntl_2013 <-

    settlement_ntl(
        col_crossing_settlements,
        ntl_2013,
        2013
    )


# ==============================================================================
# 13. Calculate 2014 settlement luminosity
# ==============================================================================

ven_ntl_2014 <-

    settlement_ntl(
        ven_crossing_settlements,
        ntl_2014,
        2014
    )


col_ntl_2014 <-

    settlement_ntl(
        col_crossing_settlements,
        ntl_2014,
        2014
    )


# ==============================================================================
# 14. Combine settlement luminosity
# ==============================================================================

settlement_ntl <-

    bind_rows(
        ven_ntl_2013,
        ven_ntl_2014,
        col_ntl_2013,
        col_ntl_2014
    )


# ------------------------------------------------------------------------------
# Average 2013–2014 luminosity
# ------------------------------------------------------------------------------

settlement_ntl_average <-

    settlement_ntl |>

    group_by(
        crossing_id,
        country,
        settlement
    ) |>

    summarise(
        ntl_2013_2014 =
            mean(
                ntl,
                na.rm = TRUE
            ),
        .groups = "drop"
    )


print(settlement_ntl_average)


# ==============================================================================
# 15. Calculate settlement centroids
# ==============================================================================

settlement_centroids <-

    bind_rows(
        ven_crossing_settlements,
        col_crossing_settlements
    ) |>

    st_centroid()


# ------------------------------------------------------------------------------
# Convert to WGS84 for geodesic distance
# ------------------------------------------------------------------------------

settlement_centroids <-

    settlement_centroids |>

    st_transform(4326)


# ==============================================================================
# 16. Calculate crossing-level gravity weights
# ==============================================================================

# ------------------------------------------------------------------------------
# For each crossing:
#
#   NTL_col = average 2013–2014 luminosity on Colombian side
#   NTL_ven = average 2013–2014 luminosity on Venezuelan side
#
#   settlement_distance = geodesic distance between settlement centroids
#
#   raw_weight =
#
#       (NTL_col * NTL_ven) /
#       settlement_distance^2
#
# Behrens then normalizes the weights to [0,1].
# ------------------------------------------------------------------------------

crossing_weights <-

    crossing_settlements |>

    select(
        crossing_id,
        side = country,
        settlement
    ) |>

    left_join(
        settlement_ntl_average |>
            select(
                crossing_id,
                country,
                settlement,
                ntl_2013_2014
            ),
        by = c(
            "crossing_id",
            "side" = "country",
            "settlement"
        )
    )


# ------------------------------------------------------------------------------
# Reshape luminosity
# ------------------------------------------------------------------------------

crossing_weights_wide <-

    crossing_weights |>

    select(
        crossing_id,
        side,
        ntl_2013_2014
    ) |>

    pivot_wider(
        names_from = side,
        values_from = ntl_2013_2014,
        names_glue = "ntl_{side}"
    )


# ------------------------------------------------------------------------------
# Get settlement centroid coordinates
# ------------------------------------------------------------------------------

settlement_coordinates <-

    settlement_centroids |>

    st_drop_geometry() |>

    select(
        crossing_id,
        country,
        settlement
    )


# ------------------------------------------------------------------------------
# Calculate geodesic distance between the two settlements
# ------------------------------------------------------------------------------

crossing_weights_distance <-

    crossing_settlements |>

    select(
        crossing_id,
        country,
        settlement
    ) |>

    left_join(
        settlement_centroids |>
            select(
                crossing_id,
                country,
                settlement,
                geometry
            ),
        by = c(
            "crossing_id",
            "country",
            "settlement"
        )
    )


# ------------------------------------------------------------------------------
# Split into Colombia and Venezuela
# ------------------------------------------------------------------------------

col_centroids <-

    crossing_weights_distance |>

    filter(
        country == "Colombia"
    ) |>

    rename(
        geometry_col = geometry,
        settlement_col = settlement
    )


ven_centroids <-

    crossing_weights_distance |>

    filter(
        country == "Venezuela"
    ) |>

    rename(
        geometry_ven = geometry,
        settlement_ven = settlement
    )


# ------------------------------------------------------------------------------
# Pair the two settlements for each crossing
# ------------------------------------------------------------------------------

crossing_pairs <-

    col_centroids |>

    st_drop_geometry() |>

    select(
        crossing_id,
        settlement_col,
        geometry_col
    ) |>

    left_join(
        ven_centroids |>
            st_drop_geometry() |>
            select(
                crossing_id,
                settlement_ven,
                geometry_ven
            ),
        by = "crossing_id"
    )


# ------------------------------------------------------------------------------
# Calculate geodesic distance in metres
# ------------------------------------------------------------------------------

crossing_pairs_sf <-

    crossing_pairs |>

    st_as_sf(
        wkt = "geometry_col",
        crs = 4326
    )


# The geometry columns need to be converted explicitly to sfc objects.
col_geom <-
    st_as_sfc(crossing_pairs$geometry_col)

ven_geom <-
    st_as_sfc(crossing_pairs$geometry_ven)


settlement_distance_m <-

    st_distance(
        col_geom,
        ven_geom,
        by_element = TRUE
    )


crossing_pairs <-

    crossing_pairs |>

    mutate(
        settlement_distance_m =
            as.numeric(
                settlement_distance_m
            )
    )


# ==============================================================================
# 17. Construct raw gravity weights
# ==============================================================================

crossing_weights <-

    crossing_pairs |>

    left_join(
        crossing_weights_wide,
        by = "crossing_id"
    ) |>

    mutate(

        raw_weight =

            (
                ntl_Colombia *
                ntl_Venezuela
            ) /

            (
                settlement_distance_m^2
            )

    )


# ==============================================================================
# 18. Normalize weights to [0,1]
# ==============================================================================

max_weight <-

    max(
        crossing_weights$raw_weight,
        na.rm = TRUE
    )


crossing_weights <-

    crossing_weights |>

    mutate(

        crossing_weight =
            raw_weight / max_weight

    )


# ------------------------------------------------------------------------------
# Add crossing names
# ------------------------------------------------------------------------------

crossing_weights <-

    crossing_weights |>

    left_join(

        crossings |>

            st_drop_geometry() |>

            select(
                crossing_id,
                crossing_name,
                crossing_type
            ),

        by = "crossing_id"

    )


# ------------------------------------------------------------------------------
# Inspect the weights
# ------------------------------------------------------------------------------

print(
    crossing_weights |>

        select(
            crossing_id,
            crossing_name,
            settlement_col,
            settlement_ven,
            ntl_Colombia,
            ntl_Venezuela,
            settlement_distance_m,
            raw_weight,
            crossing_weight
        )
)


# ==============================================================================
# 19. Save crossing weights
# ==============================================================================

write_csv(

    crossing_weights |>

        select(
            crossing_id,
            crossing_name,
            crossing_type,
            settlement_col,
            settlement_ven,
            ntl_Colombia,
            ntl_Venezuela,
            settlement_distance_m,
            raw_weight,
            crossing_weight
        ),

    here(
        output_dir,
        "border_crossing_weights.csv"
    )

)


saveRDS(

    crossing_weights,

    here(
        output_dir,
        "border_crossing_weights.rds"
    )

)


# ==============================================================================
# 20. Add crossing weights to network distances
# ==============================================================================

network_access_weighted <-

    network_access |>

    left_join(

        crossing_weights |>

            st_drop_geometry() |>

            select(
                crossing_id,
                crossing_name,
                crossing_weight
            ),

        by = "crossing_id"

    )


# ==============================================================================
# 21. Construct weighted crossing distance
# ==============================================================================

# ------------------------------------------------------------------------------
# Behrens uses the crossing weights as multipliers for distance changes.
#
# For our baseline accessibility measure we retain:
#
#   1. raw network distance
#   2. weight-adjusted distance
#
# A larger crossing weight means that the crossing is more important.
# Therefore we define:
#
#   weighted_distance = distance / crossing_weight
#
# so that an important crossing has a lower effective distance.
#
# We retain the original distance as well because it is the direct network
# measure and should be the primary variable.
# ------------------------------------------------------------------------------

network_access_weighted <-

    network_access_weighted |>

    mutate(

        distance_km =
            distance_m / 1000,

        weighted_distance_m =

            if_else(
                crossing_weight > 0,
                distance_m / crossing_weight,
                NA_real_
            ),

        weighted_distance_km =
            weighted_distance_m / 1000

    )


# ==============================================================================
# 22. Construct municipality-level nearest-crossing measures
# ==============================================================================

# ------------------------------------------------------------------------------
# Unweighted:
#
# distance to nearest road border crossing.
# ------------------------------------------------------------------------------

municipality_access <-

    network_access_weighted |>

    group_by(
        municipality_id
    ) |>

    summarise(

        min_distance_m =

            if (
                all(is.na(distance_m))
            ) {
                NA_real_
            } else {
                min(
                    distance_m,
                    na.rm = TRUE
                )
            },

        min_distance_km =

            min_distance_m / 1000,

        nearest_crossing_id =

            crossing_id[
                which.min(
                    if_else(
                        is.na(distance_m),
                        Inf,
                        distance_m
                    )
                )
            ],

        min_weighted_distance_m =

            if (
                all(is.na(weighted_distance_m))
            ) {
                NA_real_
            } else {
                min(
                    weighted_distance_m,
                    na.rm = TRUE
                )
            },

        min_weighted_distance_km =
            min_weighted_distance_m / 1000,

        .groups = "drop"

    )


# ==============================================================================
# 23. Add municipality information
# ==============================================================================

municipalities <-

    readRDS(
        here(
            "Output/maps/boundries",
            "study_area.rds"
        )
    )


municipality_access <-

    municipalities |>

    st_drop_geometry() |>

    select(
        municipality_id,
        COUNTRY,
        NAME_1,
        NAME_2
    ) |>

    left_join(
        municipality_access,
        by = "municipality_id"
    )


# ==============================================================================
# 24. Add nearest crossing name
# ==============================================================================

municipality_access <-

    municipality_access |>

    left_join(

        crossings |>

            st_drop_geometry() |>

            select(
                crossing_id,
                crossing_name
            ) |>

            rename(
                nearest_crossing_id = crossing_id,
                nearest_crossing_name = crossing_name
            ),

        by = "nearest_crossing_id"

    )


# ==============================================================================
# 25. Save final municipality accessibility dataset
# ==============================================================================

saveRDS(

    municipality_access,

    here(
        output_dir,
        "municipality_accessibility.rds"
    )

)


write_csv(

    municipality_access,

    here(
        output_dir,
        "municipality_accessibility.csv"
    )

)


# ==============================================================================
# 26. Save full municipality × crossing dataset
# ==============================================================================

saveRDS(

    network_access_weighted,

    here(
        output_dir,
        "network_access_weighted.rds"
    )

)


write_csv(

    network_access_weighted,

    here(
        output_dir,
        "network_access_weighted.csv"
    )

)


# ==============================================================================
# 27. Diagnostics
# ==============================================================================

message("")
message("==============================================")
message("SCRIPT 08 COMPLETE")
message("==============================================")

message(
    "Municipalities: ",
    nrow(municipality_access)
)

message(
    "Road crossings: ",
    nrow(crossings)
)

message(
    "Municipality × crossing observations: ",
    nrow(network_access_weighted)
)

message("")
message("Crossing weights:")
print(
    crossing_weights |>
        select(
            crossing_id,
            crossing_name,
            crossing_weight
        )
)

message("")
message("Accessibility summary:")
print(
    summary(
        municipality_access$min_distance_km
    )
)

message("")
message("Missing municipality accessibility:")
print(
    sum(
        is.na(
            municipality_access$min_distance_km
        )
    )
)

message("")
message("Output files written to:")
message(output_dir)