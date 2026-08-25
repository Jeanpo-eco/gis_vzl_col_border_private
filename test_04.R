fs::dir_create(
  here("Output/Bases/roads")
)

ven_roads <- oe_read(
  here("data/raw/OSM/venezuela.osm.pbf"),
  layer = "lines"
)

col_roads <- oe_read(
  here("data/raw/OSM/colombia.osm.pbf"),
  layer = "lines"
)

# ------------------------------------------------------------------------------
# Filters
# ------------------------------------------------------------------------------

road_types <- c(
  "motorway",
  "motorway_link",
  "trunk",
  "trunk_link",
  "primary",
  "primary_link",
  "secondary",
  "secondary_link",
  "tertiary",
  "tertiary_link",
  "unclassified",
  "residential",
  "living_street",
  "service",
  "track"
)

ven_roads <- ven_roads |>
  filter(highway %in% road_types)

col_roads <- col_roads |>
  filter(highway %in% road_types)

roads <- bind_rows(
  ven_roads,
  col_roads
)

rm(ven_roads, col_roads)


# ==============================================================================
# TEST 1 — Are tracks present immediately after the OSM filter?
# ==============================================================================

roads %>%
  st_drop_geometry() %>%
  count(highway, sort = TRUE)

roads %>%
  filter(highway == "track") %>%
  summarise(
    n_tracks = n(),
    n_osm_ids = n_distinct(osm_id)
  )


# ------------------------------------------------------------------------------
# Analysis area
# ------------------------------------------------------------------------------

analysis_area <- readRDS(
    here(
        "Output/maps/boundries/",
        "analysis_area.rds"))

analysis_area <- st_transform(
  analysis_area,
  st_crs(roads)
) %>%
  st_union()

roads <- st_make_valid(roads)

roads <- st_intersection(
  roads,
  analysis_area
)

roads <-
    roads |>
    st_collection_extract("LINESTRING") |>
    st_cast("LINESTRING")

# ==============================================================================
# TEST 2 — Are tracks still present after clipping/intersection?
# ==============================================================================

roads %>%
  st_drop_geometry() %>%
  count(highway, sort = TRUE)

roads %>%
  st_drop_geometry() %>%
  filter(highway == "track") %>%
  summarise(
    n_tracks = n(),
    n_osm_ids = n_distinct(osm_id)
  )


# ------------------------------------------------------------------------------
# Create road IDs
# ------------------------------------------------------------------------------

roads <- roads %>%
  select(
    osm_id,
    highway,
    name,
    geometry
  ) %>%
  mutate(
    road_id = row_number()
  )


# ==============================================================================
# TEST 3 — Final roads object immediately before dodgr
# ==============================================================================

roads %>%
  st_drop_geometry() %>%
  count(highway, sort = TRUE)

roads %>%
  filter(highway == "track") %>%
  st_drop_geometry() %>%
  summarise(
    n_tracks = n(),
    n_osm_ids = n_distinct(osm_id)
  )


# ==============================================================================
# TEST 4 — Specifically inspect the problematic Atabapo road
# ==============================================================================

roads %>%
  filter(osm_id == 690433442) %>%
  st_drop_geometry() %>%
  select(
    road_id,
    osm_id,
    highway,
    name
  )


# ------------------------------------------------------------------------------
# Save roads
# ------------------------------------------------------------------------------

saveRDS(
  roads,
  here(
    "Output/Bases/roads",
    "roads.rds"
  )
)

st_write(
  roads,
  here(
    "Output/Bases/roads",
    "roads.gpkg"
  ),
  delete_dsn = TRUE,
  quiet = TRUE
)
# ============================================================
# DIAGNOSTIC 2: Verify specific OSM road exists
# ============================================================

roads %>%
  filter(osm_id == 690433442) %>%
  st_drop_geometry() %>%
  select(
    road_id,
    osm_id,
    highway,
    name
  )

# ==============================================================================
# TEST 5 — Create motorcar network
# ==============================================================================

graph <-
  dodgr::weight_streetnet(
    roads,
    wt_profile = "motorcar"
  )


# ==============================================================================
# TEST 6 — Did track survive weight_streetnet()?
# ==============================================================================

graph %>%
  count(highway, sort = TRUE)


# Specifically:

graph %>%
  filter(highway == "track") %>%
  summarise(
    n_track_edges = n(),
    n_track_way_ids = n_distinct(way_id)
  )


# ==============================================================================
# TEST 7 — Check the problematic Atabapo OSM way
# ==============================================================================

graph %>%
  filter(way_id == 690433442) %>%
  select(
    edge_id,
    from_id,
    to_id,
    way_id,
    highway,
    d,
    d_weighted
  )

# ============================================================
# DIAGNOSTIC 4: Which OSM ways survive into the network?
# ============================================================

roads %>%
  st_drop_geometry() %>%
  distinct(osm_id, highway) %>%
  mutate(
    in_graph = osm_id %in% graph$way_id
  ) %>%
  count(highway, in_graph) %>%
  arrange(highway, in_graph)

# ============================================================
# DIAGNOSTIC 5: Are nearby roads routable?
# ============================================================

problem_osm_ids <- c(
  690433442,
  1112563622,
  134815657,
  1091251931,
  199790374,
  256420937,
  108006921,
  1514014133
)

roads %>%
  st_drop_geometry() %>%
  filter(osm_id %in% problem_osm_ids) %>%
  select(
    osm_id,
    highway,
    name
  ) %>%
  mutate(
    in_graph = osm_id %in% graph$way_id
  )

# ------------------------------------------------------------------------------
# Save network
# ------------------------------------------------------------------------------

saveRDS(
  graph,
  here(
    "Output/network",
    "road_network.rds"
  )
)