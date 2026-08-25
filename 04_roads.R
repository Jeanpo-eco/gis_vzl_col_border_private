# ==============================================================================
# Script: 04_roads.R
# Description:
#   Downloads and prepares the Colombia-Venezuela road network from OSM,
#   restricts it to the analysis area, and constructs a motorcar-weighted
#   routing network using dodgr.
# ==============================================================================
# ------------------------------------------------------------------------------
# 1. Output directories
# ------------------------------------------------------------------------------

fs::dir_create(
  here("Output/Bases/roads"),
  recurse = TRUE
)

fs::dir_create(
  here("Output/network"),
  recurse = TRUE
)

# ------------------------------------------------------------------------------
# 2. Load OSM road data
# ------------------------------------------------------------------------------

ven_roads <- oe_read(
  here("data/raw/OSM/venezuela.osm.pbf"),
  layer = "lines"
)

col_roads <- oe_read(
  here("data/raw/OSM/colombia.osm.pbf"),
  layer = "lines"
)

# ------------------------------------------------------------------------------
# 3. Retain relevant road classes
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

ven_roads <- ven_roads %>%
  filter(highway %in% road_types)

col_roads <- col_roads %>%
  filter(highway %in% road_types)

roads <- bind_rows(
  ven_roads,
  col_roads
)

rm(ven_roads, col_roads)

# ------------------------------------------------------------------------------
# 4. Restrict roads to analysis area
# ------------------------------------------------------------------------------

analysis_area <- readRDS(
  here(
    "Output/maps/boundries",
    "analysis_area.rds"
  )
)

analysis_area <- analysis_area %>%
  st_transform(st_crs(roads)) %>%
  st_union()

roads <- st_make_valid(roads)

roads <- st_intersection(
  roads,
  analysis_area
)

# Keep only valid LINESTRING geometries
roads <- roads %>%
  st_collection_extract("LINESTRING") %>%
  st_cast("LINESTRING") %>%
  filter(!st_is_empty(.))

# ------------------------------------------------------------------------------
# 5. Keep required attributes and create stable road ID
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

# ------------------------------------------------------------------------------
# 6. Save clipped road layer
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

# ------------------------------------------------------------------------------
# 7. Construct motorcar routing network
# ------------------------------------------------------------------------------

# The raw road layer retains all selected OSM road classes, including "track".
# The motorcar routing profile determines which road classes are considered
# routable for the baseline accessibility measure.

graph <- dodgr::weight_streetnet(
  roads,
  wt_profile = "motorcar"
)

# Recompute connected components after applying the motorcar profile.
#
# weight_streetnet() can remove non-routable road classes such as "track".
# Components therefore need to be recalculated on the final routable graph,
# rather than relying on component labels inherited from an earlier topology.

graph <- graph %>%
  select(-component) %>%
  dodgr::dodgr_components()

# ------------------------------------------------------------------------------
# 8. Save routing network
# ------------------------------------------------------------------------------

saveRDS(
  graph,
  here(
    "Output/network",
    "road_network.rds"
  )
)

message(
  "Script 04 finished successfully: clipped OSM roads and motorcar routing network saved."
)