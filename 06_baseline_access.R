# ==============================================================================
#   Calculates shortest motorcar road-network distances from each administrative
#   unit in the analysis area to each of the five official border crossings.
#
#   The road network is constructed in Script 04 using dodgr's "motorcar"
#   weighting profile, and connected components are recomputed after weighting.
#
#   Municipalities are first snapped to the nearest node in the FULL motorcar
#   network. Only municipalities belonging to the same connected component as
#   the border crossings are routed.
#
#   Municipalities in other network components remain in the output, but their
#   municipality-crossing distances are recorded as NA.
#
#   No geodesic fallback and no invented travel times are used.
# ==============================================================================

# ------------------------------------------------------------------------------
# Parameters
# ------------------------------------------------------------------------------

target_crs <- 9377

# ------------------------------------------------------------------------------
# Load spatial inputs
# ------------------------------------------------------------------------------

analysis_area <- readRDS(
  here(
    "Output/maps/boundries",
    "analysis_area.rds"
  )
) %>%
  st_transform(target_crs)

crossings <- readRDS(
  here(
    "Output/Bases/border_crossings",
    "border_crossings.rds"
  )
) %>%
  st_transform(target_crs)


# ------------------------------------------------------------------------------
# Validate crossing dataset
# ------------------------------------------------------------------------------

if (nrow(crossings) != 5) {
  stop(
    "Expected exactly 5 border crossings, but found ",
    nrow(crossings),
    "."
  )
}

crossings <- crossings %>%
  mutate(
    crossing_id = as.character(crossing_id)
  )

if (anyDuplicated(crossings$crossing_id) > 0) {
  stop("crossing_id must be unique.")
}


# ------------------------------------------------------------------------------
# Create municipality representative points
# ------------------------------------------------------------------------------

municipality_points <- analysis_area %>%
  st_point_on_surface() %>%
  mutate(
    municipality_id = as.character(municipality_id)
  )

if (anyDuplicated(municipality_points$municipality_id) > 0) {
  stop("municipality_id must be unique in analysis_area.")
}


# ------------------------------------------------------------------------------
# Load final motorcar network
# ------------------------------------------------------------------------------

network <- readRDS(
  here(
    "Output/network",
    "road_network.rds"
  )
)

required_network_cols <- c(
  "from_id",
  "to_id",
  "from_lon",
  "from_lat",
  "to_lon",
  "to_lat",
  "d",
  "component"
)

missing_network_cols <- setdiff(
  required_network_cols,
  names(network)
)

if (length(missing_network_cols) > 0) {
  stop(
    "The network is missing required columns: ",
    paste(missing_network_cols, collapse = ", ")
  )
}


# ------------------------------------------------------------------------------
# Prepare full motorcar edge table
# ------------------------------------------------------------------------------

# d is the physical edge length in metres.
# The motorcar profile has already determined which roads are routable.

edges <- network %>%
  transmute(
    from_id = as.character(from_id),
    to_id = as.character(to_id),
    component = as.integer(component),
    dist = as.numeric(d),
    from_lon,
    from_lat,
    to_lon,
    to_lat
  ) %>%
  filter(
    !is.na(from_id),
    !is.na(to_id),
    !is.na(component),
    !is.na(dist),
    is.finite(dist),
    dist >= 0,
    !is.na(from_lon),
    !is.na(from_lat),
    !is.na(to_lon),
    !is.na(to_lat)
  )


# ------------------------------------------------------------------------------
# Build spatial node table from FULL motorcar network
# ------------------------------------------------------------------------------

nodes_from <- edges %>%
  select(
    node_id = from_id,
    lon = from_lon,
    lat = from_lat,
    component
  )

nodes_to <- edges %>%
  select(
    node_id = to_id,
    lon = to_lon,
    lat = to_lat,
    component
  )

node_df <- bind_rows(
  nodes_from,
  nodes_to
) %>%
  distinct(
    node_id,
    .keep_all = TRUE
  )

if (anyDuplicated(node_df$node_id) > 0) {
  stop("Network node IDs are not unique.")
}

nodes_sf <- node_df %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  ) %>%
  st_transform(target_crs)


# ------------------------------------------------------------------------------
# Snap municipality points and crossings to FULL motorcar network
# ------------------------------------------------------------------------------

municipality_idx <- st_nearest_feature(
  municipality_points,
  nodes_sf
)

crossing_idx <- st_nearest_feature(
  crossings,
  nodes_sf
)

municipality_node_ids <- as.character(
  nodes_sf$node_id[municipality_idx]
)

crossing_node_ids <- as.character(
  nodes_sf$node_id[crossing_idx]
)

municipality_components <- nodes_sf$component[
  municipality_idx
]

crossing_components <- nodes_sf$component[
  crossing_idx
]

if (any(is.na(municipality_node_ids))) {
  stop(
    "At least one municipality could not be snapped to the motorcar network."
  )
}

if (any(is.na(crossing_node_ids))) {
  stop(
    "At least one crossing could not be snapped to the motorcar network."
  )
}


# ------------------------------------------------------------------------------
# Calculate snap distances
# ------------------------------------------------------------------------------

municipality_snap_distance <- st_distance(
  municipality_points,
  nodes_sf[municipality_idx, ],
  by_element = TRUE
)

crossing_snap_distance <- st_distance(
  crossings,
  nodes_sf[crossing_idx, ],
  by_element = TRUE
)


# ------------------------------------------------------------------------------
# Create municipality and crossing node maps
# ------------------------------------------------------------------------------

municipality_node_map <- municipality_points %>%
  st_drop_geometry() %>%
  transmute(
    municipality_id = as.character(municipality_id),
    node_id = municipality_node_ids,
    network_component = municipality_components,
    snap_distance_m = as.numeric(municipality_snap_distance)
  )

crossing_node_map <- crossings %>%
  st_drop_geometry() %>%
  transmute(
    crossing_id = as.character(crossing_id),
    crossing_name,
    node_id = crossing_node_ids,
    network_component = crossing_components,
    snap_distance_m = as.numeric(crossing_snap_distance)
  )


# ------------------------------------------------------------------------------
# Identify border-crossing network component
# ------------------------------------------------------------------------------

n_crossing_components <- n_distinct(
  crossing_node_map$network_component
)

if (n_crossing_components != 1) {
  stop(
    "The five border crossings do not belong to the same motorcar-network component."
  )
}

crossing_component <- unique(
  crossing_node_map$network_component
)

message(
  "Border crossings belong to network component: ",
  crossing_component
)


# ------------------------------------------------------------------------------
# Determine which municipalities are routable to the crossings
# ------------------------------------------------------------------------------

municipality_node_map <- municipality_node_map %>%
  mutate(
    network_reachable =
      network_component == crossing_component
  )

n_routable <- sum(
  municipality_node_map$network_reachable
)

n_unroutable <- sum(
  !municipality_node_map$network_reachable
)

message(
  "Municipalities in crossing component: ",
  n_routable
)

message(
  "Municipalities outside crossing component: ",
  n_unroutable
)


# ------------------------------------------------------------------------------
# Build cppRouting graph using crossing component only
# ------------------------------------------------------------------------------

routing_edges <- edges %>%
  filter(
    component == crossing_component
  )

cpp_graph <- makegraph(
  df = routing_edges %>%
    select(
      from_id,
      to_id,
      dist
    ),
  directed = FALSE
)


# ------------------------------------------------------------------------------
# Calculate distances for routable municipalities only
# ------------------------------------------------------------------------------

routable_municipality_ids <- municipality_node_map %>%
  filter(network_reachable) %>%
  pull(municipality_id)

routable_node_ids <- municipality_node_map %>%
  filter(network_reachable) %>%
  pull(node_id)

dist_matrix_routable <- get_distance_matrix(
  Graph = cpp_graph,
  from = routable_node_ids,
  to = crossing_node_ids
)

if (
  nrow(dist_matrix_routable) != length(routable_node_ids) ||
  ncol(dist_matrix_routable) != length(crossing_node_ids)
) {
  stop(
    "Unexpected dimensions returned by get_distance_matrix()."
  )
}


# ------------------------------------------------------------------------------
# Reconstruct complete municipality × crossing matrix
# ------------------------------------------------------------------------------

dist_matrix <- matrix(
  NA_real_,
  nrow = nrow(municipality_points),
  ncol = nrow(crossings)
)

rownames(dist_matrix) <- as.character(
  municipality_points$municipality_id
)

colnames(dist_matrix) <- as.character(
  crossings$crossing_id
)

dist_matrix[
  match(
    routable_municipality_ids,
    rownames(dist_matrix)
  ),
  seq_len(ncol(dist_matrix))
] <- dist_matrix_routable


# ------------------------------------------------------------------------------
# Convert distance matrix to long format
# ------------------------------------------------------------------------------

travel_distances_long <- as.data.frame(
  dist_matrix
) %>%
  rownames_to_column(
    var = "municipality_id"
  ) %>%
  pivot_longer(
    cols = -municipality_id,
    names_to = "crossing_id",
    values_to = "distance_m"
  ) %>%
  mutate(
    municipality_id = as.character(municipality_id),
    crossing_id = as.character(crossing_id),
    distance_m = as.numeric(distance_m),
    distance_km = distance_m / 1000
  ) %>%
  left_join(
    crossings %>%
      st_drop_geometry() %>%
      select(
        crossing_id,
        crossing_name
      ),
    by = "crossing_id"
  )


# ------------------------------------------------------------------------------
# Create municipality-level network-quality table
# ------------------------------------------------------------------------------

network_quality <- travel_distances_long %>%
  group_by(
    municipality_id
  ) %>%
  summarise(
    n_crossings = n(),
    n_reachable_crossings = sum(!is.na(distance_m)),
    n_missing_crossings = sum(is.na(distance_m)),
    .groups = "drop"
  ) %>%
  left_join(
    municipality_node_map,
    by = "municipality_id"
  ) %>%
  left_join(
    analysis_area %>%
      st_drop_geometry() %>%
      transmute(
        municipality_id = as.character(municipality_id),
        COUNTRY,
        NAME_1,
        NAME_2
      ),
    by = "municipality_id"
  )


# ------------------------------------------------------------------------------
# Diagnostics / Can be skipped, next section is for saving outputs
# ------------------------------------------------------------------------------

# municipality_count <- n_distinct(
#   travel_distances_long$municipality_id
# )

# crossing_count <- n_distinct(
#   travel_distances_long$crossing_id
# )

# pair_count <- nrow(
#   travel_distances_long
# )

# missing_pairs <- sum(
#   is.na(travel_distances_long$distance_m)
# )

# unreachable_municipalities <- network_quality %>%
#   filter(
#     !network_reachable
#   )

# message("Municipality count: ", municipality_count)
# message("Crossing count: ", crossing_count)
# message("Municipality-crossing pairs: ", pair_count)
# message("Disconnected municipality-crossing pairs: ", missing_pairs)

# message(
#   "Municipalities outside crossing network component: ",
#   nrow(unreachable_municipalities)
# )

# message("Municipality snap-distance summary (m):")

# print(
#   summary(
#     network_quality$snap_distance_m
#   )
# )

# message("Crossing snap-distance summary (m):")

# print(
#   summary(
#     crossing_node_map$snap_distance_m
#   )
# )

# message("Network-distance summary (km):")

# print(
#   summary(
#     travel_distances_long$distance_km
#   )
# )

# message("Municipalities by network component:")

# print(
#   network_quality %>%
#     count(
#       network_component,
#       sort = TRUE
#     )
# )

# if (nrow(unreachable_municipalities) > 0) {

#   message(
#     "Municipalities outside the border-crossing component:"
#   )

#   print(
#     unreachable_municipalities %>%
#       select(
#         municipality_id,
#         COUNTRY,
#         NAME_1,
#         NAME_2,
#         network_component,
#         snap_distance_m
#       ) %>%
#       arrange(
#         network_component,
#         desc(snap_distance_m)
#       )
#   )
# }


# ------------------------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------------------------

output_dir <- here(
  "Output/Bases/access"
)

fs::dir_create(
  output_dir,
  recurse = TRUE
)

saveRDS(
  municipality_node_map,
  file.path(
    output_dir,
    "municipality_node_map.rds"
  )
)

saveRDS(
  crossing_node_map,
  file.path(
    output_dir,
    "crossing_node_map.rds"
  )
)

saveRDS(
  network_quality,
  file.path(
    output_dir,
    "municipality_network_quality.rds"
  )
)

write_csv(
  network_quality,
  file.path(
    output_dir,
    "municipality_network_quality.csv"
  )
)

saveRDS(
  travel_distances_long,
  file.path(
    output_dir,
    "baseline_travel_distances.rds"
  )
)

write_csv(
  travel_distances_long,
  file.path(
    output_dir,
    "baseline_travel_distances.csv"
  )
)

