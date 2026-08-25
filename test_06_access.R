# ==============================================================================
# Script: 07_variables.R
# Description:
# Computes shortest-path network distances from every municipality to every
# official border crossing using the road network built in Script 06.
# ==============================================================================

roads <-
    readRDS(
        here(
            "Output/Bases/roads",
            "roads.rds"
        )
    )

graph <-
    readRDS(
        here(
            "Output/network",
            "road_network.rds"
        )
    )

analysis_area <-
    readRDS(
        here(
            "Output/maps/boundries",
            "analysis_area.rds"
        )
    )

crossings <-
    readRDS(
        here(
            "Output/Bases/border_crossings",
            "border_crossings.rds"
        )
    ) %>%
    filter(crossing_type != "River")

municipalities <-

    analysis_area |>

    st_point_on_surface() |>

    select(
        municipality_id,
        COUNTRY,
        NAME_1,
        NAME_2,
        geometry
    )

largest_component <-

    graph |>

    count(component, sort = TRUE) |>

    slice(1) |>

    pull(component)

graph <-

    graph |>

    filter(component == largest_component)

edges <-

    graph |>

    transmute(

        from_id,

        to_id,

        cost = d_weighted,

        from_lon,

        from_lat,

        to_lon,

        to_lat

    )

nodes <-

    bind_rows(

        edges |>

            select(

                node_id = from_id,

                lon = from_lon,

                lat = from_lat

            ),

        edges |>

            select(

                node_id = to_id,

                lon = to_lon,

                lat = to_lat

            )

    ) |>

    distinct(node_id, .keep_all = TRUE)

node_lookup <-

    nodes |>

    mutate(

        cpp_id = row_number()

    )

edges <-

    edges |>

    left_join(
        node_lookup |>
            select(node_id, from = cpp_id),
        by = c("from_id" = "node_id")
    ) |>

    left_join(
        node_lookup |>
            select(node_id, to = cpp_id),
        by = c("to_id" = "node_id")
    )

cpp_graph <-

    makegraph(

        edges |>

            select(
                from,
                to,
                cost
            ),

        directed = FALSE

    )

nodes_sf <-

    node_lookup |>

    st_as_sf(

        coords = c("lon", "lat"),

        crs = st_crs(roads)

    )

municipalities <-
    municipalities |>
    st_transform(st_crs(nodes_sf))

crossings <-
    crossings |>
    st_transform(st_crs(nodes_sf))

municipality_nodes <-

    st_nearest_feature(
        municipalities,
        nodes_sf
    )

crossing_nodes <-

    st_nearest_feature(
        crossings,
        nodes_sf
    )

municipality_cpp <-

    node_lookup$cpp_id[
        municipality_nodes
    ]

crossing_cpp <-

    node_lookup$cpp_id[
        crossing_nodes
    ]

dist_matrix <-

    get_distance_matrix(

        Graph = cpp_graph,

        from = municipality_cpp,

        to = crossing_cpp

    )

dist_panel <- as.data.frame(dist_matrix)

dist_panel <-

    bind_cols(

        municipalities |>

            st_drop_geometry() |>

            select(
                municipality_id
            ),

        dist_panel

    )

network_access <-

    dist_panel |>

    pivot_longer(

        cols = -municipality_id,

        names_to = "crossing_id",

        values_to = "distance_m"

    ) |>

    mutate(

        crossing_id = as.integer(crossing_id)

    )

network_access <-

    network_access |>

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

# nrow(municipalities)
# dim(dist_matrix)
# summary(network_access)

# which(is.na(municipality_cpp))

# which(is.na(crossing_cpp))
# rowSums(is.na(dist_matrix))
# colSums(is.na(dist_matrix))
# crossings$crossing_name
# crossing_cpp

# municipalities |>
#     st_drop_geometry() |>
#     mutate(

#         missing = rowSums(is.na(dist_matrix)) == 5

#     ) |>
#     filter(missing) |>
#     select(

#         municipality_id,
#         COUNTRY,
#         NAME_1,
#         NAME_2

#     )

# graph |>
#     filter(
#         from_id == node_lookup$node_id[crossing_nodes[6]] |
#         to_id   == node_lookup$node_id[crossing_nodes[6]]
#     ) |>
#     distinct(component)

fs::dir_create(
    here(
        "Output/Bases/access"
    )
)

municipalities_snapped <-

    municipalities |>

    mutate(

        cpp_node = municipality_cpp

    )

saveRDS(

    municipalities_snapped,

    here(
        "Output/Bases/access",
        "municipality_network_points.rds"
    )
)

st_write(

    municipalities_snapped,

    here(
        "Output/Bases/access",
        "municipality_network_points.gpkg"
    ),

    delete_dsn = TRUE,
    quiet = TRUE
)

crossings_snapped <-

    crossings |>

    mutate(

        cpp_node = crossing_cpp

    )

saveRDS(

    crossings_snapped,

    here(
        "Output/Bases/access",
        "crossing_network_points.rds"
    )
)

st_write(

    crossings_snapped,

    here(
        "Output/Bases/access",
        "crossing_network_points.gpkg"
    ),

    delete_dsn = TRUE,
    quiet = TRUE
)

saveRDS(

    dist_matrix,

    here(
        "Output/Bases/access",
        "network_distance_matrix.rds"
    )
)

write.csv(

    dist_matrix,

    here(
        "Output/Bases/access",
        "network_distance_matrix.csv"
    ),

    row.names = TRUE
)
saveRDS(

    network_access,

    here(
        "Output/Bases/access",
        "network_access.rds"
    )
)

write_csv(

    network_access,

    here(
        "Output/Bases/access",
        "network_access.csv"
    )
)