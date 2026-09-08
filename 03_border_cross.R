# ==============================================================================
# Script for delimiting the five official vehicular crossings between Venezuela and Colombia.
# This script creates the spatial crossing dataset only; no historical status or treatment logic.
# ==============================================================================

# ==============================================================================
# Setup
# ==============================================================================


fs::dir_create(here("Data/raw/border_crossings"))

crossings <- tribble(
 ~crossing_id, ~crossing_name, ~crossing_type,
 ~country_col, ~municipality_col,
 ~country_ven, ~municipality_ven,

 "C01", "Puente Internacional Simón Bolívar", "Bridge",
 "Colombia", "Cúcuta",
 "Venezuela", "San Antonio del Táchira",

 "C02", "Puente Internacional Francisco de Paula Santander", "Bridge",
 "Colombia", "Villa del Rosario",
 "Venezuela", "Ureña",

 "C03", "Puente Internacional Atanasio Girardot", "Bridge",
 "Colombia", "Villa del Rosario",
 "Venezuela", "Ureña",

 "C04", "Paraguachón", "Land",
 "Colombia", "Maicao",
 "Venezuela", "Paraguachón",

 "C05", "Puente Internacional José Antonio Páez", "Bridge",
 "Colombia", "Arauca",
 "Venezuela", "El Amparo"
)

# ==============================================================================
# Look up 
# ==============================================================================

crossings <- crossings %>%
 mutate(
   search = paste(crossing_name, 
   #municipality_col, 
   "Colombia")
 )

crossings_geo <- crossings %>%
 geocode(
   search,
   method = "osm",
   lat = latitude,
   long = longitude
 )

# Validate geocoding result before creating sf object
if (nrow(crossings_geo) != 5) {
 stop("Crossing geocoding failed: expected exactly 5 rows, got ", nrow(crossings_geo), ".")
}

if (length(unique(crossings_geo$crossing_id)) != 5) {
 stop("Crossing geocoding failed: crossing_id is not unique.")
}

if (any(is.na(crossings_geo$latitude))) {
 stop("Crossing geocoding failed: missing latitude values detected.")
}

if (any(is.na(crossings_geo$longitude))) {
 stop("Crossing geocoding failed: missing longitude values detected.")
}

crossings_geo <- crossings_geo %>%
 mutate(
   latitude = case_when(
     crossing_id == "C05" ~ 7.0896082,
     TRUE ~ latitude
   ),
   longitude = case_when(
     crossing_id == "C05" ~ -70.7402657,
     TRUE ~ longitude
   )
 )

# Final validation after override
if (any(is.na(crossings_geo$latitude))) {
 stop("Crossing validation failed: latitude still missing after override.")
}

if (any(is.na(crossings_geo$longitude))) {
 stop("Crossing validation failed: longitude still missing after override.")
}

if (nrow(crossings_geo) != 5) {
 stop("Crossing validation failed: expected exactly 5 crossings.")
}

if (length(unique(crossings_geo$crossing_id)) != 5) {
 stop("Crossing validation failed: crossing_id is not unique.")
}

write_csv(
 crossings_geo,
 here(
   "Data/raw/border_crossings",
   "border_crossings.csv"
 )
)

# ==============================================================================
# Create sf object for border crossings
# ==============================================================================

study_area <- st_read(
 here("Output/maps/boundries/study_area.gpkg"),
 quiet = TRUE
)

target_crs <- st_crs(study_area)

crossings_sf <- st_as_sf(
 crossings_geo,
 coords = c("longitude", "latitude"),
 crs = 4326
)

crossings_sf <- st_transform(crossings_sf, target_crs)

saveRDS(
 crossings_sf,
 here("Output/Bases/border_crossings", "border_crossings.rds")
)

st_write(
 crossings_sf,
 here("Output/Bases/border_crossings", "border_crossings.gpkg"),
 delete_dsn = TRUE,
 quiet = TRUE
)

# crossings_sf

# crossings_sf %>%
#   st_drop_geometry() %>%
#   select(crossing_id, crossing_name, latitude, longitude)
