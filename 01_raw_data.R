# Script to download the raw data
  # Maps / Polygons of Venezuela and Colombia
  # VIIRS

# Folders
dir.create(here("Data"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Data/raw"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Data/maps/gadm"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Data/raw/OSM"), showWarnings = FALSE, recursive = TRUE)


# Municipalities boundires for Venezuela and Colombia -----------------

ven <- geodata::gadm(
    country = "VEN",
    level = 2,
    path = "./Data/maps/gadm"
) # file is "/Data/maps/gadm/gadm41_VEN_2_pk.rds"

col <- geodata::gadm(
    country = "COL",
    level = 2,
    path = "./Data/maps/gadm"
) # file is "/Data/maps/gadm/gadm41_COL_2_pk.rds"

# Countriy borders -------------------------------------------------

countries <- rnaturalearth::ne_countries(
    returnclass = "sf"
)

saveRDS(
  countries,
  here("Data/raw/countries.rds")
)

# Roads ------------------------------------------------------------

download_geofabrik <- function(country) {

  urls <- c(
    venezuela = "https://download.geofabrik.de/south-america/venezuela-latest.osm.pbf",
    colombia  = "https://download.geofabrik.de/south-america/colombia-latest.osm.pbf"
  )

  outfile <- here(
    "Data/raw/OSM",
    paste0(country, ".osm.pbf")
  )

  if (file.exists(outfile)) {
    message(country, " already downloaded.")
    return(outfile)
  }

  message("Downloading ", country, "...")

  request(urls[[country]]) |>
    req_perform(path = outfile)

  message("Finished.")

  outfile
}

download_geofabrik("venezuela")

download_geofabrik("colombia")
