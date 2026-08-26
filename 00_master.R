# Code made by Jean Pierre Oliveros
# Original date: 30.06.2026
# Last update: 
# Description:
  # Project for the GIS seminar project 2026 for the UOL in the program master Applied Economics and Data Science

# Environment 
rm(list = ls())

# Packages --------------------------------------------------

  # Data manipulation
manipulation <- c(
  "tidyverse", 
  "stringr"
)

  # GIS
gis <- c(
  "modisfast",
  "terra",
  "sf",
  "giscoR",
  # "blackmarbler", 
  "geodata", # Shapefiles of countries and administrative boundaries
  "rnaturalearth", # Country borders
  "osmdata", # Open Street Map data
  "osmextract", # OSM extraction,
  "tidygeocoder",
  "exactextractr",
  "sfnetworks",
  "cppRouting",
  "dodgr",
  "blackmarbler"
)

  # Useful
use <- c(
  "keyring", 
  "here",
  "httr2",
  "tidygraph",
  "fixest",
  "modelsummary",
  "fs",
  "patchwork"
)
# Install packages if not already installed  

pkgs = c(manipulation, gis, use)
to_install = !pkgs %in% installed.packages()
if (any(to_install)) {
  install.packages(pkgs[to_install])
}
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, to_install, manipulation, gis, use)

# Settup credentials -----------------------------------------------------------------
#  Set API info for NASA data download 

# keyring::key_set(service = "nasa_api", username = "my_earth_data_username", prompt = "Enter earthdata username:")
# keyring::key_set(service = "nasa_api", username = "my_earth_data_password", prompt = "Enter earthdata password:")

nasa_username <- keyring::key_get(service = "nasa_api", username = "my_earth_data_username")
nasa_password <- keyring::key_get(service = "nasa_api", username = "my_earth_data_password")


nasa_login <- blackmarbler::get_nasa_token(username = nasa_username, password = nasa_password)
rm(nasa_username, nasa_password) # Remove from environment for security

