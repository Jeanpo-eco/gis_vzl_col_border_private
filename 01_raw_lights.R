# Script to download the data for processing for VIIIRS

# General settup
cntry_iso <- "VEN"
  
  # File paths
  path_map <- file.path("./Data/maps")
  path_viirs <- file.path("./Data/VIIRS")
    # Years for each viirs
    year_range <- seq(2018, 2025)
    for(year in year_range){
      dir.create(file.path("./Data/VIIRS", year), recursive = TRUE, showWarnings = FALSE)
    }

# Download Venezuela lights data ----------------------------------

##  Get Venezuela GDAM ---------------------------------------------

vzla_sf <- gisco_get_countries(country = "VE", resolution = "20") %>%
  sf::st_as_sf()

# # Check coordinate reference system (CRS)
# sf::st_crs(vzla_sf)

# # View
# mapview::mapview(vzla_sf, legend = TRUE, viewer.suppress = FALSE)

# Get NASA raster data
# Set directory for downloaded data
bm_product_id <- "VNP46A3" # Anual aggegated night lights product
temp_year <- 2024

for(year in temp_year){
  print(paste("Downloading for", year))
  # Setup
  temp_dir <- file.path(path_viirs, year)
  months <- seq(as.Date(paste0(year, "/01/01")), as.Date(paste0(year, "/12/01")), by = "month")
  
  bm_data <- blackmarbler::bm_raster(
  roi_sf = vzla_sf,
  product_id = bm_product_id,
  date = months,
  bearer = nasa_login,
  check_all_tiles_exist = FALSE,
  save_dir = temp_dir,
  overwrite = TRUE
  )
  print(paste("Finished", year))
}

bm_data_mask <- bm_data %>%
  terra::clamp(lower = 0) %>%
  terra::crop(vzla_sf) %>% # crop to relevant area
  terra::mask(vzla_sf) # mask the raster

# Plot the map
plot(bm_data_mask)

# 5. Calculate SUL (Sum of Urban Light)
# viirs_raster is a multi-layer SpatRaster (one layer per month). 
# We use terra::global to sum all pixel values in each layer.
sul_values <- terra::global(bm_data_mask, fun = "sum", na.rm = TRUE)

# 6. Clean and Format the Final Dataframe
sul_df <- sul_values %>%
  as_tibble(rownames = "layer_name") %>%
  rename(sul = sum) %>%
  mutate(
    # Extract the date from the layer name (format usually contains YYYY-MM-DD)
    date = ym(str_extract(layer_name, "\\d{4}_\\d{2}"))
  ) %>%
  select(date, sul)

print(sul_df)

test <- blackmarbler::bm_raster(
  roi_sf = vzla_sf,
  product_id = "VNP46A4",
  date = 2018,
  bearer = nasa_login,
  check_all_tiles_exist = FALSE,
  save_dir = file.path(path_viirs, "2024"),
  overwrite = TRUE)

test_mask <- test %>%
  #terra::clamp(lower = 0) %>%
  terra::crop(vzla_sf) %>% # crop to relevant area
  terra::mask(vzla_sf) # mask the raster

plot(test_mask)
