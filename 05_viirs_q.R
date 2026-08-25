# ==============================================================================
# Script: 05_viirs_q.R
# Description: Downloads quarterly VIIRS nighttime lights via blackmarbler,
#              crops/masks to the study area, and builds a municipality-quarter
#              panel of mean lights.
# ==============================================================================

# 2. User parameters -----------------------------------------------------------
# Adjust this range as needed for your empirical window.
start_year <- 2012
end_year <- 2025
viirs_product <- "VNP46A3"

# VIIRS monthly availability starts in 2012-04. We keep the full year range but
# skip quarters that contain no available data.
min_available_month <- as.Date("2012-04-01")

# 3. Inputs/outputs ------------------------------------------------------------
study_area <- st_read(
  here("Output/maps/boundries/study_area.gpkg"),
  quiet = TRUE
)

download_dir <- here("Data/viirs_q/raw")
output_dir <- here("Output/nightlights/quarterly")
panel_dir <- here("Output/nightlights")

fs::dir_create(download_dir, recurse = TRUE)
fs::dir_create(output_dir, recurse = TRUE)
fs::dir_create(panel_dir, recurse = TRUE)

# blackmarbler works more reliably with valid WGS84 geometries.
study_area_bm <- study_area %>%
  st_make_valid() %>%
  st_buffer(0) %>%
  st_transform(4326)


# 5. Quarter index -------------------------------------------------------------
quarters_tbl <- expand.grid(
  year = seq(start_year, end_year),
  quarter = 1:4
) %>%
  as_tibble() %>%
  mutate(
    start_month = (quarter - 1) * 3 + 1L,
    q_start = as.Date(sprintf("%d-%02d-01", year, start_month)),
    q_end = q_start %m+% months(2)
  ) %>%
  arrange(year, quarter)

# 6. Download -> quarterly crop ------------------------------------------------
for (i in seq_len(nrow(quarters_tbl))) {
  year_i <- quarters_tbl$year[i]
  quarter_i <- quarters_tbl$quarter[i]
  q_start_i <- quarters_tbl$q_start[i]
  q_end_i <- quarters_tbl$q_end[i]

  out_file <- here(
    "Output/nightlights/quarterly",
    sprintf("VIIRS_%d_Q%d_crop.tif", year_i, quarter_i)
  )

  if (file.exists(out_file)) {
    message("Skipping existing file: ", fs::path_file(out_file))
    next
  }

  message(
    "Processing ", year_i, " Q", quarter_i,
    " (", q_start_i, " to ", q_end_i, ")"
  )

  quarter_months <- seq(q_start_i, q_end_i, by = "month")
  quarter_months <- quarter_months[quarter_months >= min_available_month]

  if (length(quarter_months) == 0) {
    message("Skipping ", year_i, " Q", quarter_i, ": no VIIRS months available.")
    next
  }

  quarter_stack <- tryCatch(
    {
      blackmarbler::bm_raster(
        roi_sf = study_area_bm,
        product_id = viirs_product,
        date = quarter_months,
        bearer = nasa_login,
        check_all_tiles_exist = FALSE,
        save_dir = download_dir,
        overwrite = FALSE
      )
    },
    error = function(e) {
      message(
        "Skipping ", year_i, " Q", quarter_i,
        " due to blackmarbler error: ", conditionMessage(e)
      )
      NULL
    }
  )

  if (is.null(quarter_stack)) {
    next
  }

  quarter_mean <- terra::app(quarter_stack, fun = mean, na.rm = TRUE)
  study_area_proj <- st_transform(study_area, crs(quarter_mean))
  quarter_crop <- terra::crop(quarter_mean, terra::vect(study_area_proj))
  quarter_mask <- terra::mask(quarter_crop, terra::vect(study_area_proj))

  terra::writeRaster(
    quarter_mask,
    filename = out_file,
    overwrite = TRUE
  )
}

message("Quarterly cropped VIIRS rasters saved to: ", output_dir)

# 7. Build municipality-quarter panel ------------------------------------------
quarterly_files <- fs::dir_ls(
  output_dir,
  regexp = "VIIRS_\\d{4}_Q[1-4]_crop\\.tif$"
)

if (length(quarterly_files) == 0) {
  stop("No quarterly cropped rasters found in: ", output_dir)
}

panel_list <- purrr::map(quarterly_files, function(file_path) {
  file_name <- fs::path_file(file_path)

  year_val <- stringr::str_extract(file_name, "\\d{4}") %>% as.integer()
  quarter_val <- stringr::str_extract(file_name, "Q[1-4]") %>%
    stringr::str_remove("Q") %>%
    as.integer()

  if (is.na(year_val) || is.na(quarter_val)) {
    stop("Could not parse year/quarter from file name: ", file_name)
  }

  viirs_q <- terra::rast(file_path)
  study_area_proj <- st_transform(study_area, crs(viirs_q))
  mean_light <- exactextractr::exact_extract(viirs_q, study_area_proj, "mean")

  study_area %>%
    st_drop_geometry() %>%
    transmute(
      municipality_id = as.character(municipality_id),
      year = year_val,
      quarter = quarter_val,
      year_quarter = paste0(year_val, "Q", quarter_val),
      mean_light
    )
})

nightlights_q_panel <- bind_rows(panel_list) %>%
  arrange(municipality_id, year, quarter)

saveRDS(
  nightlights_q_panel,
  here("Output/nightlights/municipality_nightlights_quarterly.rds")
)

readr::write_csv(
  nightlights_q_panel,
  here("Output/nightlights/municipality_nightlights_quarterly.csv")
)

message(
  "Script 05_viirs_q finished successfully: quarterly rasters and municipality-quarter panel saved."
)
