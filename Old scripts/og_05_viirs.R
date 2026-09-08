study_area <- st_read(
  here(
    "Output/maps/boundries/study_area.gpkg")
)

fs::dir_create(
  here("Output/nightlights")
)

gz_files <-
  fs::dir_ls(
    here("Data/viirs"),
    regexp = "\\.gz$"
  )

walk(
  gz_files,
  ~R.utils::gunzip(
    .x,
    overwrite = FALSE,
    remove = FALSE
  )
)


viirs_files <-
  fs::dir_ls(
    here("Data/viirs"),
    regexp = "\\.tif$"
  )

nightlights_panel <- list()

for(file in viirs_files){

  message("Processing: ", fs::path_file(file))

  year <-
    str_extract(
      fs::path_file(file),
      "20\\d{2}"
    ) |>
    as.integer()

  viirs <- rast(file)

  study_area_proj <-
    st_transform(
      study_area,
      crs(viirs)
    )

  viirs_crop <-
    crop(
      viirs,
      vect(study_area_proj)
    )

  writeRaster(
    viirs_crop,
    filename = here(
      "Output/nightlights",
      paste0("VNL_", year, "_crop.tif")
    ),
    overwrite = TRUE
  )

  mean_light <-
    exact_extract(
      viirs_crop,
      study_area_proj,
      "mean"
    )

  panel_year <-
    study_area |>
    st_drop_geometry() |>
    transmute(
      municipality_id,
      year,
      mean_light
    )

  nightlights_panel[[as.character(year)]] <-
    panel_year

}

nightlights_panel <-
  bind_rows(
    nightlights_panel
  )

write_csv(
  nightlights_panel,
  here(
    "Output/nightlights",
    "municipality_nightlights.csv"
  )
)

saveRDS(
    nightlights_panel,
    here(
        "Output/nightlights",
        "municipality_nightlights.rds"
    )
)
