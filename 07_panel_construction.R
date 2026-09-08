# ==============================================================================
#   Builds the municipality-quarter estimation panel by combining:
#
#     1. Quarterly VIIRS nighttime lights
#     2. Municipality-to-crossing motorcar network distances from Script 06
#     3. Historically documented formal cargo-access status of the five
#        Colombia-Venezuela crossings
#     4. Municipality metadata and network-quality diagnostics
#
#   Main spatial treatment:
#
#       D_i = log(d_post_i) - log(d_pre_i)
#
#   where:
#
#       d_pre_i:
#         Shortest motorcar-network distance to an operational formal cargo
#         crossing during the clean pre-treatment period.
#
#       d_post_i:
#         Shortest motorcar-network distance to an operational formal cargo
#         crossing during the Feb 2019-Sep 2022 closure/rerouting regime.
#
#   Clean baseline estimation window:
#
#       Pre:   2017Q1-2018Q4
#       Post:  2019Q2-2022Q2
#
#   Transition quarters:
#
#       2019Q1  -> Feb 23, 2019 closure occurs during the quarter
#       2022Q3  -> Sep 26, 2022 reopening occurs during the quarter
#
#   These transition quarters are excluded from the baseline TWFE sample.
#
#   Municipalities outside the connected motorcar-network component containing
#   the five crossings remain in the panel, but their distance-based treatment
#   variables remain NA.
# ==============================================================================

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------

nightlights <- readRDS(
  here(
    "Output/nightlights",
    "municipality_nightlights_quarterly.rds"
  )
)

travel_distances <- readRDS(
  here(
    "Output/Bases/access",
    "baseline_travel_distances.rds"
  )
)

network_quality <- readRDS(
  here(
    "Output/Bases/access",
    "municipality_network_quality.rds"
  )
)

crossings <- readRDS(
  here(
    "Output/Bases/border_crossings",
    "border_crossings.rds"
  )
)

metadata <- read_csv(
  here(
    "Output/maps/boundries",
    "municipality_metadata.csv"
  ),
  show_col_types = FALSE
)


# ------------------------------------------------------------------------------
# Standardize IDs
# ------------------------------------------------------------------------------

nightlights <- nightlights %>%
  mutate(
    municipality_id = as.character(municipality_id)
  )

travel_distances <- travel_distances %>%
  mutate(
    municipality_id = as.character(municipality_id),
    crossing_id = as.character(crossing_id)
  )

network_quality <- network_quality %>%
  mutate(
    municipality_id = as.character(municipality_id)
  )

crossings <- crossings %>%
  mutate(
    crossing_id = as.character(crossing_id)
  )

metadata <- metadata %>%
  mutate(
    municipality_id = as.character(municipality_id)
  )


# ------------------------------------------------------------------------------
# Validate static network-distance input
# ------------------------------------------------------------------------------

distance_duplicates <- travel_distances %>%
  count(
    municipality_id,
    crossing_id
  ) %>%
  filter(
    n > 1
  )

if (nrow(distance_duplicates) > 0) {
  stop(
    "travel_distances is not unique at municipality x crossing level."
  )
}

expected_pairs <-
  n_distinct(travel_distances$municipality_id) *
  n_distinct(travel_distances$crossing_id)

if (nrow(travel_distances) != expected_pairs) {
  stop(
    "The municipality x crossing distance matrix is incomplete."
  )
}

if (n_distinct(crossings$crossing_id) != 5) {
  stop(
    "Expected exactly five official border crossings."
  )
}

if (n_distinct(network_quality$municipality_id) != 235) {
  warning(
    "Expected 235 analysis-area municipalities, but network_quality contains ",
    n_distinct(network_quality$municipality_id),
    "."
  )
}


# ------------------------------------------------------------------------------
# Restrict nightlights to analysis-area municipalities
# ------------------------------------------------------------------------------

analysis_municipalities <- network_quality %>%
  distinct(
    municipality_id
  )

nightlights_analysis <- nightlights %>%
  semi_join(
    analysis_municipalities,
    by = "municipality_id"
  )

message(
  "Nightlights municipalities before restriction: ",
  n_distinct(nightlights$municipality_id)
)

message(
  "Nightlights municipalities in analysis area: ",
  n_distinct(nightlights_analysis$municipality_id)
)


# ------------------------------------------------------------------------------
# Create quarterly date index
# ------------------------------------------------------------------------------

quarter_dates <- nightlights_analysis %>%
  distinct(
    year,
    quarter,
    year_quarter
  ) %>%
  mutate(

    quarter_start = as.Date(
      sprintf(
        "%d-%02d-01",
        year,
        (quarter - 1) * 3 + 1
      )
    ),

    quarter_end =
      quarter_start %m+%
      months(3) -
      days(1)

  ) %>%
  arrange(
    year,
    quarter
  )


# ------------------------------------------------------------------------------
# Historical crossing definitions
# ------------------------------------------------------------------------------

# Crossing IDs:
#
# C01 = Puente Internacional Simón Bolívar
# C02 = Puente Internacional Francisco de Paula Santander
# C03 = Puente Internacional Atanasio Girardot / Tienditas
# C04 = Paraguachón
# C05 = Puente Internacional José Antonio Páez
#
#
# Formal cargo-access interpretation:
#
# Clean pre-2019 regime:
#   C01 = open
#   C02 = open
#   C03 = unopened
#   C04 = open
#   C05 = open
#
# Feb 2019-Sep 2022 closure/rerouting regime:
#   C01 = closed to formal cargo
#   C02 = closed to formal cargo
#   C03 = unopened
#   C04 = open
#   C05 = closed
#
# Reopening:
#   C01 = reopened Sep 26, 2022
#   C02 = reopened Sep 26, 2022
#   C04 = remains open
#   C03 = opens Jan 1, 2023
#   C05 = reopens Jan 2023
#
# Transition quarters are explicitly separated rather than assigned a full
# binary treatment state.


# ------------------------------------------------------------------------------
# Construct crossing x quarter status matrix
# ------------------------------------------------------------------------------

crossing_status_q <- tidyr::crossing(
  crossing_id = crossings$crossing_id,
  quarter_dates
) %>%

  mutate(

    # --------------------------------------------------------------------------
    # Institutional regime
    # --------------------------------------------------------------------------

    regime = case_when(

      quarter_end < as.Date("2015-08-01") ~
        "pre_2015_open",

      quarter_start >= as.Date("2015-08-01") &
        quarter_end < as.Date("2017-01-01") ~
        "2015_2016_transition",

      quarter_start >= as.Date("2017-01-01") &
        quarter_end < as.Date("2019-02-23") ~
        "pre_2019_open",

      quarter_start <= as.Date("2019-02-23") &
        quarter_end >= as.Date("2019-02-23") ~
        "2019Q1_transition",

      quarter_start >= as.Date("2019-04-01") &
        quarter_end < as.Date("2022-09-26") ~
        "2019_2022_closure",

      quarter_start <= as.Date("2022-09-26") &
        quarter_end >= as.Date("2022-09-26") ~
        "2022Q3_transition",

      quarter_start >= as.Date("2022-10-01") ~
        "post_reopening",

      TRUE ~
        "other"
    ),


    # --------------------------------------------------------------------------
    # Binary formal-cargo accessibility
    # --------------------------------------------------------------------------

    open_binary = case_when(

      # ------------------------------------------------------------------------
      # C03: Atanasio Girardot / Tienditas
      # Not operational before 1 Jan 2023.
      # ------------------------------------------------------------------------

      crossing_id == "C03" &
        quarter_end < as.Date("2023-01-01") ~
        0L,

      crossing_id == "C03" &
        quarter_start >= as.Date("2023-01-01") ~
        1L,


      # ------------------------------------------------------------------------
      # Clean pre-treatment regime: 2017Q1-2018Q4
      # ------------------------------------------------------------------------

      regime == "pre_2019_open" &
        crossing_id %in% c(
          "C01",
          "C02",
          "C04",
          "C05"
        ) ~
        1L,


      # ------------------------------------------------------------------------
      # Clean closure/rerouting regime: 2019Q2-2022Q2
      # Paraguachón is the operational formal terrestrial cargo alternative.
      # ------------------------------------------------------------------------

      regime == "2019_2022_closure" &
        crossing_id == "C04" ~
        1L,

      regime == "2019_2022_closure" &
        crossing_id %in% c(
          "C01",
          "C02",
          "C03",
          "C05"
        ) ~
        0L,


      # ------------------------------------------------------------------------
      # Clean pre-2015 period
      # ------------------------------------------------------------------------

      regime == "pre_2015_open" &
        crossing_id %in% c(
          "C01",
          "C02",
          "C04",
          "C05"
        ) ~
        1L,

      regime == "pre_2015_open" &
        crossing_id == "C03" ~
        0L,


      # ------------------------------------------------------------------------
      # Post-Sep-2022 reopening
      # ------------------------------------------------------------------------

      regime == "post_reopening" &
        crossing_id %in% c(
          "C01",
          "C02",
          "C04"
        ) ~
        1L,

      regime == "post_reopening" &
        crossing_id == "C05" &
        quarter_start >= as.Date("2023-01-01") ~
        1L,

      regime == "post_reopening" &
        crossing_id == "C05" &
        quarter_end < as.Date("2023-01-01") ~
        0L,


      # ------------------------------------------------------------------------
      # Transition periods remain undefined for clean binary treatment.
      # ------------------------------------------------------------------------

      regime %in% c(
        "2015_2016_transition",
        "2019Q1_transition",
        "2022Q3_transition"
      ) ~
        NA_integer_,

      TRUE ~
        NA_integer_
    ),


    # --------------------------------------------------------------------------
    # Human-readable status
    # --------------------------------------------------------------------------

    status_label = case_when(

      is.na(open_binary) ~
        "transition",

      crossing_id == "C03" &
        quarter_end < as.Date("2023-01-01") ~
        "unopened",

      open_binary == 1 ~
        "open",

      open_binary == 0 ~
        "closed",

      TRUE ~
        NA_character_
    )
  )


# ------------------------------------------------------------------------------
# Validate crossing status matrix
# ------------------------------------------------------------------------------

status_duplicates <- crossing_status_q %>%
  count(
    crossing_id,
    year,
    quarter
  ) %>%
  filter(
    n > 1
  )

if (nrow(status_duplicates) > 0) {
  stop(
    "Crossing-status matrix is not unique at crossing x quarter level."
  )
}


# ------------------------------------------------------------------------------
# Expand static network distances across quarters
# ------------------------------------------------------------------------------

distance_q <- travel_distances %>%

  select(
    municipality_id,
    crossing_id,
    crossing_name,
    distance_m,
    distance_km
  ) %>%

  tidyr::crossing(
    quarter_dates %>%
      select(
        year,
        quarter,
        year_quarter,
        quarter_start,
        quarter_end
      )
  ) %>%

  left_join(
    crossing_status_q %>%
      select(
        crossing_id,
        year,
        quarter,
        regime,
        open_binary,
        status_label
      ),
    by = c(
      "crossing_id",
      "year",
      "quarter"
    )
  )


# ------------------------------------------------------------------------------
# Calculate nearest operational crossing by municipality-quarter
# ------------------------------------------------------------------------------

open_distance_q <- distance_q %>%

  group_by(
    municipality_id,
    year,
    quarter,
    year_quarter,
    regime
  ) %>%

  summarise(

    clean_status_available =
      any(
        !is.na(open_binary)
      ),

    n_open_crossings =
      sum(
        open_binary == 1,
        na.rm = TRUE
      ),

    distance_open_km = {

      usable <- which(
        open_binary == 1 &
          !is.na(distance_km)
      )

      if (length(usable) == 0) {

        NA_real_

      } else {

        min(
          distance_km[usable]
        )
      }
    },

    nearest_open_crossing_id = {

      usable <- which(
        open_binary == 1 &
          !is.na(distance_km)
      )

      if (length(usable) == 0) {

        NA_character_

      } else {

        crossing_id[
          usable[
            which.min(
              distance_km[usable]
            )
          ]
        ]
      }
    },

    nearest_open_crossing_name = {

      usable <- which(
        open_binary == 1 &
          !is.na(distance_km)
      )

      if (length(usable) == 0) {

        NA_character_

      } else {

        crossing_name[
          usable[
            which.min(
              distance_km[usable]
            )
          ]
        ]
      }
    },

    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# Construct pre-treatment network distance
# ------------------------------------------------------------------------------

pre_distance <- open_distance_q %>%

  filter(
    regime == "pre_2019_open"
  ) %>%

  group_by(
    municipality_id
  ) %>%

  summarise(

    d_pre_km = if (
      all(
        is.na(
          distance_open_km
        )
      )
    ) {

      NA_real_

    } else {

      median(
        distance_open_km,
        na.rm = TRUE
      )
    },

    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# Construct closure-period network distance
# ------------------------------------------------------------------------------

post_distance <- open_distance_q %>%

  filter(
    regime == "2019_2022_closure"
  ) %>%

  group_by(
    municipality_id
  ) %>%

  summarise(

    d_post_km = if (
      all(
        is.na(
          distance_open_km
        )
      )
    ) {

      NA_real_

    } else {

      median(
        distance_open_km,
        na.rm = TRUE
      )
    },

    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# Construct municipality-level treatment intensity
# ------------------------------------------------------------------------------

treatment_intensity <- pre_distance %>%

  full_join(
    post_distance,
    by = "municipality_id"
  ) %>%

  mutate(

    distance_change_km =
      d_post_km -
      d_pre_km,

    distance_ratio =
      d_post_km /
      d_pre_km,

    treatment_intensity = case_when(

      is.na(d_pre_km) |
        is.na(d_post_km) ~
        NA_real_,

      d_pre_km <= 0 |
        d_post_km <= 0 ~
        NA_real_,

      TRUE ~
        log(d_post_km) -
        log(d_pre_km)
    )
  )


# ------------------------------------------------------------------------------
# Define quarter-level treatment periods
# ------------------------------------------------------------------------------

quarter_treatment <- quarter_dates %>%

  mutate(

    treatment_period = case_when(

      # Clean pre-treatment period
      year >= 2017 &
        year <= 2018 ~
        "pre",

      # Feb 2019 closure occurs during this quarter
      year == 2019 &
        quarter == 1 ~
        "transition_2019Q1",

      # Clean closure/rerouting period
      (
        year == 2019 &
          quarter >= 2
      ) |
        (
          year >= 2020 &
            year <= 2021
        ) |
        (
          year == 2022 &
            quarter <= 2
        ) ~
        "post",

      # Sep 2022 reopening occurs during this quarter
      year == 2022 &
        quarter == 3 ~
        "transition_2022Q3",

      # Reopening regime
      year > 2022 |
        (
          year == 2022 &
            quarter == 4
        ) ~
        "reopened",

      TRUE ~
        "earlier_history"
    ),

    post_2019 = case_when(

      treatment_period == "pre" ~
        0L,

      treatment_period == "post" ~
        1L,

      TRUE ~
        NA_integer_
    ),

    # Time-window indicator only.
    main_time_sample =
      treatment_period %in%
      c(
        "pre",
        "post"
      )
  )


# ------------------------------------------------------------------------------
# Assemble final municipality-quarter panel
# ------------------------------------------------------------------------------

panel_df <- nightlights_analysis %>%

  left_join(
    open_distance_q %>%
      select(
        municipality_id,
        year,
        quarter,
        regime,
        n_open_crossings,
        distance_open_km,
        nearest_open_crossing_id,
        nearest_open_crossing_name
      ),
    by = c(
      "municipality_id",
      "year",
      "quarter"
    )
  ) %>%

  left_join(
    treatment_intensity,
    by = "municipality_id"
  ) %>%

  left_join(
    network_quality %>%
      select(
        municipality_id,
        network_component,
        snap_distance_m,
        network_reachable,
        n_reachable_crossings
      ),
    by = "municipality_id"
  ) %>%

  left_join(
    metadata,
    by = "municipality_id"
  ) %>%

  left_join(
    quarter_treatment %>%
      select(
        year,
        quarter,
        treatment_period,
        post_2019,
        main_time_sample
      ),
    by = c(
      "year",
      "quarter"
    )
  ) %>%

  mutate(

    # --------------------------------------------------------------------------
    # Outcome
    # --------------------------------------------------------------------------

    log_nightlights =
      if_else(
        !is.na(mean_light),
        log1p(mean_light),
        NA_real_
      ),


    # --------------------------------------------------------------------------
    # Baseline DiD interaction
    # --------------------------------------------------------------------------

    exposure_post = case_when(

      is.na(treatment_intensity) |
        is.na(post_2019) ~
        NA_real_,

      TRUE ~
        treatment_intensity *
        post_2019
    ),


    # --------------------------------------------------------------------------
    # Actual baseline estimation sample
    #
    # Requires:
    #   - clean pre/post quarter
    #   - municipality connected to crossing component
    #   - valid treatment intensity
    #   - valid nighttime-light outcome
    # --------------------------------------------------------------------------

    baseline_sample =
      main_time_sample &
      network_reachable &
      !is.na(treatment_intensity) &
      !is.na(log_nightlights)
  )


# ------------------------------------------------------------------------------
# Validate final panel
# ------------------------------------------------------------------------------

panel_duplicates <- panel_df %>%
  count(
    municipality_id,
    year,
    quarter
  ) %>%
  filter(
    n > 1
  )

if (nrow(panel_duplicates) > 0) {
  stop(
    "Final panel is not unique at municipality x quarter level."
  )
}


# ------------------------------------------------------------------------------
# Construct diagnostic samples
# ------------------------------------------------------------------------------

main_time_df <- panel_df %>%
  filter(
    main_time_sample
  )

baseline_df <- panel_df %>%
  filter(
    baseline_sample
  )


# ------------------------------------------------------------------------------
# Diagnostics / Can be skipped, next section is for saving outputs
# ------------------------------------------------------------------------------

# municipality_count <-
#   n_distinct(
#     panel_df$municipality_id
#   )

# quarter_count <-
#   n_distinct(
#     panel_df$year_quarter
#   )


# message(
#   "Analysis-area municipalities: ",
#   municipality_count
# )

# message(
#   "Quarter count: ",
#   quarter_count
# )

# message(
#   "Total panel observations: ",
#   nrow(panel_df)
# )

# message(
#   "Main time-window observations: ",
#   nrow(main_time_df)
# )

# message(
#   "Main time-window municipalities: ",
#   n_distinct(
#     main_time_df$municipality_id
#   )
# )

# message(
#   "Baseline estimation observations: ",
#   nrow(baseline_df)
# )

# message(
#   "Baseline estimation municipalities: ",
#   n_distinct(
#     baseline_df$municipality_id
#   )
# )

# message(
#   "Missing nighttime-light observations: ",
#   sum(
#     is.na(
#       panel_df$mean_light
#     )
#   )
# )

# message(
#   "Municipalities with missing pre-treatment distance: ",
#   sum(
#     is.na(
#       treatment_intensity$d_pre_km
#     )
#   )
# )

# message(
#   "Municipalities with missing post-treatment distance: ",
#   sum(
#     is.na(
#       treatment_intensity$d_post_km
#     )
#   )
# )

# message(
#   "Municipalities with missing treatment intensity: ",
#   sum(
#     is.na(
#       treatment_intensity$treatment_intensity
#     )
#   )
# )

# message(
#   "Treatment-intensity summary:"
# )

# print(
#   summary(
#     treatment_intensity$treatment_intensity
#   )
# )

# message(
#   "Absolute distance-change summary (km):"
# )

# print(
#   summary(
#     treatment_intensity$distance_change_km
#   )
# )

# message(
#   "Pre-treatment nearest operational crossings:"
# )

# print(
#   open_distance_q %>%
#     filter(
#       regime == "pre_2019_open"
#     ) %>%
#     count(
#       nearest_open_crossing_id,
#       nearest_open_crossing_name,
#       sort = TRUE
#     )
# )

# message(
#   "2019-2022 closure-regime nearest operational crossings:"
# )

# print(
#   open_distance_q %>%
#     filter(
#       regime == "2019_2022_closure"
#     ) %>%
#     count(
#       nearest_open_crossing_id,
#       nearest_open_crossing_name,
#       sort = TRUE
#     )
# )

# message(
#   "Network reachability:"
# )

# print(
#   network_quality %>%
#     count(
#       network_reachable
#     )
# )


# ------------------------------------------------------------------------------
# Output directory
# ------------------------------------------------------------------------------

output_dir <- here(
  "Output/Bases/panel"
)

fs::dir_create(
  output_dir,
  recurse = TRUE
)

saveRDS(
  crossing_status_q,
  file.path(
    output_dir,
    "crossing_status_quarterly.rds"
  )
)

write_csv(
  crossing_status_q,
  file.path(
    output_dir,
    "crossing_status_quarterly.csv"
  )
)

saveRDS(
  open_distance_q,
  file.path(
    output_dir,
    "open_crossing_distance_quarterly.rds"
  )
)

write_csv(
  open_distance_q,
  file.path(
    output_dir,
    "open_crossing_distance_quarterly.csv"
  )
)

saveRDS(
  treatment_intensity,
  file.path(
    output_dir,
    "treatment_intensity.rds"
  )
)

write_csv(
  treatment_intensity,
  file.path(
    output_dir,
    "treatment_intensity.csv"
  )
)

saveRDS(
  panel_df,
  file.path(
    output_dir,
    "estimation_panel.rds"
  )
)

write_csv(
  panel_df,
  file.path(
    output_dir,
    "estimation_panel.csv"
  )
)

saveRDS(
  baseline_df,
  file.path(
    output_dir,
    "baseline_estimation_sample.rds"
  )
)

write_csv(
  baseline_df,
  file.path(
    output_dir,
    "baseline_estimation_sample.csv"
  )
)
