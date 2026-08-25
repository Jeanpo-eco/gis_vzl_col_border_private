# ==============================================================================
# Script: 09_robustness.R
#
# Description:
#   Runs robustness checks for the Colombia-Venezuela border-access analysis.
#
#   Robustness dimensions:
#
#     1. Geographic buffer sensitivity
#     2. Alternative treatment definition
#     3. Alternative outcome transformation
#     4. Municipality-specific linear trends
#     5. Country-specific estimations
#     6. Strong spatial-time fixed effects
#     7. Reopening-period dynamic check
#
#   Important:
#   This script evaluates sensitivity of the substantive findings.
#   It is not intended to search for statistical significance.
#
# ==============================================================================

# ==============================================================================
# 1. LOAD DATA
# ==============================================================================

panel_df <- readRDS(
  here(
    "Output/Bases/panel",
    "estimation_panel.rds"
  )
)

baseline_df <- readRDS(
  here(
    "Output/Bases/panel",
    "baseline_estimation_sample.rds"
  )
)

network_quality <- readRDS(
  here(
    "Output/Bases/access",
    "municipality_network_quality.rds"
  )
)

study_area <- st_read(
  here(
    "Output/maps/boundries",
    "study_area.gpkg"
  ),
  quiet = TRUE
)

analysis_area <- readRDS(
  here(
    "Output/maps/boundries",
    "analysis_area.rds"
  )
)


# ==============================================================================
# 2. STANDARDIZE VARIABLES
# ==============================================================================

panel_df <- panel_df %>%
  mutate(
    municipality_id = as.character(municipality_id),
    year_quarter = as.character(year_quarter)
  )

baseline_df <- baseline_df %>%
  mutate(
    municipality_id = as.character(municipality_id),
    year_quarter = as.character(year_quarter)
  )

network_quality <- network_quality %>%
  mutate(
    municipality_id = as.character(municipality_id)
  )


if (
  "COUNTRY" %in% names(panel_df) &&
    !"country" %in% names(panel_df)
) {

  panel_df <- panel_df %>%
    mutate(
      country = COUNTRY
    )
}


if (
  "COUNTRY" %in% names(baseline_df) &&
    !"country" %in% names(baseline_df)
) {

  baseline_df <- baseline_df %>%
    mutate(
      country = COUNTRY
    )
}


# ==============================================================================
# 3. OUTPUT DIRECTORIES
# ==============================================================================

output_dir <- here(
  "Output/Bases/robustness"
)

figure_dir <- here(
  "Output/Figures/robustness"
)

fs::dir_create(
  output_dir,
  recurse = TRUE
)

fs::dir_create(
  figure_dir,
  recurse = TRUE
)


# ==============================================================================
# 4. BASELINE VALIDATION
# ==============================================================================

if (nrow(baseline_df) == 0) {
  stop("baseline_df is empty.")
}


required_vars <- c(
  "municipality_id",
  "country",
  "NAME_1",
  "year",
  "quarter",
  "year_quarter",
  "mean_light",
  "log_nightlights",
  "treatment_intensity",
  "distance_change_km",
  "post_2019",
  "exposure_post"
)

missing_vars <- setdiff(
  required_vars,
  names(baseline_df)
)

if (length(missing_vars) > 0) {

  stop(
    "Missing required variables: ",
    paste(
      missing_vars,
      collapse = ", "
    )
  )
}


message(
  "Baseline observations: ",
  nrow(baseline_df)
)

message(
  "Baseline municipalities: ",
  n_distinct(
    baseline_df$municipality_id
  )
)


# ==============================================================================
# 5. CONSTRUCT DISTANCE TO INTERNATIONAL BORDER
# ==============================================================================

# Reconstruct the Colombia-Venezuela international border from country polygons.

target_crs <- st_crs(
  analysis_area
)

study_area_proj <- study_area %>%
  st_transform(
    target_crs
  )


ven_country <- study_area_proj %>%
  filter(
    COUNTRY == "Venezuela"
  ) %>%
  summarise()

col_country <- study_area_proj %>%
  filter(
    COUNTRY == "Colombia"
  ) %>%
  summarise()


border <- st_intersection(
  st_boundary(
    ven_country
  ),
  st_boundary(
    col_country
  )
)


# Municipality representative points for the entire available study_area.

municipality_points_all <- study_area_proj %>%
  st_point_on_surface() %>%
  mutate(
    municipality_id =
      as.character(
        municipality_id
      )
  )


distance_to_border_m <- st_distance(
  municipality_points_all,
  border
)


# In case border has multiple geometry pieces, take minimum distance.

if (ncol(distance_to_border_m) > 1) {

  distance_to_border_m <- apply(
    distance_to_border_m,
    1,
    min
  )

} else {

  distance_to_border_m <- as.numeric(
    distance_to_border_m
  )
}


border_distance <- municipality_points_all %>%

  st_drop_geometry() %>%

  transmute(
    municipality_id,
    distance_to_border_km =
      as.numeric(
        distance_to_border_m
      ) /
      1000
  )


# Attach border distance to current panel.

baseline_df <- baseline_df %>%

  left_join(
    border_distance,
    by = "municipality_id"
  )


panel_df <- panel_df %>%

  left_join(
    border_distance,
    by = "municipality_id"
  )


message(
  "Border-distance summary for current baseline sample:"
)

print(
  summary(
    baseline_df$distance_to_border_km
  )
)


# ==============================================================================
# 6. MAIN REFERENCE MODEL
# ==============================================================================

# Preferred benchmark from Script 08:
# municipality FE + state/department x quarter FE

r0_reference <- feols(
  log_nightlights ~ exposure_post |
    municipality_id +
    NAME_1^year_quarter,
  data = baseline_df,
  cluster = ~municipality_id
)

message(
  "Reference robustness model:"
)

print(
  summary(
    r0_reference
  )
)


# ==============================================================================
# 7. BUFFER ROBUSTNESS
# ==============================================================================

buffer_values <- c(
  50,
  100,
  150,
  300
)


estimate_buffer_model <- function(
    buffer_km,
    data
) {

  buffer_df <- data %>%

    filter(
      !is.na(
        distance_to_border_km
      ),
      distance_to_border_km <=
        buffer_km
    )


  n_municipalities <- n_distinct(
    buffer_df$municipality_id
  )


  if (n_municipalities < 10) {

    message(
      "Skipping ",
      buffer_km,
      " km buffer: insufficient municipalities."
    )

    return(
      NULL
    )
  }


  model <- feols(
    log_nightlights ~ exposure_post |
      municipality_id +
      NAME_1^year_quarter,
    data = buffer_df,
    cluster = ~municipality_id
  )


  list(
    model = model,
    n_municipalities =
      n_municipalities,
    n_observations =
      nrow(
        buffer_df
      )
  )
}


buffer_results <- lapply(
  buffer_values,
  estimate_buffer_model,
  data = baseline_df
)

names(
  buffer_results
) <- paste0(
  buffer_values,
  "km"
)


# Keep only successfully estimated models.

buffer_models <- lapply(
  buffer_results,
  function(x) {

    if (is.null(x)) {
      return(NULL)
    }

    x$model
  }
)

buffer_models <- buffer_models[
  !vapply(
    buffer_models,
    is.null,
    logical(1)
  )
]


message(
  "Buffer models successfully estimated:"
)

print(
  names(
    buffer_models
  )
)


# Buffer summary table

buffer_summary <- bind_rows(
  lapply(
    seq_along(
      buffer_results
    ),
    function(i) {

      x <- buffer_results[[i]]

      if (is.null(x)) {
        return(NULL)
      }

      tibble(
        buffer_km =
          buffer_values[i],
        n_municipalities =
          x$n_municipalities,
        n_observations =
          x$n_observations,
        estimate =
          coef(
            x$model
          )[
            "exposure_post"
          ],
        std_error =
          se(
            x$model
          )[
            "exposure_post"
          ],
        p_value =
          pvalue(
            x$model
          )[
            "exposure_post"
          ]
      )
    }
  )
)


write_csv(
  buffer_summary,
  file.path(
    output_dir,
    "buffer_robustness.csv"
  )
)


message(
  "Buffer robustness results:"
)

print(
  buffer_summary
)


# ==============================================================================
# 8. ALTERNATIVE TREATMENT: ABSOLUTE DISTANCE
# ==============================================================================

baseline_df <- baseline_df %>%

  mutate(

    distance_change_100km =
      distance_change_km /
      100,

    distance_change_post =
      distance_change_100km *
      post_2019
  )


r1_absolute_distance <- feols(
  log_nightlights ~
    distance_change_post |
    municipality_id +
    NAME_1^year_quarter,
  data = baseline_df,
  cluster = ~municipality_id
)


message(
  "Alternative absolute-distance treatment:"
)

print(
  summary(
    r1_absolute_distance
  )
)


# ==============================================================================
# 9. ALTERNATIVE OUTCOME: NIGHTLIGHT LEVELS
# ==============================================================================

# This is a robustness check only.
#
# The preferred outcome remains log(1 + NTL).

r2_levels <- feols(
  mean_light ~ exposure_post |
    municipality_id +
    NAME_1^year_quarter,
  data = baseline_df,
  cluster = ~municipality_id
)


message(
  "Nighttime-light levels robustness:"
)

print(
  summary(
    r2_levels
  )
)


# ==============================================================================
# 10. MUNICIPALITY-SPECIFIC LINEAR TRENDS
# ==============================================================================

# Construct sequential quarterly time index.

quarter_lookup <- baseline_df %>%

  distinct(
    year,
    quarter,
    year_quarter
  ) %>%

  arrange(
    year,
    quarter
  ) %>%

  mutate(
    time_index =
      row_number()
  )


baseline_trend_df <- baseline_df %>%

  left_join(
    quarter_lookup,
    by = c(
      "year",
      "quarter",
      "year_quarter"
    )
  )


# fixest varying slope syntax:
#
# municipality_id[time_index]
#
# gives a separate linear time trend for each municipality.

r3_municipality_trends <- feols(
  log_nightlights ~ exposure_post |
    municipality_id +
    NAME_1^year_quarter +
    municipality_id[time_index],
  data = baseline_trend_df,
  cluster = ~municipality_id
)


message(
  "Municipality-specific linear-trend robustness:"
)

print(
  summary(
    r3_municipality_trends
  )
)


# ==============================================================================
# 11. COUNTRY-SPECIFIC ROBUSTNESS
# ==============================================================================

baseline_colombia <- baseline_df %>%
  filter(
    country == "Colombia"
  )

baseline_venezuela <- baseline_df %>%
  filter(
    country == "Venezuela"
  )


r4_colombia <- feols(
  log_nightlights ~ exposure_post |
    municipality_id +
    NAME_1^year_quarter,
  data = baseline_colombia,
  cluster = ~municipality_id
)


r5_venezuela <- feols(
  log_nightlights ~ exposure_post |
    municipality_id +
    NAME_1^year_quarter,
  data = baseline_venezuela,
  cluster = ~municipality_id
)


message(
  "Colombia robustness model:"
)

print(
  summary(
    r4_colombia
  )
)


message(
  "Venezuela robustness model:"
)

print(
  summary(
    r5_venezuela
  )
)


# ==============================================================================
# 12. LESS SATURATED COUNTRY-TIME BENCHMARK
# ==============================================================================

# Useful to show how sensitive the coefficient is to the degree of geographic
# saturation.

r6_country_time <- feols(
  log_nightlights ~ exposure_post |
    municipality_id +
    country^year_quarter,
  data = baseline_df,
  cluster = ~municipality_id
)


# ==============================================================================
# 13. SIMPLE TWFE BENCHMARK
# ==============================================================================

r7_simple_twfe <- feols(
  log_nightlights ~ exposure_post |
    municipality_id +
    year_quarter,
  data = baseline_df,
  cluster = ~municipality_id
)


# ==============================================================================
# 14. REOPENING ROBUSTNESS / VALIDATION
# ==============================================================================

# This is NOT treated as the main causal specification.
#
# It is a descriptive reverse-shock test:
# municipalities whose cargo-access distance increased more after Feb 2019
# should, in principle, gain more when crossings reopen.
#
# We examine:
#
#   Closure regime through 2022Q2
#   2022Q3 transition excluded
#   Reopened regime from 2022Q4 onward
#
# Note that C03 and C05 have additional Jan-2023 openings, so this should be
# interpreted as a broad reopening diagnostic rather than an exact inverse shock.


reopening_df <- panel_df %>%

  filter(
    network_reachable,
    !is.na(
      treatment_intensity
    ),
    !is.na(
      log_nightlights
    ),
    (
      year >= 2021 &
        year <= 2022 &
        !(year == 2022 & quarter == 3)
    ) |
      year >= 2023
  ) %>%

  mutate(

    reopen_post = case_when(

      year < 2022 ~
        0L,

      year == 2022 &
        quarter <= 2 ~
        0L,

      year == 2022 &
        quarter == 4 ~
        1L,

      year >= 2023 ~
        1L,

      TRUE ~
        NA_integer_
    ),

    reopening_exposure =
      treatment_intensity *
      reopen_post
  ) %>%

  filter(
    !is.na(
      reopen_post
    )
  )


r8_reopening <- feols(
  log_nightlights ~ reopening_exposure |
    municipality_id +
    NAME_1^year_quarter,
  data = reopening_df,
  cluster = ~municipality_id
)


message(
  "Reopening robustness model:"
)

print(
  summary(
    r8_reopening
  )
)


# ==============================================================================
# 15. REOPENING EVENT STUDY
# ==============================================================================

# Event time centered on 2022Q3.
#
# 2022Q3 = 0
# 2022Q2 = -1 reference
#
# This is descriptive because reopening dates differ slightly by crossing.


reopening_event_df <- panel_df %>%

  filter(
    network_reachable,
    !is.na(
      treatment_intensity
    ),
    !is.na(
      log_nightlights
    ),
    year >= 2021
  ) %>%

  mutate(

    reopening_event_time =
      (year - 2022) * 4 +
      (quarter - 3)
  )


e_reopening <- feols(
  log_nightlights ~
    i(
      reopening_event_time,
      treatment_intensity,
      ref = -1
    ) |
    municipality_id +
    NAME_1^year_quarter,
  data = reopening_event_df,
  cluster = ~municipality_id
)


png(
  filename = file.path(
    figure_dir,
    "reopening_event_study.png"
  ),
  width = 1800,
  height = 1200,
  res = 200
)

iplot(
  e_reopening,
  main =
    "Reopening-period dynamic robustness check",
  xlab =
    "Quarters relative to 2022Q3",
  ylab =
    "Coefficient on treatment intensity",
  ref.line = 0
)

dev.off()


write_csv(
  broom::tidy(
    e_reopening,
    conf.int = TRUE
  ),
  file.path(
    output_dir,
    "reopening_event_coefficients.csv"
  )
)


# ==============================================================================
# 16. MAIN ROBUSTNESS MODEL TABLE
# ==============================================================================

robustness_models <- list(

  "Simple TWFE" =
    r7_simple_twfe,

  "Country x Quarter FE" =
    r6_country_time,

  "State/Department x Quarter FE" =
    r0_reference,

  "Absolute Distance" =
    r1_absolute_distance,

  "NTL Levels" =
    r2_levels,

  "Municipality Trends" =
    r3_municipality_trends,

  "Colombia" =
    r4_colombia,

  "Venezuela" =
    r5_venezuela
)


etable(
  robustness_models,
  se.below = TRUE,
  fitstat =
    ~n + r2 + wr2
)


# ==============================================================================
# 17. EXPORT MAIN ROBUSTNESS TABLE
# ==============================================================================

modelsummary(
  robustness_models,

  coef_map = c(

    "exposure_post" =
      "Log distance shock × Post",

    "distance_change_post" =
      "Distance increase (100 km) × Post"
  ),

  stars = TRUE,

  gof_omit =
    "AIC|BIC|Log.Lik|RMSE|Std.Errors",

  output = file.path(
    output_dir,
    "robustness_table.html"
  )
)


modelsummary(
  robustness_models,

  coef_map = c(

    "exposure_post" =
      "Log distance shock × Post",

    "distance_change_post" =
      "Distance increase (100 km) × Post"
  ),

  stars = TRUE,

  gof_omit =
    "AIC|BIC|Log.Lik|RMSE|Std.Errors",

  output = file.path(
    output_dir,
    "robustness_table.tex"
  )
)


# ==============================================================================
# 18. BUFFER MODEL TABLE
# ==============================================================================

if (
  length(
    buffer_models
  ) > 0
) {

  modelsummary(
    buffer_models,

    coef_map = c(
      "exposure_post" =
        "Log distance shock × Post"
    ),

    stars = TRUE,

    gof_omit =
      "AIC|BIC|Log.Lik|RMSE|Std.Errors",

    output = file.path(
      output_dir,
      "buffer_models.html"
    )
  )
}


# ==============================================================================
# 19. COEFFICIENT SUMMARY DATASET
# ==============================================================================

extract_main_coefficient <- function(
    model,
    model_name,
    coefficient_name
) {

  broom::tidy(
    model,
    conf.int = TRUE
  ) %>%

    filter(
      term ==
        coefficient_name
    ) %>%

    transmute(
      model =
        model_name,
      estimate,
      std.error,
      conf.low,
      conf.high,
      p.value
    )
}


coefficient_summary <- bind_rows(

  extract_main_coefficient(
    r7_simple_twfe,
    "Simple TWFE",
    "exposure_post"
  ),

  extract_main_coefficient(
    r6_country_time,
    "Country x Quarter FE",
    "exposure_post"
  ),

  extract_main_coefficient(
    r0_reference,
    "State/Department x Quarter FE",
    "exposure_post"
  ),

  extract_main_coefficient(
    r1_absolute_distance,
    "Absolute Distance",
    "distance_change_post"
  ),

  extract_main_coefficient(
    r2_levels,
    "NTL Levels",
    "exposure_post"
  ),

  extract_main_coefficient(
    r3_municipality_trends,
    "Municipality Trends",
    "exposure_post"
  ),

  extract_main_coefficient(
    r4_colombia,
    "Colombia",
    "exposure_post"
  ),

  extract_main_coefficient(
    r5_venezuela,
    "Venezuela",
    "exposure_post"
  )
)


write_csv(
  coefficient_summary,
  file.path(
    output_dir,
    "robustness_coefficient_summary.csv"
  )
)


# ==============================================================================
# 20. COEFFICIENT PLOT
# ==============================================================================

p_robustness <- ggplot(
  coefficient_summary,
  aes(
    x = estimate,
    y = reorder(
      model,
      estimate
    )
  )
) +

  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +

  geom_errorbarh(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    height = 0.2
  ) +

  geom_point(
    size = 2.5
  ) +

  labs(
    title =
      "Robustness of estimated border-access effect",
    subtitle =
      "Points are coefficient estimates; bars are 95% confidence intervals",
    x =
      "Estimated coefficient",
    y =
      NULL
  ) +

  theme_minimal()


# print(
#   p_robustness
# )


ggsave(
  filename = file.path(
    figure_dir,
    "robustness_coefficient_plot.png"
  ),
  plot = p_robustness,
  width = 9,
  height = 6,
  dpi = 300
)


# ==============================================================================
# 21. SAVE MODEL OBJECTS
# ==============================================================================

saveRDS(
  r0_reference,
  file.path(
    output_dir,
    "r0_reference_region_time.rds"
  )
)

saveRDS(
  r1_absolute_distance,
  file.path(
    output_dir,
    "r1_absolute_distance.rds"
  )
)

saveRDS(
  r2_levels,
  file.path(
    output_dir,
    "r2_ntl_levels.rds"
  )
)

saveRDS(
  r3_municipality_trends,
  file.path(
    output_dir,
    "r3_municipality_trends.rds"
  )
)

saveRDS(
  r4_colombia,
  file.path(
    output_dir,
    "r4_colombia.rds"
  )
)

saveRDS(
  r5_venezuela,
  file.path(
    output_dir,
    "r5_venezuela.rds"
  )
)

saveRDS(
  r6_country_time,
  file.path(
    output_dir,
    "r6_country_time.rds"
  )
)

saveRDS(
  r7_simple_twfe,
  file.path(
    output_dir,
    "r7_simple_twfe.rds"
  )
)

saveRDS(
  r8_reopening,
  file.path(
    output_dir,
    "r8_reopening.rds"
  )
)

saveRDS(
  e_reopening,
  file.path(
    output_dir,
    "e_reopening.rds"
  )
)

# ==============================================================================
# SAVE ENRICHED BASELINE SAMPLE FOR FINAL TABLES
# ==============================================================================

# This version contains the original baseline estimation sample plus
# distance_to_border_km constructed in Script 09.
#
# It is saved separately so the original baseline_estimation_sample.rds
# produced upstream remains unchanged.

saveRDS(
  baseline_df,
  file.path(
    output_dir,
    "baseline_estimation_sample_robustness.rds"
  )
)

message(
  "Saved enriched baseline sample for final tables: ",
  file.path(
    output_dir,
    "baseline_estimation_sample_robustness.rds"
  )
)


# ==============================================================================
# 22. FINAL SUMMARY
# ==============================================================================

message(
  "Script 09 finished successfully."
)
