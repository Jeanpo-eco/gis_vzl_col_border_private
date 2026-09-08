# ==============================================================================
#   Estimates Difference-in-Differences models using the spatial exposure
#   measure constructed in Script 07.
#
#   Main treatment:
#
#       D_i = log(d_post_i) - log(d_pre_i)
#
#   Baseline:
#
#       log(1 + NTL_it)
#       = beta * (D_i x Post_t)
#       + municipality FE
#       + time FE
#       + error_it
#
#   The script progressively strengthens the time fixed effects:
#
#     1. Quarter FE
#     2. Country x quarter FE
#     3. Pre-treatment crossing corridor x quarter FE
#     4. State/department x quarter FE
#
#   Main clean estimation window:
#
#       Pre:   2017Q1 - 2018Q4
#       Post:  2019Q2 - 2022Q2
#
#   Transition:
#
#       2019Q1 = closure occurs during quarter
#
# ==============================================================================

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------

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

open_distance_q <- readRDS(
  here(
    "Output/Bases/panel",
    "open_crossing_distance_quarterly.rds"
  )
)


# ------------------------------------------------------------------------------
# Standardize variables
# ------------------------------------------------------------------------------

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

open_distance_q <- open_distance_q %>%
  mutate(
    municipality_id = as.character(municipality_id)
  )


# Harmonize country
if ("COUNTRY" %in% names(panel_df) &&
    !"country" %in% names(panel_df)) {

  panel_df <- panel_df %>%
    mutate(
      country = COUNTRY
    )
}

if ("COUNTRY" %in% names(baseline_df) &&
    !"country" %in% names(baseline_df)) {

  baseline_df <- baseline_df %>%
    mutate(
      country = COUNTRY
    )
}


# ------------------------------------------------------------------------------
# Validate baseline sample
# ------------------------------------------------------------------------------

if (nrow(baseline_df) == 0) {
  stop("Baseline estimation sample is empty.")
}

baseline_duplicates <- baseline_df %>%
  count(
    municipality_id,
    year_quarter
  ) %>%
  filter(
    n > 1
  )

if (nrow(baseline_duplicates) > 0) {
  stop(
    "Baseline sample is not unique at municipality x quarter level."
  )
}


required_vars <- c(
  "municipality_id",
  "year_quarter",
  "country",
  "NAME_1",
  "log_nightlights",
  "treatment_intensity",
  "post_2019",
  "exposure_post",
  "distance_change_km"
)

missing_vars <- setdiff(
  required_vars,
  names(baseline_df)
)

if (length(missing_vars) > 0) {
  stop(
    "Baseline sample is missing required variables: ",
    paste(missing_vars, collapse = ", ")
  )
}


# ------------------------------------------------------------------------------
# Output directories
# ------------------------------------------------------------------------------

output_dir <- here(
  "Output/Bases/regressions"
)

figure_dir <- here(
  "Output/Figures/regressions"
)

fs::dir_create(
  output_dir,
  recurse = TRUE
)

fs::dir_create(
  figure_dir,
  recurse = TRUE
)


# ------------------------------------------------------------------------------
# Basic diagnostics
# ------------------------------------------------------------------------------

message(
  "Baseline observations: ",
  nrow(baseline_df)
)

message(
  "Baseline municipalities: ",
  n_distinct(baseline_df$municipality_id)
)

message(
  "Baseline quarters: ",
  n_distinct(baseline_df$year_quarter)
)

message(
  "Municipalities by country:"
)

print(
  baseline_df %>%
    distinct(
      municipality_id,
      country
    ) %>%
    count(
      country,
      name = "n_municipalities"
    )
)


# ------------------------------------------------------------------------------
# Balanced-panel check
# ------------------------------------------------------------------------------

expected_quarters <- n_distinct(
  baseline_df$year_quarter
)

panel_balance <- baseline_df %>%
  count(
    municipality_id,
    name = "n_quarters"
  )

message(
  "Expected quarters per municipality: ",
  expected_quarters
)

message(
  "Municipalities with incomplete panel: ",
  sum(
    panel_balance$n_quarters != expected_quarters
  )
)


# ------------------------------------------------------------------------------
# Treatment distribution by country
# ------------------------------------------------------------------------------

exposure_by_country <- baseline_df %>%

  distinct(
    municipality_id,
    country,
    treatment_intensity,
    d_pre_km,
    d_post_km,
    distance_change_km
  ) %>%

  group_by(
    country
  ) %>%

  summarise(
    n = n(),

    mean_D =
      mean(
        treatment_intensity,
        na.rm = TRUE
      ),

    median_D =
      median(
        treatment_intensity,
        na.rm = TRUE
      ),

    sd_D =
      sd(
        treatment_intensity,
        na.rm = TRUE
      ),

    min_D =
      min(
        treatment_intensity,
        na.rm = TRUE
      ),

    max_D =
      max(
        treatment_intensity,
        na.rm = TRUE
      ),

    mean_pre_km =
      mean(
        d_pre_km,
        na.rm = TRUE
      ),

    mean_post_km =
      mean(
        d_post_km,
        na.rm = TRUE
      ),

    mean_distance_change_km =
      mean(
        distance_change_km,
        na.rm = TRUE
      ),

    .groups = "drop"
  )

# message(
#   "Exposure distribution by country:"
# )

print(
  exposure_by_country
)

# write_csv(
#   exposure_by_country,
#   file.path(
#     output_dir,
#     "exposure_by_country.csv"
#   )
# )


# ------------------------------------------------------------------------------
# Treatment distribution plot
# ------------------------------------------------------------------------------

exposure_hist_df <- baseline_df %>%
  distinct(
    municipality_id,
    country,
    treatment_intensity
  )

p_exposure_distribution <- ggplot(
  exposure_hist_df,
  aes(
    x = treatment_intensity
  )
) +
  geom_histogram(
    bins = 25
  ) +
  facet_wrap(
    ~country,
    scales = "free_y"
  ) +
  labs(
    title = "Distribution of the border-access shock by country",
    x = "Log change in distance to operational cargo crossing",
    y = "Municipalities"
  ) +
  theme_minimal()

print(
  p_exposure_distribution
)

# ggsave(
#   filename = file.path(
#     figure_dir,
#     "treatment_distribution_by_country.png"
#   ),
#   plot = p_exposure_distribution,
#   width = 9,
#   height = 5.5,
#   dpi = 300
# )


# ==============================================================================
# PRE-TREATMENT CROSSING CORRIDOR
# ==============================================================================


# ------------------------------------------------------------------------------
# Identify each municipality's pre-2019 nearest crossing
# ------------------------------------------------------------------------------

pre_crossing <- open_distance_q %>%

  filter(
    regime == "pre_2019_open"
  ) %>%

  filter(
    !is.na(
      nearest_open_crossing_id
    )
  ) %>%

  group_by(
    municipality_id
  ) %>%

  summarise(

    pre_crossing_id =
      first(
        nearest_open_crossing_id
      ),

    pre_crossing_name =
      first(
        nearest_open_crossing_name
      ),

    .groups = "drop"
  )


# Validate that pre-treatment crossing does not vary across pre-period quarters

pre_crossing_check <- open_distance_q %>%

  filter(
    regime == "pre_2019_open"
  ) %>%

  filter(
    !is.na(
      nearest_open_crossing_id
    )
  ) %>%

  group_by(
    municipality_id
  ) %>%

  summarise(
    n_pre_crossings =
      n_distinct(
        nearest_open_crossing_id
      ),
    .groups = "drop"
  )


if (
  any(
    pre_crossing_check$n_pre_crossings > 1
  )
) {

  warning(
    "At least one municipality changes nearest open crossing during the clean pre-treatment period."
  )
}


message(
  "Municipalities by pre-treatment crossing corridor:"
)

print(
  pre_crossing %>%
    count(
      pre_crossing_id,
      pre_crossing_name,
      sort = TRUE
    )
)


# ------------------------------------------------------------------------------
# Join corridor definition to model datasets
# ------------------------------------------------------------------------------

baseline_df <- baseline_df %>%

  left_join(
    pre_crossing,
    by = "municipality_id"
  )


panel_df <- panel_df %>%

  left_join(
    pre_crossing,
    by = "municipality_id"
  )


# ==============================================================================
# DESCRIPTIVE TRENDS
# ==============================================================================


# ------------------------------------------------------------------------------
# Country trends
# ------------------------------------------------------------------------------

trend_df <- baseline_df %>%

  mutate(
    quarter_date =
      as.Date(
        sprintf(
          "%d-%02d-01",
          year,
          (quarter - 1) * 3 + 1
        )
      )
  ) %>%

  group_by(
    quarter_date,
    country
  ) %>%

  summarise(
    mean_log_light =
      mean(
        log_nightlights,
        na.rm = TRUE
      ),
    .groups = "drop"
  )


p_country_trends <- ggplot(
  trend_df,
  aes(
    x = quarter_date,
    y = mean_log_light,
    color = country
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 1.4
  ) +
  geom_vline(
    xintercept = as.Date("2019-02-23"),
    linetype = "dashed"
  ) +
  labs(
    title = "Quarterly nighttime lights in the baseline sample",
    subtitle = "Dashed line marks the February 2019 foreign-trade closure",
    x = NULL,
    y = "Mean log(1 + nighttime lights)",
    color = "Country"
  ) +
  theme_minimal()


ggsave(
  filename = file.path(
    figure_dir,
    "nightlights_country_trends.png"
  ),
  plot = p_country_trends,
  width = 9,
  height = 5.5,
  dpi = 300
)


# ==============================================================================
# BASELINE MODELS
# ==============================================================================


# ------------------------------------------------------------------------------
# Original TWFE
# ------------------------------------------------------------------------------

m1_original <- feols(
  log_nightlights ~ exposure_post |
    municipality_id +
    year_quarter,
  data = baseline_df,
  cluster = ~municipality_id
)


# ------------------------------------------------------------------------------
# Country x quarter FE
# ------------------------------------------------------------------------------

m2_country_time <- feols(
  log_nightlights ~ exposure_post |
    municipality_id +
    country^year_quarter,
  data = baseline_df,
  cluster = ~municipality_id
)


# ------------------------------------------------------------------------------
# Country-specific treatment effects
# ------------------------------------------------------------------------------

baseline_df <- baseline_df %>%

  mutate(

    exposure_post_colombia =
      exposure_post *
      as.integer(
        country == "Colombia"
      ),

    exposure_post_venezuela =
      exposure_post *
      as.integer(
        country == "Venezuela"
      )
  )


m3_country_effects <- feols(
  log_nightlights ~
    exposure_post_colombia +
    exposure_post_venezuela |
    municipality_id +
    country^year_quarter,
  data = baseline_df,
  cluster = ~municipality_id
)


# ------------------------------------------------------------------------------
# Absolute distance robustness
# ------------------------------------------------------------------------------

baseline_df <- baseline_df %>%

  mutate(

    distance_change_100km =
      distance_change_km /
      100,

    distance_change_post =
      distance_change_100km *
      post_2019
  )


m4_absolute <- feols(
  log_nightlights ~ distance_change_post |
    municipality_id +
    country^year_quarter,
  data = baseline_df,
  cluster = ~municipality_id
)


# ==============================================================================
# STRONGER SPATIAL FIXED EFFECTS
# ==============================================================================


# ------------------------------------------------------------------------------
# Pre-crossing corridor x quarter FE
# ------------------------------------------------------------------------------

# Controls flexibly for different time paths across municipalities whose
# pre-treatment access depended on different crossings.

m5_corridor_time <- feols(
  log_nightlights ~ exposure_post |
    municipality_id +
    country^year_quarter +
    pre_crossing_id^year_quarter,
  data = baseline_df,
  cluster = ~municipality_id
)

message(
  "Pre-crossing corridor x quarter FE model:"
)

print(
  summary(
    m5_corridor_time
  )
)


# ------------------------------------------------------------------------------
# State / department x quarter FE
# ------------------------------------------------------------------------------

# NAME_1 identifies Colombian departments and Venezuelan states.
#
# This allows every first-level administrative region to follow its own
# unrestricted quarterly trajectory.

m6_region_time <- feols(
  log_nightlights ~ exposure_post |
    municipality_id +
    NAME_1^year_quarter,
  data = baseline_df,
  cluster = ~municipality_id
)

message(
  "State/department x quarter FE model:"
)

print(
  summary(
    m6_region_time
  )
)


# ==============================================================================
# EVENT STUDY DATA
# ==============================================================================


# ------------------------------------------------------------------------------
# Construct event-study sample
# ------------------------------------------------------------------------------

event_df <- panel_df %>%

  filter(
    network_reachable,
    !is.na(treatment_intensity),
    !is.na(log_nightlights),

    (
      year >= 2017 &
        year <= 2018
    ) |

      year == 2019 |

      (
        year >= 2020 &
          year <= 2021
      ) |

      (
        year == 2022 &
          quarter <= 2
      )
  ) %>%

  mutate(

    # 2019Q1 = 0
    # 2018Q4 = -1

    event_time =
      (year - 2019) * 4 +
      (quarter - 1),

    year_quarter =
      as.character(
        year_quarter
      )
  )


# ==============================================================================
# EVENT STUDIES
# ==============================================================================


# ------------------------------------------------------------------------------
# Original pooled event study
# ------------------------------------------------------------------------------

e1_original <- feols(
  log_nightlights ~
    i(
      event_time,
      treatment_intensity,
      ref = -1
    ) |
    municipality_id +
    year_quarter,
  data = event_df,
  cluster = ~municipality_id
)


# ------------------------------------------------------------------------------
# Country x quarter event study
# ------------------------------------------------------------------------------

e2_country_time <- feols(
  log_nightlights ~
    i(
      event_time,
      treatment_intensity,
      ref = -1
    ) |
    municipality_id +
    country^year_quarter,
  data = event_df,
  cluster = ~municipality_id
)


# ------------------------------------------------------------------------------
# Pre-crossing corridor x quarter event study
# ------------------------------------------------------------------------------

e3_corridor_time <- feols(
  log_nightlights ~
    i(
      event_time,
      treatment_intensity,
      ref = -1
    ) |
    municipality_id +
    country^year_quarter +
    pre_crossing_id^year_quarter,
  data = event_df,
  cluster = ~municipality_id
)

message(
  "Event study with crossing-corridor x quarter FE:"
)

print(
  summary(
    e3_corridor_time
  )
)


# ------------------------------------------------------------------------------
# State/department x quarter event study
# ------------------------------------------------------------------------------

e4_region_time <- feols(
  log_nightlights ~
    i(
      event_time,
      treatment_intensity,
      ref = -1
    ) |
    municipality_id +
    NAME_1^year_quarter,
  data = event_df,
  cluster = ~municipality_id
)

message(
  "Event study with state/department x quarter FE:"
)

print(
  summary(
    e4_region_time
  )
)


# ==============================================================================
# COUNTRY-SPECIFIC EVENT STUDIES
# ==============================================================================


# ------------------------------------------------------------------------------
# Colombia
# ------------------------------------------------------------------------------

event_colombia <- event_df %>%
  filter(
    country == "Colombia"
  )


e5_colombia <- feols(
  log_nightlights ~
    i(
      event_time,
      treatment_intensity,
      ref = -1
    ) |
    municipality_id +
    year_quarter,
  data = event_colombia,
  cluster = ~municipality_id
)


# ------------------------------------------------------------------------------
# Venezuela
# ------------------------------------------------------------------------------

event_venezuela <- event_df %>%
  filter(
    country == "Venezuela"
  )


e6_venezuela <- feols(
  log_nightlights ~
    i(
      event_time,
      treatment_intensity,
      ref = -1
    ) |
    municipality_id +
    year_quarter,
  data = event_venezuela,
  cluster = ~municipality_id
)


# ==============================================================================
# PRE-TREND TESTS
# ==============================================================================


# ------------------------------------------------------------------------------
# Joint pre-trend tests
# ------------------------------------------------------------------------------

pretrend_country_time <- wald(
  e2_country_time,
  keep =
    "event_time::-[2-8]:treatment_intensity"
)

pretrend_corridor <- wald(
  e3_corridor_time,
  keep =
    "event_time::-[2-8]:treatment_intensity"
)

pretrend_region <- wald(
  e4_region_time,
  keep =
    "event_time::-[2-8]:treatment_intensity"
)

pretrend_colombia <- wald(
  e5_colombia,
  keep =
    "event_time::-[2-8]:treatment_intensity"
)

pretrend_venezuela <- wald(
  e6_venezuela,
  keep =
    "event_time::-[2-8]:treatment_intensity"
)


message(
  "Country x quarter pre-trend test:"
)

print(
  pretrend_country_time
)


message(
  "Crossing-corridor x quarter pre-trend test:"
)

print(
  pretrend_corridor
)


message(
  "State/department x quarter pre-trend test:"
)

print(
  pretrend_region
)


message(
  "Colombia pre-trend test:"
)

print(
  pretrend_colombia
)


message(
  "Venezuela pre-trend test:"
)

print(
  pretrend_venezuela
)


# ==============================================================================
# EVENT-STUDY PLOTS
# ==============================================================================


# ------------------------------------------------------------------------------
# Country x quarter FE plot
# ------------------------------------------------------------------------------

png(
  filename = file.path(
    figure_dir,
    "event_study_country_quarter_fe.png"
  ),
  width = 1800,
  height = 1200,
  res = 200
)

iplot(
  e2_country_time,
  main =
    "Dynamic effect with country-by-quarter fixed effects",
  xlab =
    "Quarters relative to 2019Q1",
  ylab =
    "Coefficient on treatment intensity",
  ref.line = 0
)

dev.off()


# ------------------------------------------------------------------------------
# Crossing-corridor FE plot
# ------------------------------------------------------------------------------

png(
  filename = file.path(
    figure_dir,
    "event_study_corridor_quarter_fe.png"
  ),
  width = 1800,
  height = 1200,
  res = 200
)

iplot(
  e3_corridor_time,
  main =
    "Dynamic effect with crossing-corridor-by-quarter fixed effects",
  xlab =
    "Quarters relative to 2019Q1",
  ylab =
    "Coefficient on treatment intensity",
  ref.line = 0
)

dev.off()


# ------------------------------------------------------------------------------
# State/department FE plot
# ------------------------------------------------------------------------------

png(
  filename = file.path(
    figure_dir,
    "event_study_region_quarter_fe.png"
  ),
  width = 1800,
  height = 1200,
  res = 200
)

iplot(
  e4_region_time,
  main =
    "Dynamic effect with state/department-by-quarter fixed effects",
  xlab =
    "Quarters relative to 2019Q1",
  ylab =
    "Coefficient on treatment intensity",
  ref.line = 0
)

dev.off()


# ------------------------------------------------------------------------------
#  Colombia plot
# ------------------------------------------------------------------------------

png(
  filename = file.path(
    figure_dir,
    "event_study_colombia.png"
  ),
  width = 1800,
  height = 1200,
  res = 200
)

iplot(
  e5_colombia,
  main =
    "Colombia: dynamic border-access effect",
  xlab =
    "Quarters relative to 2019Q1",
  ylab =
    "Coefficient on treatment intensity",
  ref.line = 0
)

dev.off()


# ------------------------------------------------------------------------------
#  Venezuela plot
# ------------------------------------------------------------------------------

png(
  filename = file.path(
    figure_dir,
    "event_study_venezuela.png"
  ),
  width = 1800,
  height = 1200,
  res = 200
)

iplot(
  e6_venezuela,
  main =
    "Venezuela: dynamic border-access effect",
  xlab =
    "Quarters relative to 2019Q1",
  ylab =
    "Coefficient on treatment intensity",
  ref.line = 0
)

dev.off()


# ==============================================================================
# EXPORT EVENT-STUDY COEFFICIENTS
# ==============================================================================


# ------------------------------------------------------------------------------
# Save event-study coefficient tables
# ------------------------------------------------------------------------------

write_csv(
  broom::tidy(
    e2_country_time,
    conf.int = TRUE
  ),
  file.path(
    output_dir,
    "event_country_quarter_coefficients.csv"
  )
)

write_csv(
  broom::tidy(
    e3_corridor_time,
    conf.int = TRUE
  ),
  file.path(
    output_dir,
    "event_corridor_quarter_coefficients.csv"
  )
)

write_csv(
  broom::tidy(
    e4_region_time,
    conf.int = TRUE
  ),
  file.path(
    output_dir,
    "event_region_quarter_coefficients.csv"
  )
)

write_csv(
  broom::tidy(
    e5_colombia,
    conf.int = TRUE
  ),
  file.path(
    output_dir,
    "event_colombia_coefficients.csv"
  )
)

write_csv(
  broom::tidy(
    e6_venezuela,
    conf.int = TRUE
  ),
  file.path(
    output_dir,
    "event_venezuela_coefficients.csv"
  )
)


# ==============================================================================
# MODEL COMPARISON TABLE
# ==============================================================================


# ------------------------------------------------------------------------------
# Main model list
# ------------------------------------------------------------------------------

models <- list(

  "Original TWFE" =
    m1_original,

  "Country x Quarter FE" =
    m2_country_time,

  "Country-specific effects" =
    m3_country_effects,

  "Crossing corridor x Quarter FE" =
    m5_corridor_time,

  "State/Department x Quarter FE" =
    m6_region_time,

  "Absolute distance" =
    m4_absolute
)


etable(
  models,
  se.below = TRUE,
  fitstat =
    ~n + r2 + wr2
)


# ------------------------------------------------------------------------------
# Export HTML regression table
# ------------------------------------------------------------------------------

modelsummary(
  models,

  coef_map = c(

    "exposure_post" =
      "Log distance shock × Post",

    "exposure_post_colombia" =
      "Log distance shock × Post: Colombia",

    "exposure_post_venezuela" =
      "Log distance shock × Post: Venezuela",

    "distance_change_post" =
      "Distance increase (100 km) × Post"
  ),

  stars = TRUE,

  gof_omit =
    "AIC|BIC|Log.Lik|RMSE|Std.Errors",

  output = file.path(
    output_dir,
    "regression_comparison.html"
  )
)


# ------------------------------------------------------------------------------
# Export LaTeX regression table
# ------------------------------------------------------------------------------

modelsummary(
  models,

  coef_map = c(

    "exposure_post" =
      "Log distance shock × Post",

    "exposure_post_colombia" =
      "Log distance shock × Post: Colombia",

    "exposure_post_venezuela" =
      "Log distance shock × Post: Venezuela",

    "distance_change_post" =
      "Distance increase (100 km) × Post"
  ),

  stars = TRUE,

  gof_omit =
    "AIC|BIC|Log.Lik|RMSE|Std.Errors",

  output = file.path(
    output_dir,
    "regression_comparison.tex"
  )
)


# ==============================================================================
# SAVE MODEL OBJECTS
# ==============================================================================


# ------------------------------------------------------------------------------
# Baseline models
# ------------------------------------------------------------------------------

saveRDS(
  m1_original,
  file.path(
    output_dir,
    "m1_original_twfe.rds"
  )
)

saveRDS(
  m2_country_time,
  file.path(
    output_dir,
    "m2_country_quarter_fe.rds"
  )
)

saveRDS(
  m3_country_effects,
  file.path(
    output_dir,
    "m3_country_specific_effects.rds"
  )
)

saveRDS(
  m4_absolute,
  file.path(
    output_dir,
    "m4_absolute_distance.rds"
  )
)

saveRDS(
  m5_corridor_time,
  file.path(
    output_dir,
    "m5_corridor_quarter_fe.rds"
  )
)

saveRDS(
  m6_region_time,
  file.path(
    output_dir,
    "m6_region_quarter_fe.rds"
  )
)


# ------------------------------------------------------------------------------
# Event-study models
# ------------------------------------------------------------------------------

saveRDS(
  e1_original,
  file.path(
    output_dir,
    "e1_original_event.rds"
  )
)

saveRDS(
  e2_country_time,
  file.path(
    output_dir,
    "e2_country_quarter_event.rds"
  )
)

saveRDS(
  e3_corridor_time,
  file.path(
    output_dir,
    "e3_corridor_quarter_event.rds"
  )
)

saveRDS(
  e4_region_time,
  file.path(
    output_dir,
    "e4_region_quarter_event.rds"
  )
)

saveRDS(
  e5_colombia,
  file.path(
    output_dir,
    "e5_colombia_event.rds"
  )
)

saveRDS(
  e6_venezuela,
  file.path(
    output_dir,
    "e6_venezuela_event.rds"
  )
)


# ------------------------------------------------------------------------------
# Save augmented regression sample
# ------------------------------------------------------------------------------

write_csv(
  baseline_df,
  file.path(
    output_dir,
    "baseline_regression_sample_final.csv"
  )
)
