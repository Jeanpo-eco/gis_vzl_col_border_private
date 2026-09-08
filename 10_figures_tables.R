# ==============================================================================
#   Produces only the publication-ready tables and figures used in the paper
#   and appendix. All estimation is performed upstream in Scripts 08 and 09.
# ============================================================================== 

# ==============================================================================
# Output directories
# ==============================================================================

table_dir <- here("Output/Tables/final")
figure_dir <- here("Output/Figures/final")

fs::dir_create(table_dir, recurse = TRUE)
fs::dir_create(figure_dir, recurse = TRUE)

# ==============================================================================
# Load objects used in the paper
# ==============================================================================

# Main models from Script 08 ---------------------------------------------------

m1_original <- readRDS(
  here("Output/Bases/regressions", "m1_original_twfe.rds")
)

m2_country_time <- readRDS(
  here("Output/Bases/regressions", "m2_country_quarter_fe.rds")
)

m3_country_effects <- readRDS(
  here("Output/Bases/regressions", "m3_country_specific_effects.rds")
)

m5_corridor_time <- readRDS(
  here("Output/Bases/regressions", "m5_corridor_quarter_fe.rds")
)

m6_region_time <- readRDS(
  here("Output/Bases/regressions", "m6_region_quarter_fe.rds")
)

# Event-study models from Script 08

e2_country_time <- readRDS(
  here("Output/Bases/regressions", "e2_country_quarter_event.rds")
)

e3_corridor_time <- readRDS(
  here("Output/Bases/regressions", "e3_corridor_quarter_event.rds")
)

e4_region_time <- readRDS(
  here("Output/Bases/regressions", "e4_region_quarter_event.rds")
)

e5_colombia <- readRDS(
  here("Output/Bases/regressions", "e5_colombia_event.rds")
)

e6_venezuela <- readRDS(
  here("Output/Bases/regressions", "e6_venezuela_event.rds")
)

# Robustness models from Script 09

r0_reference <- readRDS(
  here("Output/Bases/robustness", "r0_reference_region_time.rds")
)

r1_absolute_distance <- readRDS(
  here("Output/Bases/robustness", "r1_absolute_distance.rds")
)

r2_levels <- readRDS(
  here("Output/Bases/robustness", "r2_ntl_levels.rds")
)

r3_municipality_trends <- readRDS(
  here("Output/Bases/robustness", "r3_municipality_trends.rds")
)

r4_colombia <- readRDS(
  here("Output/Bases/robustness", "r4_colombia.rds")
)

r5_venezuela <- readRDS(
  here("Output/Bases/robustness", "r5_venezuela.rds")
)

r8_reopening <- readRDS(
  here("Output/Bases/robustness", "r8_reopening.rds")
)

e_reopening <- readRDS(
  here("Output/Bases/robustness", "e_reopening.rds")
)

# Summary data

buffer_summary <- readr::read_csv(
  here("Output/Bases/robustness", "buffer_robustness.csv"),
  show_col_types = FALSE
)

baseline_df <- readRDS(
  here(
    "Output/Bases/robustness",
    "baseline_estimation_sample_robustness.rds"
  )
)

# ==============================================================================
# Functions
# ==============================================================================


# ------------------------------------------------------------------------------
# Extract one regression coefficient
# ------------------------------------------------------------------------------

extract_term <- function(
  model,
  term_name
) {

  broom::tidy(model) %>%

    filter(
      term == term_name
    ) %>%

    transmute(
      estimate,
      std_error = std.error,
      p_value = p.value,
      nobs = stats::nobs(model)
    )
}


# ------------------------------------------------------------------------------
# Extract model fit statistics
# ------------------------------------------------------------------------------

extract_model_stats <- function(model) {

  r2_values <- fixest::r2(
    model,
    type = c(
      "r2",
      "ar2",
      "wr2",
      "war2"
    )
  )

  tibble(
    nobs =
      stats::nobs(model),

    r2 =
      unname(
        r2_values["r2"]
      ),

    adj_r2 =
      unname(
        r2_values["ar2"]
      ),

    within_r2 =
      unname(
        r2_values["wr2"]
      ),

    within_adj_r2 =
      unname(
        r2_values["war2"]
      )
  )
}


# ------------------------------------------------------------------------------
# Extract coefficient + model fit statistics
# ------------------------------------------------------------------------------

extract_model_result <- function(
  model,
  term_name
) {

  bind_cols(

    extract_term(
      model,
      term_name
    ),

    extract_model_stats(
      model
    ) %>%
      select(
        -nobs
      )
  )
}


# ------------------------------------------------------------------------------
# Significance stars
# ------------------------------------------------------------------------------

stars_from_p <- function(p) {

  case_when(

    is.na(p) ~ "",

    p < 0.01 ~ "***",

    p < 0.05 ~ "**",

    p < 0.10 ~ "*",

    TRUE ~ ""
  )
}


# ------------------------------------------------------------------------------
# Formatting helpers
# ------------------------------------------------------------------------------

fmt_est <- function(
  estimate,
  p_value,
  digits = 3
) {

  paste0(
    "$",
    sprintf(
      paste0(
        "%.",
        digits,
        "f"
      ),
      estimate
    ),
    "^{",
    stars_from_p(
      p_value
    ),
    "}$"
  )
}


fmt_se <- function(
  x,
  digits = 3
) {

  paste0(
    "(",
    sprintf(
      paste0(
        "%.",
        digits,
        "f"
      ),
      x
    ),
    ")"
  )
}


fmt_n <- function(x) {

  format(
    x,
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}


fmt_p <- function(x) {

  ifelse(
    x < 0.0001,
    "$<0.0001$",
    sprintf(
      "%.4f",
      x
    )
  )
}


fmt_r2 <- function(
  x,
  digits = 3
) {

  ifelse(
    is.na(x),
    "--",
    sprintf(
      paste0(
        "%.",
        digits,
        "f"
      ),
      x
    )
  )
}

write_tex <- function(x, filename) {
  writeLines(
    x,
    con = file.path(table_dir, filename)
  )
}

save_figure_outputs <- function(plot, stem, width, height, dpi = 320) {
  ggsave(
    filename = file.path(figure_dir, paste0(stem, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    device = cairo_pdf,
    bg = "white"
  )

  ggsave(
    filename = file.path(figure_dir, paste0(stem, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

extract_event_data <- function(
  model,
  event_var = "event_time",
  reference_period = -1L
) {

  pattern <- paste0("^", event_var, "::")

  out <- broom::tidy(
    model,
    conf.int = TRUE
  ) %>%
    filter(grepl(pattern, term)) %>%
    mutate(
      event_time = stringr::str_match(
        term,
        paste0("^", event_var, "::(-?\\d+)")
      )[, 2] %>%
        as.integer()
    ) %>%
    select(
      event_time,
      estimate,
      conf.low,
      conf.high,
      p.value
    )

  bind_rows(
    out,
    tibble(
      event_time = reference_period,
      estimate = 0,
      conf.low = 0,
      conf.high = 0,
      p.value = NA_real_
    )
  ) %>%
    arrange(event_time)
}

plot_event_study <- function(data, y_label) {
  ggplot(
    data,
    aes(x = event_time, y = estimate)
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    geom_vline(
      xintercept = -0.5,
      linetype = "dotted"
    ) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.15
    ) +
    geom_point(size = 2) +
    scale_x_continuous(
      breaks = seq(
        min(data$event_time),
        max(data$event_time),
        by = 2
      )
    ) +
    labs(
      x = "Quarters relative to the border-policy change",
      y = y_label
    ) +
    theme_minimal(base_size = 12)
}

# ==============================================================================
# 4. MAIN-TEXT TABLE: MAIN REGRESSION RESULTS
# ==============================================================================


# ------------------------------------------------------------------------------
# Models included in main table
# ------------------------------------------------------------------------------

main_models <- list(

  m1_original,

  m2_country_time,

  m5_corridor_time,

  m6_region_time
)


# ------------------------------------------------------------------------------
# Extract coefficient and model-fit information
# ------------------------------------------------------------------------------

results_main <- bind_rows(

  lapply(
    main_models,
    function(model) {

      extract_model_result(
        model,
        "exposure_post"
      )
    }
  )

) %>%

  mutate(

    estimate_tex =
      map2_chr(
        estimate,
        p_value,
        fmt_est
      ),

    se_tex =
      map_chr(
        std_error,
        fmt_se
      ),

    n_tex =
      map_chr(
        nobs,
        fmt_n
      ),

    r2_tex =
      map_chr(
        r2,
        fmt_r2
      ),

    adj_r2_tex =
      map_chr(
        adj_r2,
        fmt_r2
      ),

    within_r2_tex =
      map_chr(
        within_r2,
        fmt_r2
      ),

    within_adj_r2_tex =
      map_chr(
        within_adj_r2,
        fmt_r2
      )
  )


# ------------------------------------------------------------------------------
# Build publication-ready LaTeX table
# ------------------------------------------------------------------------------

main_table_tex <- c(

  "\\begin{table}[htbp]",

  "\\centering",

  "\\caption{Border Accessibility and Nighttime Luminosity}",

  "\\label{tab:main_results}",

  "\\small",

  "\\setlength{\\tabcolsep}{4pt}",

  "\\begin{tabular}{lcccc}",

  "\\toprule",

  " & (1) & (2) & (3) & (4) \\\\",

  paste0(
    " & TWFE",
    " & Country $\\times$ Quarter",
    " & Crossing $\\times$ Quarter",
    " & State/Dept. $\\times$ Quarter",
    " \\\\"
  ),

  "\\midrule",


  # Treatment coefficient ------------------------------------------------------

  paste0(
    "Treatment intensity $\\times$ Post",
    " & ", results_main$estimate_tex[1],
    " & ", results_main$estimate_tex[2],
    " & ", results_main$estimate_tex[3],
    " & ", results_main$estimate_tex[4],
    " \\\\"
  ),

  paste0(
    " ",
    " & ", results_main$se_tex[1],
    " & ", results_main$se_tex[2],
    " & ", results_main$se_tex[3],
    " & ", results_main$se_tex[4],
    " \\\\"
  ),


  "\\addlinespace",


  # Fixed effects --------------------------------------------------------------

  "Municipality FE & Yes & Yes & Yes & Yes \\\\",

  "Quarter FE & Yes & No & No & No \\\\",

  "Country $\\times$ Quarter FE & No & Yes & Yes & No \\\\",

  "Crossing corridor $\\times$ Quarter FE & No & No & Yes & No \\\\",

  "State/Department $\\times$ Quarter FE & No & No & No & Yes \\\\",


  "\\midrule",


  # Model statistics -----------------------------------------------------------

  paste0(
    "Observations",
    " & ", results_main$n_tex[1],
    " & ", results_main$n_tex[2],
    " & ", results_main$n_tex[3],
    " & ", results_main$n_tex[4],
    " \\\\"
  ),

  paste0(
    "$R^2$",
    " & ", results_main$r2_tex[1],
    " & ", results_main$r2_tex[2],
    " & ", results_main$r2_tex[3],
    " & ", results_main$r2_tex[4],
    " \\\\"
  ),

  paste0(
    "Adjusted $R^2$",
    " & ", results_main$adj_r2_tex[1],
    " & ", results_main$adj_r2_tex[2],
    " & ", results_main$adj_r2_tex[3],
    " & ", results_main$adj_r2_tex[4],
    " \\\\"
  ),

  paste0(
    "Within $R^2$",
    " & ", results_main$within_r2_tex[1],
    " & ", results_main$within_r2_tex[2],
    " & ", results_main$within_r2_tex[3],
    " & ", results_main$within_r2_tex[4],
    " \\\\"
  ),

  paste0(
    "Adjusted within $R^2$",
    " & ", results_main$within_adj_r2_tex[1],
    " & ", results_main$within_adj_r2_tex[2],
    " & ", results_main$within_adj_r2_tex[3],
    " & ", results_main$within_adj_r2_tex[4],
    " \\\\"
  ),


  "\\bottomrule",

  "\\end{tabular}",

  "\\vspace{0.2cm}",

  "\\begin{minipage}{0.95\\textwidth}",

  "\\footnotesize",

  paste0(
    "\\textit{Notes:} The dependent variable is ",
    "$\\log(1+\\text{nighttime lights})$. ",
    "Treatment intensity is defined as ",
    "$\\log(d_i^{post})-\\log(d_i^{pre})$, where distance is the ",
    "shortest road-network distance to the nearest operational international ",
    "crossing. Post equals one during the clean closure period from ",
    "2019Q2 to 2022Q2; 2019Q1 is excluded as a transition quarter. ",
    "Standard errors in parentheses are clustered at the municipality level. ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),

  "\\end{minipage}",

  "\\end{table}"
)


write_tex(
  main_table_tex,
  "table_main_results.tex"
)

# ==============================================================================
# 5. MAIN-TEXT TABLE: DESCRIPTIVE STATISTICS
# ==============================================================================

municipality_descriptives <- baseline_df %>%
  distinct(
    municipality_id,
    d_pre_km,
    d_post_km,
    distance_change_km,
    treatment_intensity,
    distance_to_border_km
  )

descriptive_stats <- function(x) {
  tibble(
    N = sum(!is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
}

descriptive_table <- bind_rows(
  descriptive_stats(baseline_df$mean_light) %>%
    mutate(Variable = "Nighttime lights"),
  descriptive_stats(baseline_df$log_nightlights) %>%
    mutate(Variable = "Log nighttime lights"),
  descriptive_stats(municipality_descriptives$d_pre_km) %>%
    mutate(Variable = "Pre-treatment network distance (km)"),
  descriptive_stats(municipality_descriptives$d_post_km) %>%
    mutate(Variable = "Post-treatment network distance (km)"),
  descriptive_stats(municipality_descriptives$distance_change_km) %>%
    mutate(Variable = "Change in network distance (km)"),
  descriptive_stats(municipality_descriptives$treatment_intensity) %>%
    mutate(Variable = "Treatment intensity"),
  descriptive_stats(municipality_descriptives$distance_to_border_km) %>%
    mutate(Variable = "Distance to international border (km)")
) %>%
  select(Variable, N, Mean, SD, Median, Min, Max)

# Use the same precision as the Data section.
desc_rows <- pmap_chr(
  descriptive_table,
  function(Variable, N, Mean, SD, Median, Min, Max) {
    paste0(
      Variable,
      " & ", fmt_n(N),
      " & ", sprintf("%.3f", Mean),
      " & ", sprintf("%.3f", SD),
      " & ", sprintf("%.3f", Median),
      " & ", sprintf("%.3f", Min),
      " & ", sprintf("%.3f", Max),
      " \\\\"
    )
  }
)

descriptive_tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Descriptive Statistics: Baseline Estimation Sample}",
  "\\label{tab:descriptive_statistics}",
  "\\small",
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  "Variable & $N$ & Mean & SD & Median & Min. & Max. \\\\",
  "\\midrule",
  desc_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\vspace{0.2cm}",
  "\\begin{minipage}{0.95\\textwidth}",
  "\\footnotesize",
  paste0(
    "\\textit{Notes:} Nighttime-light variables vary by municipality and quarter ",
    "and therefore contain 4,032 observations. Geographic and treatment variables ",
    "are summarized once for each of the 192 municipalities. Treatment intensity ",
    "is the logarithmic change in shortest road-network distance to the nearest ",
    "operational international crossing."
  ),
  "\\end{minipage}",
  "\\end{table}"
)

write_tex(descriptive_tex, "table_descriptive_statistics.tex")

# ==============================================================================
# 6. MAIN-TEXT FIGURE: COEFFICIENT SENSITIVITY
# ==============================================================================

main_coef_df <- bind_rows(
  broom::tidy(m1_original, conf.int = TRUE) %>%
    filter(term == "exposure_post") %>%
    mutate(model = "TWFE"),
  broom::tidy(m2_country_time, conf.int = TRUE) %>%
    filter(term == "exposure_post") %>%
    mutate(model = "Country × Quarter FE"),
  broom::tidy(m5_corridor_time, conf.int = TRUE) %>%
    filter(term == "exposure_post") %>%
    mutate(model = "Crossing Corridor × Quarter FE"),
  broom::tidy(m6_region_time, conf.int = TRUE) %>%
    filter(term == "exposure_post") %>%
    mutate(model = "State/Department × Quarter FE")
) %>%
  mutate(
    model = factor(
      model,
      levels = rev(
        c(
          "TWFE",
          "Country × Quarter FE",
          "Crossing Corridor × Quarter FE",
          "State/Department × Quarter FE"
        )
      )
    )
  )

p_main_coef <- ggplot(
  main_coef_df,
  aes(x = estimate, y = model)
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    width = 0
  ) +
  geom_point(size = 2.5) +
  labs(
    x = "Coefficient on treatment intensity × post",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

save_figure_outputs(
  plot = p_main_coef,
  stem = "figure_main_coefficient_sensitivity",
  width = 8,
  height = 4.5
)

# ==============================================================================
# 7. MAIN-TEXT FIGURE: PREFERRED EVENT STUDY
# ==============================================================================

event_region_df <- extract_event_data(e4_region_time)

p_event_region <- plot_event_study(
  event_region_df,
  "Coefficient on treatment intensity"
) +
  labs(
    x = "Quarters relative to the 2019 border disruption"
  )

save_figure_outputs(
  plot = p_event_region,
  stem = "figure_event_study_region_fe",
  width = 8,
  height = 5
)

# ==============================================================================
# 8. MAIN-TEXT FIGURE: STUDY AREA AND CROSSINGS
# ==============================================================================

target_crs <- 9377

study_area_map <- st_read(
  here("Output/maps/boundries", "study_area.gpkg"),
  quiet = TRUE
) %>%
  st_make_valid() %>%
  st_transform(target_crs)

analysis_area_map <- readRDS(
  here("Output/maps/boundries", "analysis_area.rds")
) %>%
  st_make_valid() %>%
  st_transform(target_crs)

roads_map <- readRDS(
  here("Output/Bases/roads", "roads.rds")
) %>%
  st_transform(target_crs)

crossings_map <- readRDS(
  here("Output/Bases/border_crossings", "border_crossings.rds")
) %>%
  st_transform(target_crs)

countries_map <- study_area_map %>%
  group_by(COUNTRY) %>%
  summarise(
    geometry = st_union(geom),
    .groups = "drop"
  ) %>%
  st_make_valid()

colombia_map <- countries_map %>%
  filter(COUNTRY == "Colombia")

venezuela_map <- countries_map %>%
  filter(COUNTRY == "Venezuela")

international_border <- st_intersection(
  st_boundary(st_geometry(colombia_map)),
  st_boundary(st_geometry(venezuela_map))
)

analysis_outline <- analysis_area_map %>%
  summarise(
    geometry = st_union(geometry)
  ) %>%
  st_make_valid()

analysis_bbox <- st_bbox(
  st_buffer(analysis_outline, 20000)
)

crossing_coords <- crossings_map %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

crossing_labels <- crossing_coords %>%
  mutate(
    label_x = case_when(
      crossing_id == "C01" ~ x - 18000,
      crossing_id == "C02" ~ x + 80000,
      crossing_id == "C03" ~ x - 18000,
      crossing_id == "C04" ~ x - 16000,
      crossing_id == "C05" ~ x - 16000,
      TRUE ~ x - 16000
    ),
    label_y = case_when(
      crossing_id == "C01" ~ y + 30000,
      crossing_id == "C02" ~ y,
      crossing_id == "C03" ~ y - 11000,
      crossing_id == "C04" ~ y + 8000,
      crossing_id == "C05" ~ y + 7000,
      TRUE ~ y
    )
  )

stopifnot(
  nrow(crossings_map) == 5,
  n_distinct(crossings_map$crossing_id) == 5
)

p_main_map <- ggplot() +
  geom_sf(
    data = analysis_area_map,
    fill = "grey98",
    linewidth = 0.15
  ) +
  geom_sf(
    data = roads_map,
    linewidth = 0.07,
    alpha = 0.20
  ) +
  geom_sf(
    data = analysis_area_map,
    fill = NA,
    linewidth = 0.18,
    alpha = 0.5
  ) +
  geom_sf(
    data = international_border,
    linewidth = 0.9
  ) +
  geom_sf(
    data = crossings_map,
    size = 2.7,
    color = "darkgreen"
  ) +
  ggrepel::geom_label_repel(
    data = crossing_labels,
    aes(x = x, y = y, label = crossing_id),
    nudge_x = crossing_labels$label_x - crossing_labels$x,
    nudge_y = crossing_labels$label_y - crossing_labels$y,
    size = 5,
    fontface = "bold",
    color = "darkgreen",
    fill = "white",
    label.size = 0,
    box.padding = 0.35,
    point.padding = 0.45,
    min.segment.length = 0,
    segment.color = "darkgreen",
    segment.size = 0.35,
    force = 2.5,
    max.overlaps = Inf
  ) +
  coord_sf(
    xlim = c(
      analysis_bbox["xmin"],
      analysis_bbox["xmax"]
    ),
    ylim = c(
      analysis_bbox["ymin"],
      analysis_bbox["ymax"]
    ),
    datum = NA,
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.margin = margin(5, 5, 5, 5)
  )

p_locator <- ggplot() +
  geom_sf(
    data = countries_map,
    fill = "white",
    linewidth = 0.45
  ) +
  geom_sf(
    data = analysis_outline,
    fill = "grey75",
    linewidth = 0.25
  ) +
  geom_sf(
    data = international_border,
    linewidth = 0.5
  ) +
  coord_sf(
    datum = NA,
    expand = FALSE
  ) +
  theme_void() +
  theme(
    panel.border = element_rect(
      fill = NA,
      linewidth = 0.45
    ),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(2, 2, 2, 2)
  )

p_border_crossings <- p_main_map +
  patchwork::inset_element(
    p_locator,
    left = 0.72,
    bottom = 0.70,
    right = 0.985,
    top = 0.985,
    align_to = "full"
  )

save_figure_outputs(
  plot = p_border_crossings,
  stem = "figure_border_crossings",
  width = 8,
  height = 8
)

# ==============================================================================
# 9. APPENDIX TABLE: PRE-TREND TESTS
# ==============================================================================

extract_pretrend_test <- function(model, specification) {
  test <- fixest::wald(
    model,
    keep = "event_time::-[2-8]:treatment_intensity"
  )

  tibble(
    Specification = specification,
    F_statistic = as.numeric(test$stat),
    p_value = as.numeric(test$p),
    df1 = as.numeric(test$df1),
    df2 = as.numeric(test$df2)
  )
}

pretrend_table <- bind_rows(
  extract_pretrend_test(
    e2_country_time,
    "Country $\\times$ Quarter FE"
  ),
  extract_pretrend_test(
    e3_corridor_time,
    "Crossing Corridor $\\times$ Quarter FE"
  ),
  extract_pretrend_test(
    e4_region_time,
    "State/Department $\\times$ Quarter FE"
  ),
  extract_pretrend_test(
    e5_colombia,
    "Colombia"
  ),
  extract_pretrend_test(
    e6_venezuela,
    "Venezuela"
  )
)

pretrend_rows <- pretrend_table %>%
  mutate(
    row = paste0(
      Specification,
      " & ", sprintf("%.2f", F_statistic),
      " & ", fmt_p(p_value),
      " & ", df1,
      " & ", df2,
      " \\\\"
    )
  ) %>%
  pull(row)

pretrend_tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Joint Tests of Pre-Treatment Coefficients}",
  "\\label{tab:pretrend_tests}",
  "\\small",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  "Specification & $F$ statistic & $p$-value & df$_1$ & df$_2$ \\\\",
  "\\midrule",
  pretrend_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\vspace{0.2cm}",
  "\\begin{minipage}{0.90\\textwidth}",
  "\\footnotesize",
  paste0(
    "\\textit{Notes:} Joint Wald tests of the null that the seven ",
    "pre-treatment event-study coefficients are jointly equal to zero. ",
    "Standard errors are clustered at the municipality level. The omitted ",
    "event-study period is 2018Q4 ($k=-1$)."
  ),
  "\\end{minipage}",
  "\\end{table}"
)

write_tex(pretrend_tex, "table_pretrend_tests.tex")

# ==============================================================================
# 10. APPENDIX TABLE: ALTERNATIVE BORDER BUFFERS
# ==============================================================================

buffer_rows <- buffer_summary %>%
  mutate(
    row = paste0(
      buffer_km,
      " & ", n_municipalities,
      " & ", fmt_n(n_observations),
      " & ", sprintf("%.4f", estimate),
      " & ", sprintf("%.4f", std_error),
      " & ", ifelse(p_value < 0.001, "$<0.001$", sprintf("%.3f", p_value)),
      " \\\\"
    )
  ) %>%
  pull(row)

buffer_tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Robustness to Alternative Border Buffers}",
  "\\label{tab:buffer_robustness}",
  "\\small",
  "\\begin{tabular}{rrrrrr}",
  "\\toprule",
  "Buffer (km) & Municipalities & Observations & Estimate & Std. Error & $p$-value \\\\",
  "\\midrule",
  buffer_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\vspace{0.2cm}",
  "\\begin{minipage}{0.90\\textwidth}",
  "\\footnotesize",
  paste0(
    "\\textit{Notes:} Each row reports the coefficient on treatment intensity ",
    "interacted with the post-2019 indicator for the indicated maximum distance ",
    "from the international boundary. Specifications include municipality and ",
    "state/department-by-quarter fixed effects. Standard errors are clustered ",
    "at the municipality level."
  ),
  "\\end{minipage}",
  "\\end{table}"
)

write_tex(buffer_tex, "table_buffer_robustness.tex")

# Appendix figure for presentation use ------------------------------------------

buffer_plot_df <- buffer_summary %>%
  mutate(
    #buffer_km = as.numeric(stringr::str_extract(buffer_label, "\\d+")),
    conf_low = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error
  ) %>%
  arrange(buffer_km)

p_buffer_robustness <- ggplot(
  buffer_plot_df,
  aes(x = buffer_km, y = estimate)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_errorbar(
    aes(ymin = conf_low, ymax = conf_high),
    width = 7
  ) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = buffer_plot_df$buffer_km) +
  labs(
    x = "Border buffer (km)",
    y = "Coefficient on treatment intensity × post"
  ) +
  theme_minimal(base_size = 12)

save_figure_outputs(
  plot = p_buffer_robustness,
  stem = "figure_buffer_robustness",
  width = 8,
  height = 4.5
)

# ==============================================================================
# 11. APPENDIX TABLE: ALTERNATIVE SPECIFICATIONS
# ==============================================================================


# ------------------------------------------------------------------------------
# Extract model information
# ------------------------------------------------------------------------------

rob_reference <- extract_model_result(
  r0_reference,
  "exposure_post"
)


rob_absolute <- extract_model_result(
  r1_absolute_distance,
  "distance_change_post"
)


rob_levels <- extract_model_result(
  r2_levels,
  "exposure_post"
)


rob_trends <- extract_model_result(
  r3_municipality_trends,
  "exposure_post"
)


# ------------------------------------------------------------------------------
# Format cells
# ------------------------------------------------------------------------------

rob_coef_reference <- fmt_est(
  rob_reference$estimate,
  rob_reference$p_value
)

rob_se_reference <- fmt_se(
  rob_reference$std_error
)


rob_coef_absolute <- fmt_est(
  rob_absolute$estimate,
  rob_absolute$p_value
)

rob_se_absolute <- fmt_se(
  rob_absolute$std_error
)


rob_coef_levels <- fmt_est(
  rob_levels$estimate,
  rob_levels$p_value
)

rob_se_levels <- fmt_se(
  rob_levels$std_error
)


rob_coef_trends <- fmt_est(
  rob_trends$estimate,
  rob_trends$p_value
)

rob_se_trends <- fmt_se(
  rob_trends$std_error
)


# ------------------------------------------------------------------------------
# Build LaTeX table
# ------------------------------------------------------------------------------

robustness_tex <- c(

  "\\begin{table}[htbp]",

  "\\centering",

  "\\caption{Alternative Specifications}",

  "\\label{tab:robustness_specs}",

  "\\small",

  "\\setlength{\\tabcolsep}{4pt}",

  "\\begin{tabular}{lcccc}",

  "\\toprule",

  " & (1) & (2) & (3) & (4) \\\\",

  paste0(
    " & Reference",
    " & Absolute distance",
    " & NTL levels",
    " & Municipality trends",
    " \\\\"
  ),

  "\\midrule",


  # Main log-distance treatment ------------------------------------------------

  paste0(
    "Treatment intensity $\\times$ Post",
    " & ", rob_coef_reference,
    " & ",
    " & ", rob_coef_levels,
    " & ", rob_coef_trends,
    " \\\\"
  ),

  paste0(
    " ",
    " & ", rob_se_reference,
    " & ",
    " & ", rob_se_levels,
    " & ", rob_se_trends,
    " \\\\"
  ),


  # Absolute-distance treatment ------------------------------------------------

  paste0(
    "Distance increase (100 km) $\\times$ Post",
    " & ",
    " & ", rob_coef_absolute,
    " & ",
    " & ",
    " \\\\"
  ),

  paste0(
    " ",
    " & ",
    " & ", rob_se_absolute,
    " & ",
    " & ",
    " \\\\"
  ),


  "\\addlinespace",


  # Outcomes -------------------------------------------------------------------

  paste0(
    "Outcome",
    " & Log NTL",
    " & Log NTL",
    " & NTL levels",
    " & Log NTL",
    " \\\\"
  ),


  # Fixed effects and trends ---------------------------------------------------

  "Municipality FE & Yes & Yes & Yes & Yes \\\\",

  "State/Department $\\times$ Quarter FE & Yes & Yes & Yes & Yes \\\\",

  "Municipality-specific linear trends & No & No & No & Yes \\\\",


  "\\midrule",


  # Fit statistics -------------------------------------------------------------

  paste0(
    "Observations",
    " & ", fmt_n(rob_reference$nobs),
    " & ", fmt_n(rob_absolute$nobs),
    " & ", fmt_n(rob_levels$nobs),
    " & ", fmt_n(rob_trends$nobs),
    " \\\\"
  ),

  paste0(
    "$R^2$",
    " & ", fmt_r2(rob_reference$r2),
    " & ", fmt_r2(rob_absolute$r2),
    " & ", fmt_r2(rob_levels$r2),
    " & ", fmt_r2(rob_trends$r2),
    " \\\\"
  ),

  paste0(
    "Adjusted $R^2$",
    " & ", fmt_r2(rob_reference$adj_r2),
    " & ", fmt_r2(rob_absolute$adj_r2),
    " & ", fmt_r2(rob_levels$adj_r2),
    " & ", fmt_r2(rob_trends$adj_r2),
    " \\\\"
  ),

  paste0(
    "Within $R^2$",
    " & ", fmt_r2(rob_reference$within_r2),
    " & ", fmt_r2(rob_absolute$within_r2),
    " & ", fmt_r2(rob_levels$within_r2),
    " & ", fmt_r2(rob_trends$within_r2),
    " \\\\"
  ),

  paste0(
    "Adjusted within $R^2$",
    " & ", fmt_r2(rob_reference$within_adj_r2),
    " & ", fmt_r2(rob_absolute$within_adj_r2),
    " & ", fmt_r2(rob_levels$within_adj_r2),
    " & ", fmt_r2(rob_trends$within_adj_r2),
    " \\\\"
  ),


  "\\bottomrule",

  "\\end{tabular}",

  "\\vspace{0.2cm}",

  "\\begin{minipage}{0.95\\textwidth}",

  "\\footnotesize",

  paste0(
    "\\textit{Notes:} Columns (1), (3), and (4) use treatment intensity ",
    "$\\log(d_i^{post})-\\log(d_i^{pre})$. Column (2) instead uses the ",
    "absolute increase in road-network distance measured in 100-km units. ",
    "The dependent variable is $\\log(1+\\text{nighttime lights})$ except ",
    "in Column (3), which uses nighttime-light levels. All specifications ",
    "include municipality and state/department-by-quarter fixed effects. ",
    "Column (4) additionally allows each municipality to follow a separate ",
    "linear time trend. Standard errors are clustered at the municipality ",
    "level. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),

  "\\end{minipage}",

  "\\end{table}"
)


write_tex(
  robustness_tex,
  "table_robustness_specs.tex"
)

# ==============================================================================
# 12. APPENDIX TABLE: COUNTRY-SPECIFIC RESULTS
# ==============================================================================


# ------------------------------------------------------------------------------
# Joint country-specific model
# ------------------------------------------------------------------------------

main_col <- extract_term(
  m3_country_effects,
  "exposure_post_colombia"
)

main_ven <- extract_term(
  m3_country_effects,
  "exposure_post_venezuela"
)

main_country_stats <- extract_model_stats(
  m3_country_effects
)


# ------------------------------------------------------------------------------
# Separate country robustness models
# ------------------------------------------------------------------------------

rob_col <- extract_model_result(
  r4_colombia,
  "exposure_post"
)

rob_ven <- extract_model_result(
  r5_venezuela,
  "exposure_post"
)


# ------------------------------------------------------------------------------
# Format coefficients
# ------------------------------------------------------------------------------

main_col_coef <- fmt_est(
  main_col$estimate,
  main_col$p_value
)

main_col_se <- fmt_se(
  main_col$std_error
)


main_ven_coef <- fmt_est(
  main_ven$estimate,
  main_ven$p_value
)

main_ven_se <- fmt_se(
  main_ven$std_error
)


rob_col_coef <- fmt_est(
  rob_col$estimate,
  rob_col$p_value
)

rob_col_se <- fmt_se(
  rob_col$std_error
)


rob_ven_coef <- fmt_est(
  rob_ven$estimate,
  rob_ven$p_value
)

rob_ven_se <- fmt_se(
  rob_ven$std_error
)


# ------------------------------------------------------------------------------
# Build LaTeX table
# ------------------------------------------------------------------------------

country_tex <- c(

  "\\begin{table}[htbp]",

  "\\centering",

  "\\caption{Country-Specific Estimates}",

  "\\label{tab:country_heterogeneity}",

  "\\small",

  "\\setlength{\\tabcolsep}{5pt}",

  "\\begin{tabular}{lccc}",

  "\\toprule",

  " & (1) & (2) & (3) \\\\",

  paste0(
    " & Joint model",
    " & Colombia",
    " & Venezuela",
    " \\\\"
  ),

  "\\midrule",


  # Colombia coefficient -------------------------------------------------------

  paste0(
    "Colombia treatment intensity $\\times$ Post",
    " & ", main_col_coef,
    " & ", rob_col_coef,
    " & ",
    " \\\\"
  ),

  paste0(
    " ",
    " & ", main_col_se,
    " & ", rob_col_se,
    " & ",
    " \\\\"
  ),


  # Venezuela coefficient ------------------------------------------------------

  paste0(
    "Venezuela treatment intensity $\\times$ Post",
    " & ", main_ven_coef,
    " & ",
    " & ", rob_ven_coef,
    " \\\\"
  ),

  paste0(
    " ",
    " & ", main_ven_se,
    " & ",
    " & ", rob_ven_se,
    " \\\\"
  ),


  "\\addlinespace",


  # Fixed effects --------------------------------------------------------------

  "Municipality FE & Yes & Yes & Yes \\\\",

  "Country $\\times$ Quarter FE & Yes & No & No \\\\",

  "State/Department $\\times$ Quarter FE & No & Yes & Yes \\\\",


  "\\midrule",


  # Fit statistics -------------------------------------------------------------

  paste0(
    "Observations",
    " & ", fmt_n(main_country_stats$nobs),
    " & ", fmt_n(rob_col$nobs),
    " & ", fmt_n(rob_ven$nobs),
    " \\\\"
  ),

  paste0(
    "$R^2$",
    " & ", fmt_r2(main_country_stats$r2),
    " & ", fmt_r2(rob_col$r2),
    " & ", fmt_r2(rob_ven$r2),
    " \\\\"
  ),

  paste0(
    "Adjusted $R^2$",
    " & ", fmt_r2(main_country_stats$adj_r2),
    " & ", fmt_r2(rob_col$adj_r2),
    " & ", fmt_r2(rob_ven$adj_r2),
    " \\\\"
  ),

  paste0(
    "Within $R^2$",
    " & ", fmt_r2(main_country_stats$within_r2),
    " & ", fmt_r2(rob_col$within_r2),
    " & ", fmt_r2(rob_ven$within_r2),
    " \\\\"
  ),

  paste0(
    "Adjusted within $R^2$",
    " & ", fmt_r2(main_country_stats$within_adj_r2),
    " & ", fmt_r2(rob_col$within_adj_r2),
    " & ", fmt_r2(rob_ven$within_adj_r2),
    " \\\\"
  ),


  "\\bottomrule",

  "\\end{tabular}",

  "\\vspace{0.2cm}",

  "\\begin{minipage}{0.94\\textwidth}",

  "\\footnotesize",

  paste0(
    "\\textit{Notes:} Column (1) estimates Colombia- and Venezuela-specific ",
    "treatment coefficients jointly using municipality and country-by-quarter ",
    "fixed effects. Columns (2) and (3) estimate the model separately for ",
    "Colombian and Venezuelan municipalities using municipality and ",
    "state/department-by-quarter fixed effects. Treatment intensity is ",
    "$\\log(d_i^{post})-\\log(d_i^{pre})$. The dependent variable is ",
    "$\\log(1+\\text{nighttime lights})$. Standard errors are clustered at ",
    "the municipality level. $^{*}p<0.10$, $^{**}p<0.05$, ",
    "$^{***}p<0.01$."
  ),

  "\\end{minipage}",

  "\\end{table}"
)


write_tex(
  country_tex,
  "table_country_heterogeneity.tex"
)

# Appendix figure for presentation use ------------------------------------------

country_plot_df <- bind_rows(
  main_col %>%
    mutate(
      country = "Colombia",
      specification = "Main country-specific model"
    ),
  main_ven %>%
    mutate(
      country = "Venezuela",
      specification = "Main country-specific model"
    ),
  rob_col %>%
    mutate(
      country = "Colombia",
      specification = "State/Department robustness"
    ),
  rob_ven %>%
    mutate(
      country = "Venezuela",
      specification = "State/Department robustness"
    )
) %>%
  mutate(
    conf_low = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error
  )

p_country_heterogeneity <- ggplot(
  country_plot_df,
  aes(x = country, y = estimate, color = specification)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(
    aes(ymin = conf_low, ymax = conf_high),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  geom_point(
    size = 2.5,
    position = position_dodge(width = 0.5)
  ) +
  labs(
    x = NULL,
    y = "Coefficient on treatment intensity × post",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  coord_flip() +
  theme(
    legend.position = "bottom"
  )

save_figure_outputs(
  plot = p_country_heterogeneity,
  stem = "figure_country_heterogeneity",
  width = 8,
  height = 4.5
)

# ==============================================================================
# 13. APPENDIX FIGURE: COUNTRY-SPECIFIC EVENT STUDIES
# ==============================================================================

event_colombia_df <- extract_event_data(e5_colombia)
event_venezuela_df <- extract_event_data(e6_venezuela)

p_event_colombia <- plot_event_study(
  event_colombia_df,
  "Coefficient on treatment intensity"
) +
  labs(title = "Colombia")

p_event_venezuela <- plot_event_study(
  event_venezuela_df,
  "Coefficient on treatment intensity"
) +
  labs(title = "Venezuela")

p_country_events <- p_event_colombia +
  p_event_venezuela +
  patchwork::plot_layout(ncol = 2)

save_figure_outputs(
  plot = p_country_events,
  stem = "figure_country_event_studies",
  width = 11,
  height = 5
)

save_figure_outputs(
  plot = p_event_colombia,
  stem = "figure_event_study_colombia",
  width = 8,
  height = 5
)

save_figure_outputs(
  plot = p_event_venezuela,
  stem = "figure_event_study_venezuela",
  width = 8,
  height = 5
)

# ==============================================================================
# 14. APPENDIX TABLE AND FIGURE: BORDER REOPENING
# ==============================================================================


# ------------------------------------------------------------------------------
# Reopening average-effect regression
# ------------------------------------------------------------------------------

reopening_result <- extract_model_result(
  r8_reopening,
  "reopening_exposure"
)


reopening_coef <- fmt_est(
  reopening_result$estimate,
  reopening_result$p_value
)


reopening_se <- fmt_se(
  reopening_result$std_error
)


# ------------------------------------------------------------------------------
# Build reopening LaTeX table
# ------------------------------------------------------------------------------

reopening_tex <- c(

  "\\begin{table}[htbp]",

  "\\centering",

  "\\caption{Border-Reopening Diagnostic}",

  "\\label{tab:reopening_result}",

  "\\small",

  "\\begin{tabular}{lc}",

  "\\toprule",

  " & (1) \\\\",

  " & Reopening diagnostic \\\\",

  "\\midrule",


  paste0(
    "Prior exposure $\\times$ Reopening",
    " & ", reopening_coef,
    " \\\\"
  ),

  paste0(
    " ",
    " & ", reopening_se,
    " \\\\"
  ),


  "\\addlinespace",


  "Municipality FE & Yes \\\\",

  "State/Department $\\times$ Quarter FE & Yes \\\\",


  "\\midrule",


  paste0(
    "Observations",
    " & ", fmt_n(reopening_result$nobs),
    " \\\\"
  ),

  paste0(
    "$R^2$",
    " & ", fmt_r2(reopening_result$r2),
    " \\\\"
  ),

  paste0(
    "Adjusted $R^2$",
    " & ", fmt_r2(reopening_result$adj_r2),
    " \\\\"
  ),

  paste0(
    "Within $R^2$",
    " & ", fmt_r2(reopening_result$within_r2),
    " \\\\"
  ),

  paste0(
    "Adjusted within $R^2$",
    " & ", fmt_r2(reopening_result$within_adj_r2),
    " \\\\"
  ),


  "\\bottomrule",

  "\\end{tabular}",

  "\\vspace{0.2cm}",

  "\\begin{minipage}{0.90\\textwidth}",

  "\\footnotesize",

  paste0(
    "\\textit{Notes:} The dependent variable is ",
    "$\\log(1+\\text{nighttime lights})$. Prior exposure corresponds to ",
    "the treatment intensity generated by the 2019 closure regime. ",
    "The specification includes municipality and state/department-by-quarter ",
    "fixed effects. Standard errors are clustered at the municipality level. ",
    "The reopening exercise is interpreted as a descriptive reverse-shock ",
    "diagnostic rather than as an independent causal estimate. ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),

  "\\end{minipage}",

  "\\end{table}"
)


write_tex(
  reopening_tex,
  "table_reopening_result.tex"
)


# ------------------------------------------------------------------------------
# Reopening event study
# ------------------------------------------------------------------------------

event_reopening_df <- extract_event_data(
  e_reopening,
  event_var = "reopening_event_time",
  reference_period = -1L
)


p_reopening <- plot_event_study(
  event_reopening_df,
  "Coefficient on prior treatment intensity"
) +
  labs(
    x = "Quarters relative to the 2022 border reopening"
  )


save_figure_outputs(
  plot = p_reopening,
  stem = "figure_reopening_event_study",
  width = 8,
  height = 5
)

# ==============================================================================
# 15. FINAL OUTPUT SUMMARY
# ==============================================================================

message("Script 10 finished successfully.")
message("Final tables saved to: ", table_dir)
message("Final figures saved to: ", figure_dir)
message(
  paste(
    "Note: the crossing-status/source table is documentary rather than a model",
    "output and should remain maintained separately from Script 10."
  )
)

# ==============================================================================
# 16. PRESENTATION: NIGHTTIME LIGHT ANIMATION FRAMES
# ==============================================================================
# Exports one PNG per year (one selected quarter each) to
# presentation/assets/ntl_frames/ for the rotating-image widget on the
# thank-you slide. Quarters are staggered (Q3/Q1/Q4/Q2/…) so no two
# consecutive frames share the same season.

ntl_tif_dir <- here("Output/nightlights/quarterly")
ntl_png_dir <- here("presentation/assets/ntl_frames")
fs::dir_create(ntl_png_dir, recurse = TRUE)

ntl_selected <- tibble::tribble(
  ~year, ~quarter,
  2012,  3,
  2013,  1,
  2014,  4,
  2015,  2,
  2016,  3,
  2017,  1,
  2018,  4,
  2019,  2,
  2020,  3,
  2021,  1,
  2022,  4,
  2023,  2,
  2024,  3,
  2025,  4
)

ntl_tif_files <- purrr::pmap_chr(ntl_selected, function(year, quarter) {
  here(
    "Output/nightlights/quarterly",
    sprintf("VIIRS_%d_Q%d_crop.tif", year, quarter)
  )
})

missing_tifs <- ntl_tif_files[!fs::file_exists(ntl_tif_files)]
if (length(missing_tifs) > 0) {
  warning(
    "The following NTL TIF files were not found and will be skipped:\n",
    paste(basename(missing_tifs), collapse = "\n")
  )
  ntl_tif_files <- ntl_tif_files[fs::file_exists(ntl_tif_files)]
}

# Shared colour scale: cap at 99th percentile across selected frames so
# brightness is visually comparable across time.
all_vals <- unlist(lapply(ntl_tif_files, function(f) {
  r <- terra::rast(f)
  v <- terra::values(r, na.rm = TRUE)
  v[v >= 0]
}))
vmax <- quantile(all_vals, 0.99, na.rm = TRUE)
i = 0
for (tif_path in ntl_tif_files) {
  i = i + 1
  stem    <- fs::path_ext_remove(fs::path_file(tif_path))
  out_png <- fs::path(ntl_png_dir, paste0(stem, ".png"))

  # Skip if already up-to-date
  if (
    fs::file_exists(out_png) &&
    fs::file_info(out_png)$modification_time >=
      fs::file_info(tif_path)$modification_time
  ) next

  r <- terra::rast(tif_path)

  year_q <- stringr::str_extract(stem, "\\d{4}_Q[1-4]")
  label  <- stringr::str_replace(year_q, "_", " ")

  df_r <- terra::as.data.frame(r, xy = TRUE, na.rm = FALSE)
  colnames(df_r)[3] <- "radiance"
  df_r$radiance <- pmin(pmax(df_r$radiance, 0), vmax)

  p_ntl <- ggplot(df_r, aes(x = x, y = y, fill = radiance)) +
    geom_raster() +
    scale_fill_gradient(
      low  = "#0d0d0d",
      high = "#ffe97f",
      limits = c(0, vmax),
      na.value = "#0d0d0d"
    ) +
    annotate(
      "text",
      x = Inf, y = Inf,
      label = label,
      hjust = 1.1, vjust = 1.5,
      size = 5, colour = "white", fontface = "bold"
    ) +
    coord_equal(expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "#0d0d0d", colour = NA),
      plot.margin = margin(0, 0, 0, 0)
    )
  if (i == 1) { print(p_ntl) }  # Show first frame in RStudio viewer
  ggsave(
    filename = out_png,
    plot     = p_ntl,
    width    = 5,
    height   = 5,
    dpi      = 150,
    bg       = "#0d0d0d"
  )
}

message(
  "NTL animation frames saved to: ", ntl_png_dir,
  " (", length(ntl_tif_files), " frames)"
)
