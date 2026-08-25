# ==============================================================================
# Script: 10_figures_tables.R
# Description:
#   Produces only the publication-ready tables and figures used in the paper
#   and appendix. All estimation is performed upstream in Scripts 08 and 09.
# ============================================================================== 

# ==============================================================================
# 1. OUTPUT DIRECTORIES
# ==============================================================================

table_dir <- here("Output/Tables/final")
figure_dir <- here("Output/Figures/final")

fs::dir_create(table_dir, recurse = TRUE)
fs::dir_create(figure_dir, recurse = TRUE)

# ==============================================================================
# 2. LOAD OBJECTS USED IN THE PAPER
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

# Event-study models from Script 08 -------------------------------------------

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

# Robustness models from Script 09 --------------------------------------------

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

# Summary data ----------------------------------------------------------------

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
# 3. SMALL HELPERS
# ==============================================================================

extract_term <- function(model, term_name) {
  broom::tidy(model) %>%
    filter(term == term_name) %>%
    transmute(
      estimate,
      std_error = std.error,
      p_value = p.value,
      nobs = stats::nobs(model)
    )
}

stars_from_p <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

fmt_est <- function(estimate, p_value, digits = 3) {
  paste0(
    "$",
    sprintf(paste0("%.", digits, "f"), estimate),
    "^{",
    stars_from_p(p_value),
    "}$"
  )
}

fmt_se <- function(x, digits = 3) {
  paste0("(", sprintf(paste0("%.", digits, "f"), x), ")")
}

fmt_n <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_p <- function(x) {
  ifelse(
    x < 0.0001,
    "$<0.0001$",
    sprintf("%.4f", x)
  )
}

write_tex <- function(x, filename) {
  writeLines(
    x,
    con = file.path(table_dir, filename)
  )
}

extract_event_data <- function(model, reference_period = -1L) {
  out <- broom::tidy(model, conf.int = TRUE) %>%
    filter(grepl("^event_time::", term)) %>%
    mutate(
      event_time = stringr::str_extract(term, "-?\\d+") %>%
        as.integer()
    ) %>%
    select(event_time, estimate, conf.low, conf.high, p.value)

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

results_main <- bind_rows(
  extract_term(m1_original, "exposure_post"),
  extract_term(m2_country_time, "exposure_post"),
  extract_term(m5_corridor_time, "exposure_post"),
  extract_term(m6_region_time, "exposure_post")
) %>%
  mutate(
    estimate_tex = map2_chr(estimate, p_value, fmt_est),
    se_tex = map_chr(std_error, fmt_se),
    n_tex = map_chr(nobs, fmt_n)
  )

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
  " & TWFE & Country $\\times$ Quarter & Crossing $\\times$ Quarter & State/Dept. $\\times$ Quarter \\\\",
  "\\midrule",
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
  "Municipality FE & Yes & Yes & Yes & Yes \\\\",
  "Quarter FE & Yes & No & No & No \\\\",
  "Country $\\times$ Quarter FE & No & Yes & Yes & No \\\\",
  "Crossing corridor $\\times$ Quarter FE & No & No & Yes & No \\\\",
  "State/Department $\\times$ Quarter FE & No & No & No & Yes \\\\",
  "\\midrule",
  paste0(
    "Observations",
    " & ", results_main$n_tex[1],
    " & ", results_main$n_tex[2],
    " & ", results_main$n_tex[3],
    " & ", results_main$n_tex[4],
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
    "Treatment intensity is the logarithmic change in road-network distance ",
    "to the nearest operational international crossing between the ",
    "pre-treatment and closure regimes. Post equals one from 2019Q1 onward. ",
    "Standard errors in parentheses are clustered at the municipality level. ",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  "\\end{minipage}",
  "\\end{table}"
)

write_tex(main_table_tex, "table_main_results.tex")

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

ggsave(
  filename = file.path(
    figure_dir,
    "figure_main_coefficient_sensitivity.pdf"
  ),
  plot = p_main_coef,
  width = 8,
  height = 4.5,
  device = cairo_pdf,
  bg = "white"
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

ggsave(
  filename = file.path(
    figure_dir,
    "figure_event_study_region_fe.pdf"
  ),
  plot = p_event_region,
  width = 8,
  height = 5,
  device = cairo_pdf,
  bg = "white"
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
    geometry = st_union(geometry),
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

ggsave(
  filename = file.path(
    figure_dir,
    "figure_border_crossings.pdf"
  ),
  plot = p_border_crossings,
  width = 8,
  height = 8,
  device = cairo_pdf,
  bg = "white"
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

# ==============================================================================
# 11. APPENDIX TABLE: ALTERNATIVE SPECIFICATIONS
# ==============================================================================

robustness_specs <- bind_rows(
  extract_term(r0_reference, "exposure_post") %>%
    mutate(
      Specification = "Reference",
      Outcome = "Log nighttime lights",
      Treatment = "Log distance shock"
    ),
  extract_term(r1_absolute_distance, "distance_change_post") %>%
    mutate(
      Specification = "Absolute distance",
      Outcome = "Log nighttime lights",
      Treatment = "Distance increase (100 km)"
    ),
  extract_term(r2_levels, "exposure_post") %>%
    mutate(
      Specification = "NTL levels",
      Outcome = "Nighttime-light levels",
      Treatment = "Log distance shock"
    ),
  extract_term(r3_municipality_trends, "exposure_post") %>%
    mutate(
      Specification = "Municipality trends",
      Outcome = "Log nighttime lights",
      Treatment = "Log distance shock"
    )
) %>%
  mutate(
    Estimate = map2_chr(estimate, p_value, fmt_est),
    SE = map_chr(std_error, fmt_se),
    N = map_chr(nobs, fmt_n)
  )

robustness_rows <- robustness_specs %>%
  mutate(
    row = paste0(
      Specification,
      " & ", Outcome,
      " & ", Treatment,
      " & ", Estimate,
      " & ", SE,
      " & ", N,
      " \\\\"
    )
  ) %>%
  pull(row)

robustness_tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Alternative Specifications}",
  "\\label{tab:robustness_specs}",
  "\\scriptsize",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\begin{tabular}{lllccc}",
  "\\toprule",
  "Specification & Outcome & Treatment & Estimate & Std. Error & Observations \\\\",
  "\\midrule",
  robustness_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\vspace{0.2cm}",
  "\\begin{minipage}{0.95\\textwidth}",
  "\\footnotesize",
  paste0(
    "\\textit{Notes:} The reference, absolute-distance, and nighttime-light-level ",
    "specifications include municipality and state/department-by-quarter fixed ",
    "effects. The municipality-trend specification additionally allows a separate ",
    "linear time trend for each municipality. Standard errors are clustered at the ",
    "municipality level. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  "\\end{minipage}",
  "\\end{table}"
)

write_tex(robustness_tex, "table_robustness_specs.tex")

# ==============================================================================
# 12. APPENDIX TABLE: COUNTRY-SPECIFIC RESULTS
# ==============================================================================

main_col <- extract_term(m3_country_effects, "exposure_post_colombia")
main_ven <- extract_term(m3_country_effects, "exposure_post_venezuela")
rob_col <- extract_term(r4_colombia, "exposure_post")
rob_ven <- extract_term(r5_venezuela, "exposure_post")

country_cell <- function(x) {
  paste0(
    fmt_est(x$estimate, x$p_value),
    " ",
    fmt_se(x$std_error)
  )
}

country_tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Country-Specific Estimates}",
  "\\label{tab:country_heterogeneity}",
  "\\small",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & Main country-specific model & State/Department robustness \\\\",
  "\\midrule",
  paste0(
    "Colombia & ", country_cell(main_col),
    " & ", country_cell(rob_col),
    " \\\\"
  ),
  paste0(
    "Venezuela & ", country_cell(main_ven),
    " & ", country_cell(rob_ven),
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\vspace{0.2cm}",
  "\\begin{minipage}{0.92\\textwidth}",
  "\\footnotesize",
  paste0(
    "\\textit{Notes:} Entries report coefficients with clustered standard errors ",
    "in parentheses. The main country-specific model is estimated jointly with ",
    "municipality and country-by-quarter fixed effects. The robustness columns ",
    "estimate each country separately using municipality and state/department-by-quarter ",
    "fixed effects. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  "\\end{minipage}",
  "\\end{table}"
)

write_tex(country_tex, "table_country_heterogeneity.tex")

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

ggsave(
  filename = file.path(
    figure_dir,
    "figure_country_event_studies.pdf"
  ),
  plot = p_country_events,
  width = 11,
  height = 5,
  device = cairo_pdf,
  bg = "white"
)

# ==============================================================================
# 14. APPENDIX TABLE AND FIGURE: BORDER REOPENING
# ==============================================================================

reopening_result <- extract_term(
  r8_reopening,
  "reopening_exposure"
)

reopening_tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Border-Reopening Diagnostic}",
  "\\label{tab:reopening_result}",
  "\\small",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  "Variable & Estimate & Std. Error & $p$-value & Observations \\\\",
  "\\midrule",
  paste0(
    "Prior exposure $\\times$ Reopening",
    " & ", sprintf("%.4f", reopening_result$estimate),
    " & ", sprintf("%.4f", reopening_result$std_error),
    " & ", fmt_p(reopening_result$p_value),
    " & ", fmt_n(reopening_result$nobs),
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\vspace{0.2cm}",
  "\\begin{minipage}{0.90\\textwidth}",
  "\\footnotesize",
  paste0(
    "\\textit{Notes:} The dependent variable is log nighttime luminosity. ",
    "The specification includes municipality and state/department-by-quarter ",
    "fixed effects, with standard errors clustered at the municipality level. ",
    "The reopening exercise is interpreted as a descriptive reverse-shock ",
    "diagnostic rather than an independent causal estimate."
  ),
  "\\end{minipage}",
  "\\end{table}"
)

write_tex(reopening_tex, "table_reopening_result.tex")

event_reopening_df <- extract_event_data(e_reopening)

p_reopening <- plot_event_study(
  event_reopening_df,
  "Coefficient on prior treatment intensity"
) +
  labs(
    x = "Quarters relative to the 2022 reopening"
  )

ggsave(
  filename = file.path(
    figure_dir,
    "figure_reopening_event_study.pdf"
  ),
  plot = p_reopening,
  width = 8,
  height = 5,
  device = cairo_pdf,
  bg = "white"
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
