# Align calendar and relative-time trajectories for saved state pairs.
source("R/setup.R", local = TRUE)

panel_file <- "data/processed/state_panel.csv"
pair_file <- "outputs/exploration/bank_matching/tables/matched_state_pairs_1996.csv"

output_dir <- "outputs/exploration/matched_trajectories/tables"
figure_dir <- "outputs/exploration/matched_trajectories/figures"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(panel_file) || !file.exists(pair_file)) {
  stop("Run the preparation and matching scripts before this script.")
}

show_plots_in_rstudio <- cfg$show_plots

event_window_start <- -5L
event_window_end <- 8L

# 2. Build the matched-pair panel

panel <- read_csv(panel_file, show_col_types = FALSE)

recommended_pairs <- read_csv(pair_file, show_col_types = FALSE) %>%
  filter(matching_specification == "combined_distance") %>%
  select(
    retained_state,
    treated_state,
    matching_distance = distance
  )

treated_events <- panel %>%
  distinct(
    state_code,
    main_bank,
    event_type,
    event_year,
    first_full_treatment_year
  ) %>%
  filter(state_code %in% recommended_pairs$treated_state) %>%
  rename(
    treated_state = state_code,
    treated_bank = main_bank,
    treated_event_type = event_type,
    pair_event_year = event_year,
    pair_first_full_treatment_year = first_full_treatment_year
  )

pair_definitions <- recommended_pairs %>%
  left_join(treated_events, by = "treated_state") %>%
  mutate(
    pair_label = paste(retained_state, "vs", treated_state),
    includes_federal_district = retained_state == "DF"
  )

if (any(is.na(pair_definitions$pair_first_full_treatment_year))) {
  stop("At least one matched treated state has no valid treatment year.")
}

retained_panel <- pair_definitions %>%
  select(
    pair_label,
    retained_state,
    treated_state,
    treated_bank,
    treated_event_type,
    pair_event_year,
    pair_first_full_treatment_year,
    matching_distance,
    includes_federal_district
  ) %>%
  left_join(panel, by = c("retained_state" = "state_code")) %>%
  mutate(
    state_code = retained_state,
    pair_role = "Bank retained"
  )

loss_control_panel <- pair_definitions %>%
  select(
    pair_label,
    retained_state,
    treated_state,
    treated_bank,
    treated_event_type,
    pair_event_year,
    pair_first_full_treatment_year,
    matching_distance,
    includes_federal_district
  ) %>%
  left_join(panel, by = c("treated_state" = "state_code")) %>%
  mutate(
    state_code = treated_state,
    pair_role = "Lost bank control"
  )

matched_panel <- bind_rows(retained_panel, loss_control_panel) %>%
  mutate(
    assigned_event_time = year - pair_first_full_treatment_year,
    pair_role = factor(
      pair_role,
      levels = c("Bank retained", "Lost bank control")
    )
  ) %>%
  arrange(pair_label, pair_role, year)

write_csv(
  pair_definitions,
  file.path(output_dir, "matched_pair_definitions.csv")
)

write_csv(
  matched_panel,
  file.path(output_dir, "matched_pair_panel_1996_2012.csv")
)

# 3. Common plot formatting

group_colors <- c(
  "Bank retained" = "#D73027",
  "Lost bank control" = "#2166AC"
)

matched_theme <- theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9.5)
  )

save_and_show <- function(plot_object, file_name, width, height) {
  ggsave(
    file.path(figure_dir, file_name),
    plot_object,
    width = width,
    height = height,
    dpi = 300
  )

  if (show_plots_in_rstudio) {
    print(plot_object)
  }
}

# 4. Calendar-year trajectories for each matched pair

pair_event_markers <- pair_definitions %>%
  transmute(
    pair_label,
    pair_event_year,
    event_label = paste0(
      treated_bank,
      ": ",
      str_replace_all(treated_event_type, "_", " "),
      " (", pair_event_year, ")"
    )
  )

calendar_debt_plot <- ggplot(
  matched_panel,
  aes(x = year, y = debt_to_gdp, color = pair_role)
) +
  geom_vline(
    data = pair_event_markers,
    aes(xintercept = pair_event_year),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "#6A3D9A",
    linewidth = 0.7
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~ pair_label, ncol = 2) +
  scale_x_continuous(breaks = seq(min(panel$year), max(panel$year), by = 4)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = group_colors) +
  labs(
    title = "Financial debt trajectories within matched state pairs",
    subtitle = paste0(
      "Dashed line: loss-of-control event in the matched state. ",
      "The retained state is aligned to the same date only for comparison."
    ),
    x = "Year",
    y = "Financial debt / GDP",
    color = "State role"
  ) +
  matched_theme

save_and_show(
  calendar_debt_plot,
  "01_pair_calendar_debt_to_gdp.png",
  width = 11,
  height = 10
)

calendar_nominal_debt_plot <- ggplot(
  matched_panel,
  aes(x = year, y = financial_debt_brl / 1e9, color = pair_role)
) +
  geom_vline(
    data = pair_event_markers,
    aes(xintercept = pair_event_year),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "#6A3D9A",
    linewidth = 0.7
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~ pair_label, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = seq(min(panel$year), max(panel$year), by = 4)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  scale_color_manual(values = group_colors) +
  labs(
    title = "Nominal financial debt within matched state pairs",
    subtitle = "Current BRL values; vertical scales vary across pairs",
    x = "Year",
    y = "Financial debt (R$ billions)",
    color = "State role"
  ) +
  matched_theme

save_and_show(
  calendar_nominal_debt_plot,
  "02_pair_calendar_nominal_financial_debt.png",
  width = 11,
  height = 10
)

calendar_gdp_plot <- ggplot(
  matched_panel,
  aes(x = year, y = state_gdp_nominal_brl / 1e9, color = pair_role)
) +
  geom_vline(
    data = pair_event_markers,
    aes(xintercept = pair_event_year),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "#6A3D9A",
    linewidth = 0.7
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~ pair_label, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = seq(min(panel$year), max(panel$year), by = 4)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  scale_color_manual(values = group_colors) +
  labs(
    title = "Nominal GDP within matched state pairs",
    subtitle = "Current BRL values; vertical scales vary across pairs",
    x = "Year",
    y = "State GDP (R$ billions)",
    color = "State role"
  ) +
  matched_theme

save_and_show(
  calendar_gdp_plot,
  "03_pair_calendar_nominal_gdp.png",
  width = 11,
  height = 10
)

# 5. Event-time trajectories

event_window_data <- matched_panel %>%
  filter(between(
    assigned_event_time,
    event_window_start,
    event_window_end
  ))

pair_event_time_plot <- ggplot(
  event_window_data,
  aes(
    x = assigned_event_time,
    y = debt_to_gdp,
    color = pair_role
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#6A3D9A") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  facet_wrap(~ pair_label, ncol = 2) +
  scale_x_continuous(breaks = event_window_start:event_window_end) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = group_colors) +
  labs(
    title = "Matched-pair debt trajectories in event time",
    subtitle = "Event time 0 is the first full year after the matched state's loss of bank control",
    x = "Years relative to the first full treatment year",
    y = "Financial debt / GDP",
    color = "State role"
  ) +
  matched_theme

save_and_show(
  pair_event_time_plot,
  "04_pair_event_time_debt_to_gdp.png",
  width = 11,
  height = 10
)

matched_group_event_summary <- event_window_data %>%
  group_by(assigned_event_time, pair_role) %>%
  summarise(
    states = n_distinct(state_code),
    mean_debt_to_gdp = mean(debt_to_gdp),
    .groups = "drop"
  )

write_csv(
  matched_group_event_summary,
  file.path(output_dir, "matched_group_event_time_summary.csv")
)

average_event_plot <- ggplot(
  matched_group_event_summary,
  aes(
    x = assigned_event_time,
    y = mean_debt_to_gdp,
    color = pair_role
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#6A3D9A") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = event_window_start:event_window_end) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = group_colors) +
  labs(
    title = "Average financial debt in the matched sample",
    subtitle = "Five retained-bank states and their five matched loss-of-control states",
    x = "Years relative to the first full treatment year",
    y = "Average financial debt / GDP",
    color = "State role"
  ) +
  matched_theme

save_and_show(
  average_event_plot,
  "05_average_matched_event_time_debt_to_gdp.png",
  width = 10,
  height = 6
)

# 6. Federal District sensitivity comparison

df_sensitivity_summary <- bind_rows(
  event_window_data %>%
    mutate(sample = "All five matched pairs"),
  event_window_data %>%
    filter(!includes_federal_district) %>%
    mutate(sample = "Excluding DF-PE")
) %>%
  group_by(sample, assigned_event_time, pair_role) %>%
  summarise(
    states = n_distinct(state_code),
    mean_debt_to_gdp = mean(debt_to_gdp),
    .groups = "drop"
  )

write_csv(
  df_sensitivity_summary,
  file.path(output_dir, "df_sensitivity_event_time_summary.csv")
)

df_sensitivity_plot <- ggplot(
  df_sensitivity_summary,
  aes(
    x = assigned_event_time,
    y = mean_debt_to_gdp,
    color = pair_role
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#6A3D9A") +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  facet_wrap(~ sample, ncol = 1) +
  scale_x_continuous(breaks = event_window_start:event_window_end) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = group_colors) +
  labs(
    title = "Sensitivity of matched trajectories to the Federal District",
    subtitle = "The lower panel removes both DF and its matched comparison state, Pernambuco",
    x = "Years relative to the first full treatment year",
    y = "Average financial debt / GDP",
    color = "State role"
  ) +
  matched_theme

save_and_show(
  df_sensitivity_plot,
  "06_df_sensitivity_event_time_debt_to_gdp.png",
  width = 10,
  height = 9
)
