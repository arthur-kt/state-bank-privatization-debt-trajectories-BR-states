# Plot level, log and annual-change trajectories in calendar and event time.
source("R/setup.R", local = TRUE)

input_file <- "data/processed/state_panel.csv"
table_dir <- "outputs/exploration/alternative_trajectories/tables"
figure_dir <- "outputs/exploration/alternative_trajectories/figures"
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Run scripts/01_prepare_data.R first.")
}

panel <- read_csv(input_file, show_col_types = FALSE) |>
  arrange(state_code, year)

if (anyDuplicated(panel[c("state_code", "year")]) > 0) {
  stop("Duplicate state-year observations were found.")
}
if (any(panel$financial_debt_brl <= 0, na.rm = TRUE)) {
  stop("Log debt requires strictly positive debt values.")
}

panel <- panel |>
  group_by(state_code) |>
  mutate(
    debt_to_gdp_percent = 100 * debt_to_gdp,
    nominal_debt_billion = financial_debt_brl / 1e9,
    log_nominal_debt = log(financial_debt_brl),
    annual_nominal_change_billion =
      (financial_debt_brl - lag(financial_debt_brl)) / 1e9,
    annual_log_change_percent = 100 *
      (log_nominal_debt - lag(log_nominal_debt)),
    annual_ratio_change_pp =
      debt_to_gdp_percent - lag(debt_to_gdp_percent)
  ) |>
  ungroup()

sample <- panel |>
  filter(loss_of_control == 1 | retained_bank == 1, state_code != "DF") |>
  mutate(
    bank_status = if_else(
      retained_bank == 1,
      "Retained state bank",
      "Lost control of state bank"
    ),
    bank_status = factor(
      bank_status,
      levels = c("Lost control of state bank", "Retained state bank")
    )
  )

group_colors <- c(
  "Lost control of state bank" = "#2C699A",
  "Retained state bank" = "#BA3B46"
)

level_variables <- c(
  "debt_to_gdp_percent",
  "nominal_debt_billion",
  "log_nominal_debt"
)
level_labels <- c(
  "Financial debt / GDP",
  "Nominal financial debt",
  "Log nominal financial debt"
)
change_variables <- c(
  "annual_ratio_change_pp",
  "annual_nominal_change_billion",
  "annual_log_change_percent"
)
change_labels <- c(
  "Annual change in debt / GDP",
  "Annual nominal debt change",
  "Annual log debt change"
)

make_long <- function(data, variables, labels) {
  data |>
    pivot_longer(
      cols = all_of(variables),
      names_to = "outcome",
      values_to = "value"
    ) |>
    mutate(outcome = factor(outcome, levels = variables, labels = labels))
}

save_plot <- function(plot, filename, width = 10, height = 10) {
  ggsave(
    file.path(figure_dir, filename),
    plot,
    width = width,
    height = height,
    dpi = 300
  )
  plot
}

# 1. Overall calendar-year trajectories, without a common event marker

overall_calendar <- sample |>
  group_by(year, bank_status) |>
  summarise(
    states = n_distinct(state_code),
    across(
      all_of(c(level_variables, change_variables)),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

write_csv(
  overall_calendar,
  file.path(table_dir, "overall_calendar_trajectories.csv")
)

overall_levels_plot <- make_long(
  overall_calendar,
  level_variables,
  level_labels
) |>
  ggplot(aes(year, value, color = bank_status)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.7) +
  facet_wrap(vars(outcome), scales = "free_y", ncol = 1) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = seq(1996, 2012, 2)) +
  labs(
    title = "Overall debt trajectories in calendar time",
    subtitle = "Group averages; DF excluded; no common event year imposed",
    x = "Year",
    y = "Outcome level",
    color = "State group",
    caption = paste0(
      "Debt / GDP is in percent; nominal debt is in BRL billions. ",
      "Nominal series are not adjusted for inflation."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

overall_levels_plot <- save_plot(
  overall_levels_plot,
  "01_overall_calendar_levels.png"
)

overall_changes_plot <- overall_calendar |>
  filter(year > 1996) |>
  make_long(change_variables, change_labels) |>
  ggplot(aes(year, value, color = bank_status)) +
  geom_hline(yintercept = 0, color = "grey65", linewidth = 0.4) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.7) +
  facet_wrap(vars(outcome), scales = "free_y", ncol = 1) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = seq(1998, 2012, 2)) +
  labs(
    title = "Overall annual changes in financial debt",
    subtitle = "Group averages in calendar time; DF excluded",
    x = "Year",
    y = "Annual change",
    color = "State group",
    caption = paste0(
      "Debt / GDP change is in percentage points; nominal change is in ",
      "BRL billions; log change approximates percentage growth."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

overall_changes_plot <- save_plot(
  overall_changes_plot,
  "02_overall_calendar_changes.png"
)

# 2. Overall trajectories relative to the institutional event year

event_anchors <- sample |>
  filter(loss_of_control == 1) |>
  distinct(
    comparison_event = state_code,
    comparison_event_year = event_year
  )

treated_event_panel <- sample |>
  filter(loss_of_control == 1) |>
  mutate(
    comparison_event = state_code,
    comparison_event_year = event_year,
    relative_year = year - event_year
  ) |>
  select(
    comparison_event,
    comparison_event_year,
    state_code,
    bank_status,
    relative_year,
    all_of(c(level_variables, change_variables))
  )

retained_event_panel <- event_anchors |>
  cross_join(sample |> filter(retained_bank == 1)) |>
  mutate(relative_year = year - comparison_event_year) |>
  select(
    comparison_event,
    comparison_event_year,
    state_code,
    bank_status,
    relative_year,
    all_of(c(level_variables, change_variables))
  )

overall_event_time <- bind_rows(treated_event_panel, retained_event_panel) |>
  filter(between(relative_year, -4, 8)) |>
  group_by(comparison_event, relative_year, bank_status) |>
  summarise(
    across(
      all_of(c(level_variables, change_variables)),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  group_by(relative_year, bank_status) |>
  summarise(
    supporting_events = n_distinct(comparison_event),
    across(
      all_of(c(level_variables, change_variables)),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

write_csv(
  overall_event_time,
  file.path(table_dir, "overall_event_time_trajectories.csv")
)

overall_event_levels_plot <- make_long(
  overall_event_time,
  level_variables,
  level_labels
) |>
  ggplot(aes(relative_year, value, color = bank_status)) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#6A3D9A",
    linewidth = 0.7
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.7) +
  facet_wrap(vars(outcome), scales = "free_y", ncol = 1) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = -4:8) +
  labs(
    title = "Debt trajectories relative to the bank event",
    subtitle = "Dashed line marks the institutional event year (time 0)",
    x = "Years relative to the loss-of-control event",
    y = "Outcome level",
    color = "State group",
    caption = paste0(
      "Retained-bank states are aligned to each treated-state event. ",
      "Equal-weight descriptive average across events; not a model estimate."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

overall_event_levels_plot <- save_plot(
  overall_event_levels_plot,
  "03_overall_event_time_levels.png"
)

overall_event_changes_long <- make_long(
  overall_event_time,
  change_variables,
  change_labels
) |>
  filter(is.finite(value))

overall_event_changes_plot <- overall_event_changes_long |>
  ggplot(aes(relative_year, value, color = bank_status)) +
  geom_hline(yintercept = 0, color = "grey65", linewidth = 0.4) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#6A3D9A",
    linewidth = 0.7
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.7) +
  facet_wrap(vars(outcome), scales = "free_y", ncol = 1) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = -4:8) +
  labs(
    title = "Annual debt changes relative to the bank event",
    subtitle = "Dashed line marks the institutional event year (time 0)",
    x = "Years relative to the loss-of-control event",
    y = "Annual change",
    color = "State group",
    caption = paste0(
      "Debt / GDP change is in percentage points; nominal change is in ",
      "BRL billions; log change approximates percentage growth."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

overall_event_changes_plot <- save_plot(
  overall_event_changes_plot,
  "04_overall_event_time_changes.png"
)

short_event_changes_plot <- overall_event_changes_long |>
  filter(between(relative_year, -4, 1)) |>
  ggplot(aes(relative_year, value, color = bank_status)) +
  geom_hline(yintercept = 0, color = "grey65", linewidth = 0.4) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#6A3D9A",
    linewidth = 0.7
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(vars(outcome), scales = "free_y", ncol = 1) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = -4:1) +
  labs(
    title = "Annual debt changes through the first post-event year",
    subtitle = "The trajectory ends at event time +1",
    x = "Years relative to the loss-of-control event",
    y = "Annual change",
    color = "State group",
    caption = "Dashed line: institutional event year. DF is excluded."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

short_event_changes_plot <- save_plot(
  short_event_changes_plot,
  "05_event_time_changes_through_year_plus_one.png"
)

# 3. Comparisons within South/Southeast and within the Northeast

regional_sample <- sample |>
  filter(region %in% c("South", "Southeast", "Northeast")) |>
  mutate(
    regional_sample = if_else(
      region == "Northeast",
      "Northeast",
      "South and Southeast"
    ),
    regional_sample = factor(
      regional_sample,
      levels = c("South and Southeast", "Northeast")
    )
  )

regional_calendar <- regional_sample |>
  group_by(regional_sample, year, bank_status) |>
  summarise(
    states = n_distinct(state_code),
    across(
      all_of(c(level_variables, change_variables)),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

write_csv(
  regional_calendar,
  file.path(table_dir, "regional_calendar_trajectories.csv")
)

regional_levels_plot <- make_long(
  regional_calendar,
  level_variables,
  level_labels
) |>
  ggplot(aes(year, value, color = bank_status)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  facet_grid(
    rows = vars(outcome),
    cols = vars(regional_sample),
    scales = "free_y"
  ) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = seq(1996, 2012, 4)) +
  labs(
    title = "Debt levels compared within broad regions",
    subtitle = "Calendar-year averages; no common event year imposed",
    x = "Year",
    y = "Outcome level",
    color = "State group",
    caption = "South/Southeast and Northeast are evaluated separately."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

regional_levels_plot <- save_plot(
  regional_levels_plot,
  "06_regional_calendar_levels.png",
  width = 13
)

regional_changes_plot <- regional_calendar |>
  filter(year > 1996) |>
  make_long(change_variables, change_labels) |>
  ggplot(aes(year, value, color = bank_status)) +
  geom_hline(yintercept = 0, color = "grey65", linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  facet_grid(
    rows = vars(outcome),
    cols = vars(regional_sample),
    scales = "free_y"
  ) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = seq(1998, 2012, 4)) +
  labs(
    title = "Annual debt changes compared within broad regions",
    subtitle = "Calendar-year averages; no common event year imposed",
    x = "Year",
    y = "Annual change",
    color = "State group",
    caption = paste0(
      "Debt / GDP change is in percentage points; nominal change is in ",
      "BRL billions; log change approximates percentage growth."
    )
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

regional_changes_plot <- save_plot(
  regional_changes_plot,
  "07_regional_calendar_changes.png",
  width = 13
)

regional_event_anchors <- regional_sample |>
  filter(loss_of_control == 1) |>
  distinct(
    regional_sample,
    comparison_event = state_code,
    comparison_event_year = event_year
  )

regional_treated_events <- regional_sample |>
  filter(loss_of_control == 1) |>
  mutate(
    comparison_event = state_code,
    comparison_event_year = event_year,
    relative_year = year - event_year
  ) |>
  select(
    regional_sample,
    comparison_event,
    comparison_event_year,
    state_code,
    bank_status,
    relative_year,
    all_of(c(level_variables, change_variables))
  )

regional_retained_events <- regional_event_anchors |>
  inner_join(
    regional_sample |> filter(retained_bank == 1),
    by = "regional_sample",
    relationship = "many-to-many"
  ) |>
  mutate(relative_year = year - comparison_event_year) |>
  select(
    regional_sample,
    comparison_event,
    comparison_event_year,
    state_code,
    bank_status,
    relative_year,
    all_of(c(level_variables, change_variables))
  )

regional_event_time <- bind_rows(
  regional_treated_events,
  regional_retained_events
) |>
  filter(between(relative_year, -4, 8)) |>
  group_by(
    regional_sample,
    comparison_event,
    relative_year,
    bank_status
  ) |>
  summarise(
    across(
      all_of(c(level_variables, change_variables)),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  group_by(regional_sample, relative_year, bank_status) |>
  summarise(
    supporting_events = n_distinct(comparison_event),
    across(
      all_of(c(level_variables, change_variables)),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

write_csv(
  regional_event_time,
  file.path(table_dir, "regional_event_time_trajectories.csv")
)

regional_event_levels_plot <- make_long(
  regional_event_time,
  level_variables,
  level_labels
) |>
  ggplot(aes(relative_year, value, color = bank_status)) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#6A3D9A",
    linewidth = 0.6
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  facet_grid(
    rows = vars(outcome),
    cols = vars(regional_sample),
    scales = "free_y"
  ) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = seq(-4, 8, 2)) +
  labs(
    title = "Regional debt levels relative to the bank event",
    subtitle = "Dashed line marks the institutional event year (time 0)",
    x = "Years relative to the loss-of-control event",
    y = "Outcome level",
    color = "State group",
    caption = paste0(
      "Retained states are aligned only to events in the same region. ",
      "Equal-weight descriptive average across events."
    )
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

regional_event_levels_plot <- save_plot(
  regional_event_levels_plot,
  "08_regional_event_time_levels.png",
  width = 13
)

regional_event_changes_plot <- make_long(
  regional_event_time,
  change_variables,
  change_labels
) |>
  filter(is.finite(value)) |>
  ggplot(aes(relative_year, value, color = bank_status)) +
  geom_hline(yintercept = 0, color = "grey65", linewidth = 0.4) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#6A3D9A",
    linewidth = 0.6
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  facet_grid(
    rows = vars(outcome),
    cols = vars(regional_sample),
    scales = "free_y"
  ) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = seq(-4, 8, 2)) +
  labs(
    title = "Regional annual debt changes relative to the bank event",
    subtitle = "Dashed line marks the institutional event year (time 0)",
    x = "Years relative to the loss-of-control event",
    y = "Annual change",
    color = "State group",
    caption = paste0(
      "Retained states are aligned only to events in the same region. ",
      "These are descriptive trajectories, not model estimates."
    )
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

regional_event_changes_plot <- save_plot(
  regional_event_changes_plot,
  "09_regional_event_time_changes.png",
  width = 13
)

# 4. State trajectories with each state's event year marked

state_event_lines <- regional_sample |>
  filter(loss_of_control == 1) |>
  distinct(state_code, event_year)

state_log_plot <- ggplot(
  regional_sample,
  aes(year, log_nominal_debt, color = bank_status)
) +
  geom_vline(
    data = state_event_lines,
    aes(xintercept = event_year),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "#6A3D9A",
    linewidth = 0.55
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.1) +
  facet_wrap(vars(state_code), scales = "free_y", ncol = 5) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = c(1996, 2000, 2004, 2008, 2012)) +
  labs(
    title = "Log nominal financial debt by state",
    subtitle = "Dashed line marks each state's loss-of-control event",
    x = "Year",
    y = "Log nominal financial debt",
    color = "State group",
    caption = paste0(
      "South, Southeast and Northeast samples. ",
      "Retained states have no event line."
    )
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

state_log_plot <- save_plot(
  state_log_plot,
  "10_state_log_debt_with_events.png",
  width = 14
)

state_log_change_plot <- regional_sample |>
  filter(year > 1996) |>
  ggplot(aes(year, annual_log_change_percent, color = bank_status)) +
  geom_hline(yintercept = 0, color = "grey65", linewidth = 0.4) +
  geom_vline(
    data = state_event_lines,
    aes(xintercept = event_year),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "#6A3D9A",
    linewidth = 0.55
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.1) +
  facet_wrap(vars(state_code), scales = "free_y", ncol = 5) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = c(1998, 2002, 2006, 2010)) +
  labs(
    title = "Annual log change in nominal financial debt by state",
    subtitle = "Dashed line marks each state's loss-of-control event",
    x = "Year",
    y = "Annual log change (approximately percent)",
    color = "State group",
    caption = "Nominal growth includes the effect of inflation."
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

state_log_change_plot <- save_plot(
  state_log_change_plot,
  "11_state_log_debt_change_with_events.png",
  width = 14
)

state_ratio_change_plot <- regional_sample |>
  filter(year > 1996) |>
  ggplot(aes(year, annual_ratio_change_pp, color = bank_status)) +
  geom_hline(yintercept = 0, color = "grey65", linewidth = 0.4) +
  geom_vline(
    data = state_event_lines,
    aes(xintercept = event_year),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "#6A3D9A",
    linewidth = 0.55
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.1) +
  facet_wrap(vars(state_code), scales = "free_y", ncol = 5) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = c(1998, 2002, 2006, 2010)) +
  labs(
    title = "Annual change in financial debt / GDP by state",
    subtitle = "Dashed line marks each state's loss-of-control event",
    x = "Year",
    y = "Annual change in debt / GDP (percentage points)",
    color = "State group",
    caption = "South, Southeast and Northeast samples; DF is excluded."
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

state_ratio_change_plot <- save_plot(
  state_ratio_change_plot,
  "12_state_debt_ratio_change_with_events.png",
  width = 14
)

if (interactive()) {
  print(overall_levels_plot)
  print(overall_changes_plot)
  print(overall_event_levels_plot)
  print(overall_event_changes_plot)
  print(short_event_changes_plot)
  print(regional_levels_plot)
  print(regional_changes_plot)
  print(regional_event_levels_plot)
  print(regional_event_changes_plot)
  print(state_log_plot)
  print(state_log_change_plot)
  print(state_ratio_change_plot)
}

message("Alternative-outcome trajectory analysis completed.")
message("Tables: ", table_dir)
message("Figures: ", figure_dir)
