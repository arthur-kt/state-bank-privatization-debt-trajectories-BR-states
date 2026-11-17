# Estimate common-event-cohort DiD and export aligned descriptive trajectories.
source("R/setup.R", local = TRUE)

source("R/extension_data.R", local = TRUE)
table_dir <- "outputs/robustness/same_year_did/tables"
figure_dir <- "outputs/robustness/same_year_did/figures"
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
cohort_composition <- causal_sample |>
  filter(loss_of_control == 1) |>
  distinct(state_code, event_year, first_full_treatment_year) |>
  count(event_year, first_full_treatment_year, name = "treated_states") |>
  filter(treated_states >= 2) |>
  arrange(event_year)

control_states <- causal_sample |>
  filter(retained_bank == 1) |>
  distinct(state_code) |>
  pull(state_code)

same_year_did_results <- list()
same_year_trajectory_data <- list()
same_year_extended_trajectory_data <- list()

for (row_index in seq_len(nrow(cohort_composition))) {
  cohort_event_year <- cohort_composition$event_year[row_index]
  cohort_treatment_year <- cohort_composition$first_full_treatment_year[row_index]

  cohort_treated_states <- causal_sample |>
    filter(
      loss_of_control == 1,
      event_year == cohort_event_year
    ) |>
    distinct(state_code) |>
    pull(state_code)

  cohort_data <- causal_sample |>
    filter(
      state_code %in% c(cohort_treated_states, control_states),
      between(
        year,
        cohort_treatment_year - 2L,
        cohort_treatment_year + 1L
      )
    ) |>
    mutate(
      cohort_event_year = cohort_event_year,
      cohort_treatment_year = cohort_treatment_year,
      relative_year = year - cohort_treatment_year,
      treated_group = as.integer(state_code %in% cohort_treated_states),
      post_period = as.integer(year >= cohort_treatment_year),
      group_label = if_else(
        treated_group == 1,
        "Same-year event states",
        "Retained-bank controls"
      )
    )

  did_model <- feols(
    debt_to_gdp ~ treated_group:post_period | state_code + year,
    data = cohort_data,
    cluster = ~ state_code
  )

  did_estimate <- tidy(did_model, conf.int = TRUE) |>
    filter(term == "treated_group:post_period")

  group_period_means <- cohort_data |>
    group_by(treated_group, post_period) |>
    summarise(mean_debt_to_gdp = mean(debt_to_gdp), .groups = "drop") |>
    mutate(
      cell = case_when(
        treated_group == 1 & post_period == 1 ~ "treated_post",
        treated_group == 1 & post_period == 0 ~ "treated_pre",
        treated_group == 0 & post_period == 1 ~ "control_post",
        TRUE ~ "control_pre"
      )
    ) |>
    select(cell, mean_debt_to_gdp) |>
    pivot_wider(names_from = cell, values_from = mean_debt_to_gdp)

  pretrend_means <- cohort_data |>
    filter(relative_year %in% c(-2L, -1L)) |>
    group_by(treated_group, relative_year) |>
    summarise(mean_debt_to_gdp = mean(debt_to_gdp), .groups = "drop") |>
    mutate(
      cell = paste0(
        if_else(treated_group == 1, "treated", "control"),
        "_",
        if_else(relative_year == -2L, "minus2", "minus1")
      )
    ) |>
    select(cell, mean_debt_to_gdp) |>
    pivot_wider(names_from = cell, values_from = mean_debt_to_gdp)

  same_year_did_results[[row_index]] <- did_estimate |>
    transmute(
      event_year = cohort_event_year,
      first_full_treatment_year = cohort_treatment_year,
      treated_states = length(cohort_treated_states),
      treated_state_codes = paste(sort(cohort_treated_states), collapse = ", "),
      control_states = length(control_states),
      control_state_codes = paste(sort(control_states), collapse = ", "),
      pre_window = paste0(
        cohort_treatment_year - 2L,
        "-",
        cohort_treatment_year - 1L
      ),
      post_window = paste0(
        cohort_treatment_year,
        "-",
        cohort_treatment_year + 1L
      ),
      estimate = estimate,
      standard_error = std.error,
      statistic,
      p_value = p.value,
      confidence_low = conf.low,
      confidence_high = conf.high,
      estimate_percentage_points = 100 * estimate,
      confidence_low_percentage_points = 100 * confidence_low,
      confidence_high_percentage_points = 100 * confidence_high,
      manual_did_percentage_points = 100 * (
        group_period_means$treated_post -
          group_period_means$treated_pre -
          group_period_means$control_post +
          group_period_means$control_pre
      ),
      pretrend_difference_percentage_points = 100 * (
        pretrend_means$treated_minus1 -
          pretrend_means$treated_minus2 -
          pretrend_means$control_minus1 +
          pretrend_means$control_minus2
      )
    )

  same_year_trajectory_data[[row_index]] <- cohort_data |>
    group_by(
      cohort_event_year,
      cohort_treatment_year,
      relative_year,
      group_label
    ) |>
    summarise(
      states = n_distinct(state_code),
      mean_debt_to_gdp = mean(debt_to_gdp),
      .groups = "drop"
    )

  same_year_extended_trajectory_data[[row_index]] <- causal_sample |>
    filter(
      state_code %in% c(cohort_treated_states, control_states),
      year >= cohort_treatment_year - 2L
    ) |>
    mutate(
      cohort_event_year = cohort_event_year,
      cohort_treatment_year = cohort_treatment_year,
      relative_year = year - cohort_treatment_year,
      group_label = if_else(
        state_code %in% cohort_treated_states,
        "Same-year event states",
        "Retained-bank controls"
      )
    ) |>
    group_by(
      cohort_event_year,
      cohort_treatment_year,
      relative_year,
      group_label
    ) |>
    summarise(
      states = n_distinct(state_code),
      mean_debt_to_gdp = mean(debt_to_gdp),
      .groups = "drop"
    )
}

same_year_did_results <- bind_rows(same_year_did_results)
same_year_trajectory_data <- bind_rows(same_year_trajectory_data)
same_year_extended_trajectory_data <- bind_rows(
  same_year_extended_trajectory_data
)

stopifnot(
  max(abs(
    same_year_did_results$estimate_percentage_points -
      same_year_did_results$manual_did_percentage_points
  )) < 1e-8
)

write_csv(
  cohort_composition,
  file.path(table_dir, "same_year_event_cohort_composition.csv")
)
write_csv(
  same_year_did_results,
  file.path(table_dir, "same_year_event_classic_did_results.csv")
)
write_csv(
  same_year_trajectory_data,
  file.path(table_dir, "same_year_event_trajectories.csv")
)
write_csv(
  same_year_extended_trajectory_data,
  file.path(table_dir, "same_year_event_extended_trajectories.csv")
)

same_year_did_plot <- same_year_did_results |>
  mutate(
    cohort_label = paste0(
      "Event in ",
      event_year,
      " (", treated_state_codes, ")"
    ),
    cohort_label = fct_reorder(cohort_label, event_year)
  ) |>
  ggplot(aes(estimate_percentage_points, cohort_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
  geom_errorbar(
    aes(
      xmin = confidence_low_percentage_points,
      xmax = confidence_high_percentage_points
    ),
    orientation = "y",
    width = 0.18,
    color = "#2C699A"
  ) +
  geom_point(size = 3, color = "#2C699A") +
  labs(
    title = "Classic DiD estimates for common event-year cohorts",
    subtitle = paste("Two pre years and two full post years; controls:", paste(control_states, collapse = ", ")),
    x = "DiD estimate for financial debt / GDP (percentage points)",
    y = NULL,
    caption = "The event year is a transition year and is not included in the post window."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(figure_dir, "02_same_year_event_classic_did.png"),
  same_year_did_plot,
  width = 10,
  height = 6,
  dpi = 300
)

cohort_trajectory_plot <- ggplot(
  same_year_trajectory_data,
  aes(relative_year, mean_debt_to_gdp, color = group_label)
) +
  geom_vline(xintercept = -cfg$treatment_lag, linetype = "dashed", color = "#6A3D9A") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(
    vars(cohort_event_year),
    scales = "free_y",
    labeller = labeller(
      cohort_event_year = function(x) paste("Event in", x)
    )
  ) +
  scale_x_continuous(breaks = -2:1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c(
      "Retained-bank controls" = "#B23A48",
      "Same-year event states" = "#2C699A"
    )
  ) +
  labs(
    title = "Debt trajectories for common event-year cohorts",
    subtitle = "Relative year 0 is the first full calendar year after the event",
    x = "Years relative to first full treatment year",
    y = "Average financial debt / GDP",
    color = "State group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  file.path(figure_dir, "03_same_year_event_trajectories.png"),
  cohort_trajectory_plot,
  width = 11,
  height = 8,
  dpi = 300
)

# 3.1 Extended trajectories: immediate years and the end of the panel

common_max_relative_year <- same_year_extended_trajectory_data |>
  group_by(cohort_event_year) |>
  summarise(max_relative_year = max(relative_year), .groups = "drop") |>
  summarise(common_max_relative_year = min(max_relative_year)) |>
  pull(common_max_relative_year)

overall_extended_trajectory <- same_year_extended_trajectory_data |>
  filter(relative_year <= common_max_relative_year) |>
  group_by(relative_year, group_label) |>
  summarise(
    mean_debt_to_gdp = mean(mean_debt_to_gdp),
    contributing_cohorts = n_distinct(cohort_event_year),
    .groups = "drop"
  )

write_csv(
  overall_extended_trajectory,
  file.path(table_dir, "overall_extended_event_trajectory.csv")
)

overall_extended_plot <- ggplot(
  overall_extended_trajectory,
  aes(relative_year, mean_debt_to_gdp, color = group_label)
) +
  annotate(
    "rect",
    xmin = -0.5,
    xmax = 1.5,
    ymin = -Inf,
    ymax = Inf,
    fill = "#FDE9A9",
    alpha = 0.28
  ) +
  annotate(
    "rect",
    xmin = 3.5,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    fill = "#D9EAF7",
    alpha = 0.28
  ) +
  geom_vline(xintercept = -1, linetype = "dashed", color = "#6A3D9A") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  scale_x_continuous(
    breaks = seq(-2, max(overall_extended_trajectory$relative_year), by = 2)
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c(
      "Retained-bank controls" = "#B23A48",
      "Same-year event states" = "#2C699A"
    )
  ) +
  labs(
    title = "Debt trajectories from the event through the end of the panel",
    subtitle = paste(
      "Cohort-weighted averages; dashed line is the institutional event year",
      "Yellow: immediate window (k = 0 to 1); blue: long run (k >= 4)",
      sep = "\n"
    ),
    x = "Years relative to the first full treatment year",
    y = "Average financial debt / GDP",
    color = "State group",
    caption = paste0(
      "Common support retains the event-year cohorts through k = ",
      common_max_relative_year,
      "."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

ggsave(
  file.path(figure_dir, "05_overall_extended_event_trajectory.png"),
  overall_extended_plot,
  width = 11,
  height = 7.5,
  dpi = 300
)

first_year_trajectory <- overall_extended_trajectory |>
  filter(relative_year %in% -2:0)

treated_event <- first_year_trajectory |>
  filter(
    relative_year == -1,
    group_label == "Same-year event states"
  ) |>
  pull(mean_debt_to_gdp)
treated_first_post <- first_year_trajectory |>
  filter(
    relative_year == 0,
    group_label == "Same-year event states"
  ) |>
  pull(mean_debt_to_gdp)
control_event <- first_year_trajectory |>
  filter(
    relative_year == -1,
    group_label == "Retained-bank controls"
  ) |>
  pull(mean_debt_to_gdp)
control_first_post <- first_year_trajectory |>
  filter(
    relative_year == 0,
    group_label == "Retained-bank controls"
  ) |>
  pull(mean_debt_to_gdp)

first_year_descriptive_did_pp <- 100 * (
  treated_first_post - treated_event -
    control_first_post + control_event
)

first_year_plot <- ggplot(
  first_year_trajectory,
  aes(relative_year, mean_debt_to_gdp, color = group_label)
) +
  annotate(
    "rect",
    xmin = -0.5,
    xmax = 0.5,
    ymin = -Inf,
    ymax = Inf,
    fill = "#FDE9A9",
    alpha = 0.35
  ) +
  geom_vline(xintercept = -1, linetype = "dashed", color = "#6A3D9A") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(
    aes(label = percent(mean_debt_to_gdp, accuracy = 0.1)),
    vjust = -0.8,
    show.legend = FALSE,
    size = 3.8
  ) +
  scale_x_continuous(
    breaks = -2:0,
    labels = c(
      "-2" = "Pre-event year",
      "-1" = "Event year",
      "0" = "First full year after"
    )
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c(
      "Retained-bank controls" = "#B23A48",
      "Same-year event states" = "#2C699A"
    )
  ) +
  labs(
    title = "Debt trajectory through the first year after the event",
    subtitle = paste0(
      "Cohort-weighted averages; dashed line marks the event year. ",
      "The yellow area is the first full post-event year."
    ),
    x = NULL,
    y = "Average financial debt / GDP",
    color = "State group",
    caption = paste0(
      "One-year descriptive DiD (k = -1 to k = 0): ",
      sprintf("%+.2f", first_year_descriptive_did_pp),
      " pp. The event year is transitional; the main causal estimates use ",
      "Sun-Abraham."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

ggsave(
  file.path(figure_dir, "07_trajectory_through_first_post_event_year.png"),
  first_year_plot,
  width = 9,
  height = 6.5,
  dpi = 300
)

cohort_did_annotations <- same_year_did_results |>
  transmute(
    cohort_event_year = event_year,
    annotation = paste0(
      "Immediate 2x2 DiD: ",
      sprintf("%+.2f", estimate_percentage_points),
      " pp\np = ",
      sprintf("%.3f", p_value)
    )
  )

extended_cohort_plot <- ggplot(
  same_year_extended_trajectory_data,
  aes(relative_year, mean_debt_to_gdp, color = group_label)
) +
  geom_vline(xintercept = -1, linetype = "dashed", color = "#6A3D9A") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  geom_text(
    data = cohort_did_annotations,
    aes(x = Inf, y = Inf, label = annotation),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = 1.25,
    size = 3.2,
    color = "#333333"
  ) +
  facet_wrap(
    vars(cohort_event_year),
    scales = "free_y",
    labeller = labeller(
      cohort_event_year = function(x) paste("Event in", x)
    )
  ) +
  scale_x_continuous(
    breaks = seq(
      -2,
      max(same_year_extended_trajectory_data$relative_year),
      by = 2
    )
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c(
      "Retained-bank controls" = "#B23A48",
      "Same-year event states" = "#2C699A"
    )
  ) +
  labs(
    title = "Extended debt trajectories by common event-year cohort",
    subtitle = paste0(
      "The dashed line marks the institutional event year; ",
      "the trajectory continues through 2012"
    ),
    x = "Years relative to the first full treatment year",
    y = "Average financial debt / GDP",
    color = "State group",
    caption = paste0(
      "The displayed 2x2 DiD uses k = -2 and -1 as the pre window and ",
      "k = 0 and 1 as the immediate post window."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  file.path(figure_dir, "06_extended_trajectories_by_event_cohort.png"),
  extended_cohort_plot,
  width = 12,
  height = 9,
  dpi = 300
)
