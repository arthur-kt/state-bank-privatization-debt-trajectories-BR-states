# Create distributions, correlations and state/group trajectories.
source("R/setup.R", local = TRUE)

input_file <- "data/processed/state_panel.csv"
table_dir <- "outputs/eda/tables"
figure_dir <- "outputs/eda/figures"
state_figure_dir <- file.path(figure_dir, "states")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(state_figure_dir, recursive = TRUE, showWarnings = FALSE)

show_plots_in_rstudio <- cfg$show_plots

if (!file.exists(input_file)) {
  stop("The model table was not found. Run scripts/01_prepare_data.R first.")
}

panel <- read_csv(input_file, show_col_types = FALSE) %>%
  mutate(
    bank_group = factor(
      bank_group,
      levels = c(
        "Retained state-owned commercial bank",
        "All other states"
      )
    )
  )

analysis_variables <- c(
  "financial_debt_brl",
  "union_debt_brl",
  "state_gdp_nominal_brl",
  "debt_to_gdp",
  "union_debt_to_gdp",
  "gdp_share_percent"
)

variable_labels <- c(
  financial_debt_brl = "Financial debt (BRL)",
  union_debt_brl = "Debt with the federal government (BRL)",
  state_gdp_nominal_brl = "State GDP (BRL)",
  debt_to_gdp = "Financial debt / GDP",
  union_debt_to_gdp = "Federal government debt / GDP",
  gdp_share_percent = "Share of Brazilian GDP (%)"
)

# 2. Basic data checks and panel coverage

if (anyDuplicated(panel[c("state_code", "year")]) > 0) {
  stop("Duplicate state-year observations were found.")
}

missing_values <- panel %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_n") %>%
  mutate(missing_percent = 100 * missing_n / nrow(panel))

panel_coverage <- panel %>%
  group_by(state_code, state_name, bank_group) %>%
  summarise(
    first_year = min(year),
    last_year = max(year),
    observations = n(),
    .groups = "drop"
  )

write_csv(missing_values, file.path(table_dir, "missing_values.csv"))
write_csv(panel_coverage, file.path(table_dir, "panel_coverage.csv"))

# 3. Descriptive statistics

numeric_long <- panel %>%
  select(state_code, state_name, year, bank_group, all_of(analysis_variables)) %>%
  pivot_longer(
    cols = all_of(analysis_variables),
    names_to = "variable",
    values_to = "value"
  )

descriptive_overall <- numeric_long %>%
  group_by(variable) %>%
  summarise(
    observations = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    standard_deviation = sd(value, na.rm = TRUE),
    minimum = min(value, na.rm = TRUE),
    percentile_25 = quantile(value, 0.25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    percentile_75 = quantile(value, 0.75, na.rm = TRUE),
    maximum = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label = unname(variable_labels[variable]), .after = variable)

descriptive_by_group <- numeric_long %>%
  group_by(bank_group, variable) %>%
  summarise(
    observations = sum(!is.na(value)),
    states = n_distinct(state_code[!is.na(value)]),
    mean = mean(value, na.rm = TRUE),
    standard_deviation = sd(value, na.rm = TRUE),
    minimum = min(value, na.rm = TRUE),
    percentile_25 = quantile(value, 0.25, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    percentile_75 = quantile(value, 0.75, na.rm = TRUE),
    maximum = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label = unname(variable_labels[variable]), .after = variable)

write_csv(
  descriptive_overall,
  file.path(table_dir, "descriptive_statistics_overall.csv")
)
write_csv(
  descriptive_by_group,
  file.path(table_dir, "descriptive_statistics_by_group.csv")
)

# 4. Correlations

correlation_matrix <- panel %>%
  select(all_of(analysis_variables)) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")

correlation_table <- as.data.frame(correlation_matrix) %>%
  rownames_to_column("variable")

write_csv(correlation_table, file.path(table_dir, "correlation_matrix.csv"))

correlation_long <- correlation_table %>%
  pivot_longer(-variable, names_to = "variable_2", values_to = "correlation") %>%
  mutate(
    variable = factor(variable, levels = analysis_variables),
    variable_2 = factor(variable_2, levels = rev(analysis_variables))
  )

correlation_plot <- ggplot(
  correlation_long,
  aes(x = variable, y = variable_2, fill = correlation)
) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.2f", correlation)), size = 3) +
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, limits = c(-1, 1)
  ) +
  scale_x_discrete(labels = variable_labels) +
  scale_y_discrete(labels = variable_labels) +
  labs(
    title = "Correlation matrix",
    x = NULL,
    y = NULL,
    fill = "Correlation"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    panel.grid = element_blank()
  )

ggsave(
  file.path(figure_dir, "01_correlation_matrix.png"),
  correlation_plot,
  width = 10,
  height = 7,
  dpi = 300
)

# 5. Original versus logarithmic distributions

distribution_long <- numeric_long %>%
  filter(!is.na(value), value > 0) %>%
  transmute(
    variable,
    label = unname(variable_labels[variable]),
    original = value,
    logarithm = log(value)
  ) %>%
  pivot_longer(
    cols = c(original, logarithm),
    names_to = "scale",
    values_to = "value"
  ) %>%
  mutate(
    scale = recode(scale, original = "Original", logarithm = "Natural log")
  )

transformation_diagnostics <- distribution_long %>%
  group_by(variable, label, scale) %>%
  summarise(
    observations = n(),
    mean = mean(value),
    standard_deviation = sd(value),
    skewness = mean((value - mean(value))^3) / sd(value)^3,
    excess_kurtosis = mean((value - mean(value))^4) / sd(value)^4 - 3,
    shapiro_wilk_p_value = shapiro.test(value)$p.value,
    normality_score = abs(skewness) + abs(excess_kurtosis),
    .groups = "drop"
  ) %>%
  select(
    variable, label, scale, observations, mean, standard_deviation,
    skewness, excess_kurtosis, shapiro_wilk_p_value, normality_score
  )

transformation_choice <- transformation_diagnostics %>%
  select(variable, label, scale, normality_score) %>%
  pivot_wider(names_from = scale, values_from = normality_score) %>%
  mutate(
    preferred_scale = if_else(
      `Natural log` < Original,
      "Natural log",
      "Original"
    ),
    score_improvement_percent = 100 *
      (Original - `Natural log`) / Original
  )

transformation_diagnostics <- transformation_diagnostics %>%
  left_join(
    transformation_choice %>%
      select(variable, preferred_scale, score_improvement_percent),
    by = "variable"
  )

write_csv(
  transformation_diagnostics,
  file.path(table_dir, "transformation_diagnostics.csv")
)

distribution_plot_data <- distribution_long %>%
  group_by(variable, scale) %>%
  mutate(standardized_value = (value - mean(value)) / sd(value)) %>%
  ungroup()

distribution_plot <- ggplot(
  distribution_plot_data,
  aes(x = standardized_value)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 25,
    fill = "#4C78A8",
    color = "white"
  ) +
  geom_density(color = "#B2182B", linewidth = 0.7) +
  facet_grid(label ~ scale, scales = "free") +
  labs(
    title = "Distributions before and after logarithmic transformation",
    x = "Standardized value within each panel",
    y = "Density"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text.y = element_text(angle = 0))

ggsave(
  file.path(figure_dir, "02_distributions_original_vs_log.png"),
  distribution_plot,
  width = 11,
  height = 13,
  dpi = 300
)

qq_plot <- ggplot(distribution_plot_data, aes(sample = standardized_value)) +
  stat_qq(alpha = 0.45, size = 0.7, color = "#4C78A8") +
  stat_qq_line(color = "#B2182B", linewidth = 0.6) +
  facet_grid(label ~ scale, scales = "free") +
  labs(
    title = "Normal Q-Q plots before and after logarithmic transformation",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text.y = element_text(angle = 0))

ggsave(
  file.path(figure_dir, "03_qq_plots_original_vs_log.png"),
  qq_plot,
  width = 11,
  height = 13,
  dpi = 300
)

# 6. Comparisons between retained-bank states and all other states

group_year_summary <- panel %>%
  group_by(year, bank_group) %>%
  summarise(
    states = n_distinct(state_code),
    mean_debt_to_gdp = mean(debt_to_gdp),
    sd_debt_to_gdp = sd(debt_to_gdp),
    mean_union_debt_to_gdp = mean(union_debt_to_gdp),
    sd_union_debt_to_gdp = sd(union_debt_to_gdp),
    .groups = "drop"
  ) %>%
  mutate(
    debt_to_gdp_se = sd_debt_to_gdp / sqrt(states),
    union_debt_to_gdp_se = sd_union_debt_to_gdp / sqrt(states)
  )

write_csv(group_year_summary, file.path(table_dir, "annual_group_statistics.csv"))

debt_group_plot <- ggplot(
  group_year_summary,
  aes(x = year, y = mean_debt_to_gdp, color = bank_group, fill = bank_group)
) +
  geom_ribbon(
    aes(
      ymin = mean_debt_to_gdp - 1.96 * debt_to_gdp_se,
      ymax = mean_debt_to_gdp + 1.96 * debt_to_gdp_se
    ),
    alpha = 0.12,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(min(panel$year), max(panel$year), by = 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02")) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02")) +
  labs(
    title = "Average financial debt relative to GDP",
    subtitle = "State averages with approximate 95% confidence bands",
    x = "Year",
    y = "Financial debt / GDP",
    color = "Bank group",
    fill = "Bank group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(margin = margin(t = 12)),
    legend.margin = margin(t = 10),
    plot.margin = margin(10, 10, 18, 10)
  )

ggsave(
  file.path(figure_dir, "04_average_debt_to_gdp_by_group.png"),
  debt_group_plot,
  width = 10,
  height = 6,
  dpi = 300
)

union_group_plot <- ggplot(
  group_year_summary,
  aes(
    x = year,
    y = mean_union_debt_to_gdp,
    color = bank_group,
    fill = bank_group
  )
) +
  geom_ribbon(
    aes(
      ymin = mean_union_debt_to_gdp - 1.96 * union_debt_to_gdp_se,
      ymax = mean_union_debt_to_gdp + 1.96 * union_debt_to_gdp_se
    ),
    alpha = 0.12,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = seq(min(panel$year), max(panel$year), by = 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02")) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02")) +
  labs(
    title = "Average federal government debt relative to GDP",
    subtitle = "State averages with approximate 95% confidence bands",
    x = "Year",
    y = "Federal government debt / GDP",
    color = "Bank group",
    fill = "Bank group"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "05_average_union_debt_to_gdp_by_group.png"),
  union_group_plot,
  width = 10,
  height = 6,
  dpi = 300
)

state_trajectory_plot <- ggplot(
  panel,
  aes(x = year, y = debt_to_gdp, group = state_code, color = bank_group)
) +
  geom_line(alpha = 0.28, linewidth = 0.55) +
  stat_summary(
    aes(group = bank_group),
    fun = mean,
    geom = "line",
    linewidth = 1.25
  ) +
  scale_x_continuous(breaks = seq(min(panel$year), max(panel$year), by = 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02")) +
  labs(
    title = "State trajectories and group averages",
    subtitle = "Thin lines represent states; thick lines represent group means",
    x = "Year",
    y = "Financial debt / GDP",
    color = "Bank group"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "06_state_trajectories_debt_to_gdp.png"),
  state_trajectory_plot,
  width = 10,
  height = 6,
  dpi = 300
)

boxplot_data <- panel %>%
  select(bank_group, debt_to_gdp, union_debt_to_gdp) %>%
  pivot_longer(
    cols = c(debt_to_gdp, union_debt_to_gdp),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = recode(
      variable,
      debt_to_gdp = "Financial debt / GDP",
      union_debt_to_gdp = "Federal government debt / GDP"
    )
  )

boxplot_figure <- ggplot(boxplot_data, aes(x = bank_group, y = value, fill = bank_group)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.35) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02")) +
  labs(
    title = "Debt distributions by bank group",
    x = NULL,
    y = "Ratio to GDP",
    fill = "Bank group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    legend.position = "none"
  )

ggsave(
  file.path(figure_dir, "07_debt_boxplots_by_group.png"),
  boxplot_figure,
  width = 10,
  height = 5.5,
  dpi = 300
)

# 7. Descriptive event-time profile

event_time_summary <- panel %>%
  filter(event_study_eligible == 1, between(event_time, -5, 8)) %>%
  group_by(event_time) %>%
  summarise(
    states = n_distinct(state_code),
    mean_debt_to_gdp = mean(debt_to_gdp),
    standard_deviation = sd(debt_to_gdp),
    standard_error = standard_deviation / sqrt(states),
    .groups = "drop"
  )

write_csv(event_time_summary, file.path(table_dir, "event_time_statistics.csv"))

event_time_plot <- ggplot(
  event_time_summary,
  aes(x = event_time, y = mean_debt_to_gdp)
) +
  geom_ribbon(
    aes(
      ymin = mean_debt_to_gdp - 1.96 * standard_error,
      ymax = mean_debt_to_gdp + 1.96 * standard_error
    ),
    fill = "#4C78A8",
    alpha = 0.15
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#B2182B") +
  geom_line(color = "#4C78A8", linewidth = 1) +
  geom_point(color = "#4C78A8", size = 2) +
  scale_x_continuous(breaks = -5:8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Descriptive financial debt profile around loss of bank control",
    subtitle = "Event time zero is the first full treatment year",
    x = "Years relative to the first full treatment year",
    y = "Average financial debt / GDP"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  file.path(figure_dir, "08_descriptive_event_time_profile.png"),
  event_time_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# 8. Individual state trajectories with bank event markers

primary_event_markers <- panel %>%
  distinct(state_code, main_bank, event_type, event_year) %>%
  filter(!is.na(event_year)) %>%
  transmute(
    state_code,
    bank_name = main_bank,
    event_type,
    event_year,
    marker_role = "Primary treatment event"
  )

additional_event_markers <- tribble(
  ~state_code, ~bank_name, ~event_type, ~event_year, ~marker_role,
  "SP", "BANESPA", "auction_sale_to_santander", 2000L,
  "Additional institutional event",
  "SP", "Nossa Caixa", "transfer_to_banco_do_brasil", 2009L,
  "Additional institutional event"
)

event_markers <- bind_rows(primary_event_markers, additional_event_markers) %>%
  mutate(
    event_description = event_type %>%
      str_replace_all("_", " ") %>%
      str_to_sentence(),
    event_label = paste0(bank_name, ": ", event_description, " (", event_year, ")")
  ) %>%
  arrange(state_code, event_year)

write_csv(
  event_markers,
  file.path(table_dir, "state_event_markers_for_plots.csv")
)

state_trajectory_data <- panel %>%
  select(
    state_code, state_name, year, bank_group,
    debt_to_gdp
  ) %>%
  arrange(state_code, year)

for (code in unique(panel$state_code)) {
  state_data <- state_trajectory_data %>% filter(state_code == code)
  state_events <- event_markers %>% filter(state_code == code)
  state_title <- paste0(first(state_data$state_name), " (", code, ")")

  if (nrow(state_events) == 0) {
    state_subtitle <- "No comparable loss-of-control event in the analysis period"
  } else {
    state_subtitle <- paste(state_events$event_label, collapse = " | ")
  }

  state_plot <- ggplot(
    state_data,
    aes(x = year, y = debt_to_gdp)
  ) +
    geom_line(linewidth = 1, color = "#D62728") +
    geom_point(size = 2, color = "#D62728") +
    geom_vline(
      data = state_events,
      aes(xintercept = event_year),
      inherit.aes = FALSE,
      color = "#6A3D9A",
      linetype = "dashed",
      linewidth = 0.8
    ) +
    geom_text(
      data = state_events,
      aes(
        x = event_year,
        y = Inf,
        label = paste0(bank_name, " (", event_year, ")")
      ),
      inherit.aes = FALSE,
      angle = 90,
      hjust = 1.05,
      vjust = -0.35,
      size = 3.1,
      color = "#6A3D9A"
    ) +
    scale_x_continuous(
      breaks = seq(min(panel$year), max(panel$year), by = 2),
      limits = range(panel$year)
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0.05, 0.18))
    ) +
    labs(
      title = state_title,
      subtitle = state_subtitle,
      x = "Year",
      y = "Financial debt / GDP"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.subtitle = element_text(size = 9),
      plot.margin = margin(8, 20, 8, 8)
    ) +
    coord_cartesian(clip = "off")

  ggsave(
    file.path(state_figure_dir, paste0(code, "_debt_trajectory.png")),
    state_plot,
    width = 10,
    height = 6,
    dpi = 300
  )

  if (show_plots_in_rstudio) {
    print(state_plot)
  }
}

# 9. State-level scatter plots

state_average_statistics <- panel %>%
  arrange(state_code, year) %>%
  group_by(state_code, state_name, bank_group, retained_bank) %>%
  summarise(
    average_financial_debt_brl = mean(financial_debt_brl),
    average_union_debt_brl = mean(union_debt_brl),
    average_state_gdp_brl = mean(state_gdp_nominal_brl),
    average_debt_to_gdp = mean(debt_to_gdp),
    average_union_debt_to_gdp = mean(union_debt_to_gdp),
    average_gdp_share_percent = mean(gdp_share_percent),
    change_debt_to_gdp = last(debt_to_gdp) - first(debt_to_gdp),
    .groups = "drop"
  )

write_csv(
  state_average_statistics,
  file.path(table_dir, "state_average_statistics.csv")
)

scatter_colors <- c(
  "Retained state-owned commercial bank" = "#1B9E77",
  "All other states" = "#D95F02"
)

debt_scatter <- ggplot(
  state_average_statistics,
  aes(
    x = average_debt_to_gdp,
    y = average_union_debt_to_gdp,
    color = bank_group
  )
) +
  geom_point(size = 3, alpha = 0.85) +
  geom_text(
    aes(label = state_code),
    nudge_y = 0.006,
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = scatter_colors) +
  labs(
    title = "Average financial debt and federal government debt",
    subtitle = "Each point is a state; averages cover 1996-2012",
    x = "Average financial debt / GDP",
    y = "Average federal government debt / GDP",
    color = "Bank group"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "09_state_average_debt_scatter.png"),
  debt_scatter,
  width = 10,
  height = 7,
  dpi = 300
)

gdp_debt_scatter <- ggplot(
  state_average_statistics,
  aes(
    x = average_state_gdp_brl,
    y = average_debt_to_gdp,
    color = bank_group
  )
) +
  geom_point(size = 3, alpha = 0.85) +
  geom_text(
    aes(label = state_code),
    nudge_y = 0.006,
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_x_log10(
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = scatter_colors) +
  labs(
    title = "Average state GDP and financial debt burden",
    subtitle = "GDP uses a logarithmic axis; each point is a state",
    x = "Average state GDP (BRL, log scale)",
    y = "Average financial debt / GDP",
    color = "Bank group"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "10_state_gdp_and_debt_burden_scatter.png"),
  gdp_debt_scatter,
  width = 10,
  height = 7,
  dpi = 300
)

level_change_scatter <- ggplot(
  state_average_statistics,
  aes(
    x = average_debt_to_gdp,
    y = change_debt_to_gdp,
    color = bank_group
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
  geom_point(size = 3, alpha = 0.85) +
  geom_text(
    aes(label = state_code),
    nudge_y = 0.008,
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = scatter_colors) +
  labs(
    title = "Average debt burden and change from 1996 to 2012",
    subtitle = "Negative values on the vertical axis indicate a decline",
    x = "Average financial debt / GDP",
    y = "Change in financial debt / GDP",
    color = "Bank group"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "11_state_debt_level_and_change_scatter.png"),
  level_change_scatter,
  width = 10,
  height = 7,
  dpi = 300
)

# 10. Nominal GDP and nominal financial debt

nominal_scatter <- ggplot(
  state_average_statistics,
  aes(
    x = average_state_gdp_brl,
    y = average_financial_debt_brl,
    color = bank_group
  )
) +
  geom_point(size = 3, alpha = 0.85) +
  geom_text(
    aes(label = state_code),
    nudge_y = 0.08,
    size = 3,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_x_log10(
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  scale_y_log10(
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  scale_color_manual(values = scatter_colors) +
  labs(
    title = "Average nominal state GDP and financial debt",
    subtitle = "Each point is a state; both axes use logarithmic scales",
    x = "Average nominal state GDP (BRL)",
    y = "Average nominal financial debt (BRL)",
    color = "Bank group"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "12_state_nominal_gdp_and_debt_scatter.png"),
  nominal_scatter,
  width = 10,
  height = 7,
  dpi = 300
)

nominal_group_year <- panel %>%
  group_by(year, bank_group) %>%
  summarise(
    average_financial_debt_brl = mean(financial_debt_brl),
    average_state_gdp_brl = mean(state_gdp_nominal_brl),
    .groups = "drop"
  )

write_csv(
  nominal_group_year,
  file.path(table_dir, "annual_nominal_group_statistics.csv")
)

nominal_group_year_long <- nominal_group_year %>%
  pivot_longer(
    cols = c(average_financial_debt_brl, average_state_gdp_brl),
    names_to = "variable",
    values_to = "value_brl"
  ) %>%
  mutate(
    variable = recode(
      variable,
      average_financial_debt_brl = "Average nominal financial debt",
      average_state_gdp_brl = "Average nominal state GDP"
    )
  )

nominal_evolution_plot <- ggplot(
  nominal_group_year_long,
  aes(x = year, y = value_brl, color = bank_group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = seq(min(panel$year), max(panel$year), by = 2)) +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  scale_color_manual(values = scatter_colors) +
  labs(
    title = "Evolution of nominal GDP and financial debt",
    subtitle = "Annual unweighted state averages by bank group",
    x = "Year",
    y = "BRL",
    color = "Bank group"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "13_nominal_gdp_and_debt_evolution.png"),
  nominal_evolution_plot,
  width = 10,
  height = 8,
  dpi = 300
)

nominal_group_index <- nominal_group_year_long %>%
  group_by(bank_group, variable) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(index_1996 = 100 * value_brl / first(value_brl)) %>%
  ungroup()

write_csv(
  nominal_group_index,
  file.path(table_dir, "annual_nominal_group_index_1996_100.csv")
)

nominal_index_plot <- ggplot(
  nominal_group_index,
  aes(x = year, y = index_1996, color = variable)
) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "grey50") +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  facet_wrap(~ bank_group, ncol = 1) +
  scale_x_continuous(breaks = seq(min(panel$year), max(panel$year), by = 2)) +
  scale_color_manual(values = c("#D62728", "#1F78B4")) +
  labs(
    title = "Nominal GDP and financial debt growth",
    subtitle = "Each series equals 100 in 1996",
    x = "Year",
    y = "Index (1996 = 100)",
    color = "Variable"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(margin = margin(t = 12)),
    legend.margin = margin(t = 10),
    plot.margin = margin(10, 10, 18, 10)
  )

ggsave(
  file.path(figure_dir, "14_nominal_gdp_and_debt_index_1996_100.png"),
  nominal_index_plot,
  width = 10,
  height = 9,
  dpi = 300
)

# 11. Display figures in the RStudio Plots pane

if (show_plots_in_rstudio) {
  print(correlation_plot)
  print(distribution_plot)
  print(qq_plot)
  print(debt_group_plot)
  print(union_group_plot)
  print(state_trajectory_plot)
  print(boxplot_figure)
  print(event_time_plot)
  print(debt_scatter)
  print(gdp_debt_scatter)
  print(level_change_scatter)
  print(nominal_scatter)
  print(nominal_evolution_plot)
  print(nominal_index_plot)
}

