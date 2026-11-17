# Estimate binned immediate, medium and longer relative-time effects.
source("R/setup.R", local = TRUE)

source("R/extension_data.R", local = TRUE)
table_dir <- "outputs/robustness/treatment_horizons/tables"
figure_dir <- "outputs/robustness/treatment_horizons/figures"
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
horizon_model <- feols(
  debt_to_gdp ~ sunab(
    treatment_cohort,
    year,
    ref.p = -1,
    bin.rel = list(
      "0 to 1" = 0:1,
      "2 to 3" = 2:3,
      "4 plus" = 4:20
    )
  ) | state_code + year,
  data = causal_sample,
  cluster = ~ state_code
)

horizon_matrix <- aggregate(
  horizon_model,
  agg = "year::(.+):cohort"
)

horizon_results <- tibble(
  event_window = rownames(horizon_matrix),
  estimate = horizon_matrix[, "Estimate"],
  standard_error = horizon_matrix[, "Std. Error"],
  statistic = horizon_matrix[, "t value"],
  p_value = horizon_matrix[, "Pr(>|t|)"]
) |>
  filter(event_window %in% c("0 to 1", "2 to 3", "4 plus")) |>
  mutate(
    event_window = factor(
      event_window,
      levels = c("0 to 1", "2 to 3", "4 plus"),
      labels = c(
        "Immediate: k = 0 to 1",
        "Medium run: k = 2 to 3",
        "Long run: k >= 4"
      )
    ),
    critical_value = qt(
      0.975,
      df = n_distinct(causal_sample$state_code) - 1
    ),
    confidence_low = estimate - critical_value * standard_error,
    confidence_high = estimate + critical_value * standard_error,
    estimate_percentage_points = 100 * estimate,
    confidence_low_percentage_points = 100 * confidence_low,
    confidence_high_percentage_points = 100 * confidence_high
  ) |>
  select(-critical_value)

write_csv(
  horizon_results,
  file.path(table_dir, "sun_abraham_treatment_horizons.csv")
)

horizon_plot <- ggplot(
  horizon_results,
  aes(estimate_percentage_points, event_window)
) +
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
    title = "Treatment effects at different post-event horizons",
    subtitle = "Sun-Abraham estimates; DF excluded; event time -1 is the reference",
    x = "Estimated effect on financial debt / GDP (percentage points)",
    y = NULL,
    caption = paste0(
      "k = 0 is the first full calendar year after the bank event. ",
      "Confidence intervals use state-clustered standard errors."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(figure_dir, "01_immediate_medium_long_run_effects.png"),
  horizon_plot,
  width = 9,
  height = 5,
  dpi = 300
)
