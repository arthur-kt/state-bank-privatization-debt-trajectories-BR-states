# Estimate the event model for log debt and annual debt changes.
source("R/setup.R", local = TRUE)

source("R/extension_data.R", local = TRUE)
table_dir <- "outputs/robustness/alternative_outcomes/tables"
figure_dir <- "outputs/robustness/alternative_outcomes/figures"
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
model_debt_ratio <- feols(
  debt_to_gdp ~ sunab(treatment_cohort, year, ref.p = -1) |
    state_code + year,
  data = causal_sample,
  cluster = ~ state_code
)

model_log_nominal_debt <- feols(
  log_financial_debt ~ sunab(treatment_cohort, year, ref.p = -1) |
    state_code + year,
  data = causal_sample,
  cluster = ~ state_code
)

model_annual_log_debt_change <- feols(
  annual_log_debt_change_percent ~
    sunab(treatment_cohort, year, ref.p = -1) |
    state_code + year,
  data = causal_sample,
  cluster = ~ state_code
)

model_annual_change_debt_ratio <- feols(
  annual_change_debt_to_gdp_pp ~
    sunab(treatment_cohort, year, ref.p = -1) |
    state_code + year,
  data = causal_sample,
  cluster = ~ state_code
)

alternative_outcome_results <- bind_rows(
  tidy(summary(model_debt_ratio, agg = "ATT"), conf.int = TRUE) |>
    mutate(
      outcome = "Financial debt / GDP",
      scale = "Percentage points",
      display_multiplier = 100,
      observations = nobs(model_debt_ratio)
    ),
  tidy(summary(model_log_nominal_debt, agg = "ATT"), conf.int = TRUE) |>
    mutate(
      outcome = "Log nominal financial debt",
      scale = "Approximate percent",
      display_multiplier = 100,
      observations = nobs(model_log_nominal_debt)
    ),
  tidy(summary(model_annual_log_debt_change, agg = "ATT"), conf.int = TRUE) |>
    mutate(
      outcome = "Annual log change in nominal debt",
      scale = "Percentage points of annual growth",
      display_multiplier = 1,
      observations = nobs(model_annual_log_debt_change)
    ),
  tidy(summary(model_annual_change_debt_ratio, agg = "ATT"), conf.int = TRUE) |>
    mutate(
      outcome = "Annual change in debt / GDP",
      scale = "Percentage points of annual change",
      display_multiplier = 1,
      observations = nobs(model_annual_change_debt_ratio)
    )
) |>
  transmute(
    estimator = "Sun-Abraham ATT",
    outcome,
    scale,
    observations,
    estimate,
    standard_error = std.error,
    statistic,
    p_value = p.value,
    confidence_low = conf.low,
    confidence_high = conf.high,
    displayed_estimate = display_multiplier * estimate,
    displayed_confidence_low = display_multiplier * confidence_low,
    displayed_confidence_high = display_multiplier * confidence_high
  )

write_csv(
  alternative_outcome_results,
  file.path(table_dir, "sun_abraham_alternative_outcomes.csv")
)

alternative_outcome_plot <- alternative_outcome_results |>
  mutate(
    outcome = factor(
      outcome,
      levels = rev(c(
        "Financial debt / GDP",
        "Log nominal financial debt",
        "Annual log change in nominal debt",
        "Annual change in debt / GDP"
      ))
    )
  ) |>
  ggplot(aes(displayed_estimate, outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
  geom_errorbar(
    aes(
      xmin = displayed_confidence_low,
      xmax = displayed_confidence_high
    ),
    orientation = "y",
    width = 0.18,
    color = "#2C699A"
  ) +
  geom_point(size = 3, color = "#2C699A") +
  facet_grid(
    rows = vars(scale),
    scales = "free",
    space = "free_y"
  ) +
  labs(
    title = "Sun-Abraham estimates with alternative debt outcomes",
    subtitle = "Comparable bank-event sample; DF excluded",
    x = "Estimated effect in the scale shown above each panel",
    y = NULL,
    caption = paste0(
      "Nominal-debt outcomes retain state and year fixed effects. ",
      "Log debt effects are approximate percentage changes."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  file.path(figure_dir, "04_alternative_debt_outcomes.png"),
  alternative_outcome_plot,
  width = 10,
  height = 7.5,
  dpi = 300
)
