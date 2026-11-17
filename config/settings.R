# Shared analysis parameters. Run scripts from the project root.
cfg <- list(
  start_year = 1996L,
  end_year = 2012L,
  refresh_downloads = FALSE,
  show_plots = interactive(),
  seed = 123L,
  treatment_lag = 1L,
  reference_period = -1L,
  plot_window = c(-5L, 8L),
  exclude_states = character(),
  treatment_types = c("direct_sale", "federalization", "liquidation"),
  outcomes = c("debt_to_gdp", "union_debt_to_gdp", "log_debt_to_gdp"),
  extension_exclude_states = "DF",
  synthetic_state = "PB",
  classic_state = "BA",
  classic_controls = c("ES", "PA", "RS", "SE"),
  classic_end_year = 2004L,
  comparison_models = c("twfe", "sun_abraham"),
  comparison_outcomes = "debt_to_gdp",
  comparison_allow_different_samples = FALSE,
  run_extensions = FALSE
)
