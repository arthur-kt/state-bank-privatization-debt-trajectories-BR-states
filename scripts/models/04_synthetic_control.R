# Fit a synthetic control using pre-event outcomes and retained-bank donors.
source("R/setup.R", local = TRUE)

library(Synth)
source("R/extension_data.R", local = TRUE)
table_dir <- "outputs/models/synthetic_control/tables"
figure_dir <- "outputs/models/synthetic_control/figures"
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
synthetic_state <- cfg$synthetic_state
synthetic_event_year <- unique(causal_sample$event_year[causal_sample$state_code == synthetic_state])
synthetic_first_full_year <- synthetic_event_year + cfg$treatment_lag
synthetic_pre_years <- cfg$start_year:(synthetic_event_year - 1L)
synthetic_plot_years <- cfg$start_year:cfg$end_year

synthetic_data <- causal_sample |>
  filter(state_code %in% c(synthetic_state, control_states)) |>
  mutate(
    state_id = as.integer(factor(state_code)),
    state_label = paste0(state_code, " - ", state_name)
  )

treated_identifier <- synthetic_data |>
  filter(state_code == synthetic_state) |>
  distinct(state_id) |>
  pull(state_id)
control_identifiers <- synthetic_data |>
  filter(state_code %in% control_states) |>
  distinct(state_id) |>
  pull(state_id)

special_predictors <- lapply(
  synthetic_pre_years,
  function(selected_year) {
    list("debt_to_gdp", selected_year, "mean")
  }
)

synthetic_setup <- dataprep(
  foo = as.data.frame(synthetic_data),
  predictors = "debt_to_gdp",
  predictors.op = "mean",
  special.predictors = special_predictors,
  dependent = "debt_to_gdp",
  unit.variable = "state_id",
  unit.names.variable = "state_label",
  time.variable = "year",
  treatment.identifier = treated_identifier,
  controls.identifier = control_identifiers,
  time.predictors.prior = synthetic_pre_years,
  time.optimize.ssr = synthetic_pre_years,
  time.plot = synthetic_plot_years
)

synthetic_fit <- synth(
  data.prep.obj = synthetic_setup,
  method = "Nelder-Mead",
  verbose = FALSE
)

synthetic_path <- tibble(
  year = synthetic_plot_years,
  treated_debt_to_gdp = as.vector(synthetic_setup$Y1plot),
  synthetic_debt_to_gdp = as.vector(
    synthetic_setup$Y0plot %*% synthetic_fit$solution.w
  )
) |>
  mutate(
    gap = treated_debt_to_gdp - synthetic_debt_to_gdp,
    event_time = year - synthetic_first_full_year
  )

synthetic_weights <- tibble(
  donor_state = rownames(synthetic_fit$solution.w),
  weight = as.vector(synthetic_fit$solution.w)
) |>
  left_join(
    synthetic_data |>
      distinct(state_id, state_code, state_name) |>
      mutate(state_id = as.character(state_id)),
    by = c("donor_state" = "state_id")
  ) |>
  arrange(desc(weight))

synthetic_summary <- synthetic_path |>
  summarise(
    treated_state = synthetic_state,
    event_year = synthetic_event_year,
    first_full_treatment_year = synthetic_first_full_year,
    pre_years = paste(range(synthetic_pre_years), collapse = "-"),
    donor_states = paste(control_states, collapse = ", "),
    pre_rmspe = sqrt(mean(gap[year %in% synthetic_pre_years]^2)),
    post_rmspe = sqrt(mean(gap[year >= synthetic_first_full_year]^2)),
    average_post_gap = mean(gap[year >= synthetic_first_full_year]),
    average_post_gap_percentage_points = 100 * average_post_gap
  ) |>
  mutate(post_pre_rmspe_ratio = post_rmspe / pre_rmspe)

write_csv(
  synthetic_path,
  file.path(table_dir, "synthetic_control_path.csv")
)
write_csv(
  synthetic_weights,
  file.path(table_dir, "synthetic_control_weights.csv")
)
write_csv(
  synthetic_summary,
  file.path(table_dir, "synthetic_control_summary.csv")
)

synthetic_path_long <- synthetic_path |>
  select(year, treated_debt_to_gdp, synthetic_debt_to_gdp) |>
  pivot_longer(
    -year,
    names_to = "series",
    values_to = "debt_to_gdp"
  ) |>
  mutate(
    series = recode(
      series,
      treated_debt_to_gdp = synthetic_state,
      synthetic_debt_to_gdp = paste("Synthetic", synthetic_state)
    )
  )

synthetic_path_plot <- ggplot(
  synthetic_path_long,
  aes(year, debt_to_gdp, color = series)
) +
  geom_vline(
    xintercept = synthetic_event_year,
    linetype = "dashed",
    color = "#6A3D9A"
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(
    values = setNames(c("#2C699A", "#B23A48"), c(synthetic_state, paste("Synthetic", synthetic_state)))
  ) +
  labs(
    title = paste(synthetic_state, "and its synthetic control"),
    subtitle = paste0(
      "Pre-event fit: ", paste(range(synthetic_pre_years), collapse = "-"),
      "; event year: ", synthetic_event_year
    ),
    x = "Year",
    y = "Financial debt / GDP",
    color = NULL,
    caption = paste0(
      "Donor pool: ", paste(control_states, collapse = ", ")
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(figure_dir, "02_synthetic_control_path.png"),
  synthetic_path_plot,
  width = 10,
  height = 6,
  dpi = 300
)

synthetic_gap_plot <- ggplot(synthetic_path, aes(year, 100 * gap)) +
  geom_hline(yintercept = 0, color = "grey45") +
  geom_vline(
    xintercept = synthetic_event_year,
    linetype = "dashed",
    color = "#6A3D9A"
  ) +
  geom_line(linewidth = 1.1, color = "#2C699A") +
  geom_point(size = 1.8, color = "#2C699A") +
  labs(
    title = paste("Observed minus synthetic:", synthetic_state),
    subtitle = "Positive values mean higher observed debt / GDP",
    x = "Year",
    y = "Treated minus synthetic (percentage points)"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(figure_dir, "03_synthetic_control_gap.png"),
  synthetic_gap_plot,
  width = 10,
  height = 5.5,
  dpi = 300
)

comparison_table <- tibble(
  method = paste("Synthetic control:", synthetic_state), outcome = "debt_to_gdp",
  sample_id = paste("synth", synthetic_state, paste(control_states, collapse = "-"),
                    cfg$start_year, cfg$end_year, synthetic_first_full_year, sep = "_"),
  estimate = synthetic_summary$average_post_gap, std.error = NA_real_,
  p.value = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
  estimate_display = 100 * estimate, conf_low_display = NA_real_, conf_high_display = NA_real_)
write_csv(comparison_table, "outputs/models/synthetic_control/estimates.csv")
saveRDS(list(setup = synthetic_setup, fitted = synthetic_fit),
        "outputs/models/synthetic_control/model.rds")
if (cfg$show_plots) { print(synthetic_path_plot); print(synthetic_gap_plot) }
