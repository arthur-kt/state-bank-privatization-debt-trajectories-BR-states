# Fit Synthetic DiD separately for supported common-event cohorts.
source("R/setup.R", local = TRUE)

library(synthdid)
source("R/extension_data.R", local = TRUE)
table_dir <- "outputs/models/synthetic_did/tables"
figure_dir <- "outputs/models/synthetic_did/figures"
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
synthdid_cohorts <- causal_sample |>
  filter(loss_of_control == 1) |>
  distinct(event_year, first_full_treatment_year, state_code) |>
  count(event_year, first_full_treatment_year, name = "treated_states") |>
  mutate(clean_pre_years = event_year - min(panel$year)) |>
  filter(clean_pre_years >= 3) |>
  arrange(event_year)

synthdid_results <- list()
synthdid_paths <- list()

for (cohort_index in seq_len(nrow(synthdid_cohorts))) {
  selected_event_year <- synthdid_cohorts$event_year[cohort_index]
  selected_treatment_year <- synthdid_cohorts$first_full_treatment_year[cohort_index]

  cohort_treated_codes <- causal_sample |>
    filter(
      loss_of_control == 1,
      event_year == selected_event_year
    ) |>
    distinct(state_code) |>
    pull(state_code)

  cohort_panel <- causal_sample |>
    filter(
      state_code %in% c(cohort_treated_codes, control_states),
      year != selected_event_year
    ) |>
    transmute(
      state_code,
      year,
      debt_to_gdp,
      treated = as.integer(
        state_code %in% cohort_treated_codes &
          year >= selected_treatment_year
      )
    )

  setup <- panel.matrices(
    as.data.frame(cohort_panel),
    unit = "state_code",
    time = "year",
    outcome = "debt_to_gdp",
    treatment = "treated"
  )

  estimate <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
  inference_method <- if_else(
    setup$N0 > (nrow(setup$Y) - setup$N0),
    "placebo",
    "jackknife"
  )
  standard_error <- sqrt(
    as.numeric(vcov(estimate, method = inference_method))
  )

  treated_mean <- colMeans(setup$Y[(setup$N0 + 1):nrow(setup$Y), , drop = FALSE])
  weights <- attr(estimate, "weights")$omega
  synthetic_mean <- as.vector(
    t(weights) %*% setup$Y[1:setup$N0, , drop = FALSE]
  )
  available_years <- sort(unique(cohort_panel$year))

  synthdid_paths[[cohort_index]] <- tibble(
    event_year = selected_event_year,
    first_full_treatment_year = selected_treatment_year,
    year = available_years,
    event_time = year - selected_treatment_year,
    treated_mean = treated_mean,
    synthetic_mean = synthetic_mean,
    gap = treated_mean - synthetic_mean
  )

  synthdid_results[[cohort_index]] <- tibble(
    event_year = selected_event_year,
    first_full_treatment_year = selected_treatment_year,
    treated_states = length(cohort_treated_codes),
    treated_state_codes = paste(sort(cohort_treated_codes), collapse = ", "),
    control_states = length(control_states),
    control_state_codes = paste(control_states, collapse = ", "),
    inference_method = inference_method,
    clean_pre_periods = setup$T0,
    post_periods = ncol(setup$Y) - setup$T0,
    estimate = as.numeric(estimate),
    estimate_percentage_points = 100 * as.numeric(estimate),
    standard_error = standard_error,
    standard_error_percentage_points = 100 * standard_error,
    confidence_low_percentage_points = 100 * (
      as.numeric(estimate) - 1.96 * standard_error
    ),
    confidence_high_percentage_points = 100 * (
      as.numeric(estimate) + 1.96 * standard_error
    ),
    p_value = 2 * pnorm(-abs(as.numeric(estimate) / standard_error))
  )
}

synthdid_results <- bind_rows(synthdid_results)
synthdid_paths <- bind_rows(synthdid_paths)

write_csv(
  synthdid_results,
  file.path(table_dir, "synthetic_did_cohort_results.csv")
)
write_csv(
  synthdid_paths,
  file.path(table_dir, "synthetic_did_cohort_paths.csv")
)

synthdid_result_plot <- synthdid_results |>
  mutate(
    cohort_label = paste0(
      "Event in ", event_year, " (", treated_state_codes, ")"
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
    title = "Synthetic DiD estimates by common event-year cohort",
    subtitle = paste0(
      "Event year omitted; cohorts require at least three clean pre-event years"
    ),
    x = "Estimated effect on debt / GDP (percentage points)",
    y = NULL,
    caption = paste0(
      "Placebo inference is used when controls outnumber treated units; ",
      "otherwise jackknife inference is used. Donors: ", paste(control_states, collapse = ", ")
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(figure_dir, "04_synthetic_did_cohort_estimates.png"),
  synthdid_result_plot,
  width = 10,
  height = 6,
  dpi = 300
)

synthdid_path_long <- synthdid_paths |>
  select(event_year, year, treated_mean, synthetic_mean) |>
  pivot_longer(
    c(treated_mean, synthetic_mean),
    names_to = "series",
    values_to = "debt_to_gdp"
  ) |>
  mutate(
    series = recode(
      series,
      treated_mean = "Event states",
      synthetic_mean = "Synthetic comparison"
    )
  )

synthdid_event_markers <- synthdid_results |>
  distinct(event_year)

synthdid_path_plot <- ggplot(
  synthdid_path_long,
  aes(year, debt_to_gdp, color = series)
) +
  geom_vline(
    data = synthdid_event_markers,
    aes(xintercept = event_year),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "#6A3D9A"
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(
    vars(event_year),
    scales = "free_y",
    labeller = labeller(event_year = function(x) paste("Event in", x))
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(
    values = c(
      "Event states" = "#2C699A",
      "Synthetic comparison" = "#B23A48"
    )
  ) +
  labs(
    title = "Observed and synthetic trajectories in the Synthetic DiD",
    subtitle = paste0(
      "Dashed line marks the institutional event year, which is omitted ",
      "from each cohort panel"
    ),
    x = "Year",
    y = "Financial debt / GDP",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  file.path(figure_dir, "05_synthetic_did_cohort_paths.png"),
  synthdid_path_plot,
  width = 11,
  height = 8,
  dpi = 300
)

comparison_table <- synthdid_results |>
  transmute(method = paste("Synthetic DiD: event", event_year), outcome = "debt_to_gdp",
            sample_id = paste("synthdid", event_year, treated_state_codes, control_state_codes,
                              cfg$start_year, cfg$end_year, sep = "_"),
            estimate, std.error = standard_error, p.value = p_value,
            conf.low = confidence_low_percentage_points / 100,
            conf.high = confidence_high_percentage_points / 100,
            estimate_display = estimate_percentage_points,
            conf_low_display = confidence_low_percentage_points,
            conf_high_display = confidence_high_percentage_points)
write_csv(comparison_table, "outputs/models/synthetic_did/estimates.csv")
if (cfg$show_plots) { print(synthdid_result_plot); print(synthdid_path_plot) }
