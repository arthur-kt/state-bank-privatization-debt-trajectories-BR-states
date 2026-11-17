# Construct sensitivity intervals from cohort-aggregated coefficients and covariance.
source("R/setup.R", local = TRUE)

library(HonestDiD)
source("R/extension_data.R", local = TRUE)
table_dir <- "outputs/robustness/honest_did/tables"
figure_dir <- "outputs/robustness/honest_did/figures"
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
event_study_model <- feols(
  debt_to_gdp ~ sunab(treatment_cohort, year, ref.p = -1) |
    state_code + year,
  data = causal_sample,
  cluster = ~ state_code
)

aggregate_sunab_periods <- function(model, periods) {
  raw_coefficients <- model$coefficients
  coefficient_names <- names(raw_coefficients)
  model_matrix <- model.matrix(model)
  covariance_matrix <- model$cov.scaled
  aggregation_matrix <- matrix(
    0,
    nrow = length(periods),
    ncol = length(coefficient_names),
    dimnames = list(as.character(periods), coefficient_names)
  )

  for (row_index in seq_along(periods)) {
    prefix <- paste0("year::", periods[row_index], ":cohort::")
    selected_names <- coefficient_names[
      startsWith(coefficient_names, prefix)
    ]
    if (length(selected_names) == 0) {
      stop("No Sun-Abraham coefficients were found for period ", periods[row_index])
    }
    shares <- colSums(sign(model_matrix[, selected_names, drop = FALSE]))
    shares <- shares / sum(shares)
    aggregation_matrix[
      row_index,
      match(selected_names, coefficient_names)
    ] <- shares
  }

  estimates <- as.vector(aggregation_matrix %*% raw_coefficients)
  covariance <- aggregation_matrix %*%
    covariance_matrix %*%
    t(aggregation_matrix)

  list(
    periods = periods,
    estimates = estimates,
    covariance = covariance,
    aggregation_matrix = aggregation_matrix
  )
}

honest_periods <- c(-4L, -3L, -2L, 0:8)
honest_inputs <- aggregate_sunab_periods(event_study_model, honest_periods)

fixest_period_table <- aggregate(event_study_model, agg = "period")
fixest_selected <- fixest_period_table[
  paste0("year::", honest_periods),
  ,
  drop = FALSE
]
stopifnot(
  max(abs(fixest_selected[, "Estimate"] - honest_inputs$estimates)) < 1e-10,
  max(abs(
    fixest_selected[, "Std. Error"] -
      sqrt(diag(honest_inputs$covariance))
  )) < 1e-10
)

num_pre_periods <- 3L
num_post_periods <- 9L
mbar_values <- seq(0, 2, by = 0.5)

honest_targets <- tribble(
  ~target, ~target_label, ~l_vector,
  "immediate", "Average effect at k = 0 to 1", c(0.5, 0.5, rep(0, 7)),
  "long_run", "Average effect at k = 4 to 8", c(rep(0, 4), rep(0.2, 5))
)

honest_results <- list()
honest_original_results <- list()

for (target_index in seq_len(nrow(honest_targets))) {
  target_name <- honest_targets$target[target_index]
  target_label <- honest_targets$target_label[target_index]
  l_vector <- honest_targets$l_vector[[target_index]]

  original_result <- constructOriginalCS(
    betahat = honest_inputs$estimates,
    sigma = honest_inputs$covariance,
    numPrePeriods = num_pre_periods,
    numPostPeriods = num_post_periods,
    l_vec = l_vector,
    alpha = 0.05
  )

  robust_result <- createSensitivityResults_relativeMagnitudes(
    betahat = honest_inputs$estimates,
    sigma = honest_inputs$covariance,
    numPrePeriods = num_pre_periods,
    numPostPeriods = num_post_periods,
    Mbarvec = mbar_values,
    l_vec = l_vector,
    alpha = 0.05,
    method = "C-LF"
  )

  honest_original_results[[target_name]] <- as_tibble(original_result) |>
    mutate(
      lb = as.numeric(lb),
      ub = as.numeric(ub),
      target = target_name,
      target_label = target_label
    )
  honest_results[[target_name]] <- as_tibble(robust_result) |>
    mutate(
      lb = as.numeric(lb),
      ub = as.numeric(ub),
      Mbar = as.numeric(Mbar),
      target = target_name,
      target_label = target_label
    )

  robust_plot_data <- as_tibble(robust_result) |>
    transmute(
      Mbar = as.numeric(Mbar),
      confidence_low_percentage_points = 100 * as.numeric(lb),
      confidence_high_percentage_points = 100 * as.numeric(ub)
    )

  sensitivity_plot <- ggplot(
    robust_plot_data,
    aes(Mbar)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
    geom_ribbon(
      aes(
        ymin = confidence_low_percentage_points,
        ymax = confidence_high_percentage_points
      ),
      fill = "#9ECAE1",
      alpha = 0.45
    ) +
    geom_line(
      aes(y = confidence_low_percentage_points),
      linewidth = 0.9,
      color = "#2C699A"
    ) +
    geom_line(
      aes(y = confidence_high_percentage_points),
      linewidth = 0.9,
      color = "#2C699A"
    ) +
    geom_point(
      aes(y = confidence_low_percentage_points),
      size = 2,
      color = "#2C699A"
    ) +
    geom_point(
      aes(y = confidence_high_percentage_points),
      size = 2,
      color = "#2C699A"
    ) +
    scale_x_continuous(breaks = mbar_values) +
    labs(
      title = paste("Honest DiD sensitivity:", target_label),
      subtitle = paste0(
        "Relative-magnitude restriction; event-study window k = -4 to 8; ",
        "DF excluded"
      ),
      x = "Sensitivity parameter M",
      y = "Robust 95% confidence interval (percentage points)",
      caption = paste0(
        "M = 0 imposes parallel trends. Larger M allows post-event trend ",
        "violations relative to the largest pre-event violation."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  ggsave(
    file.path(
      figure_dir,
      paste0("01_honest_did_", target_name, ".png")
    ),
    sensitivity_plot,
    width = 8.5,
    height = 5.5,
    dpi = 300
  )
}

honest_results <- bind_rows(honest_results)
honest_original_results <- bind_rows(honest_original_results)

write_csv(
  honest_results,
  file.path(table_dir, "honest_did_relative_magnitude_results.csv")
)
write_csv(
  honest_original_results,
  file.path(table_dir, "honest_did_original_confidence_intervals.csv")
)
write_csv(
  tibble(
    event_time = honest_inputs$periods,
    estimate = honest_inputs$estimates,
    standard_error = sqrt(diag(honest_inputs$covariance))
  ),
  file.path(table_dir, "honest_did_event_study_inputs.csv")
)
