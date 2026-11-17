# Compare saved model tables without estimating a model.
source("R/setup.R", local = TRUE)
outcome_info <- read_csv("config/outcomes.csv", show_col_types = FALSE)
panel_labels <- setNames(paste(outcome_info$label, outcome_info$display_unit, sep = " — "),
                         outcome_info$name)
output_dir <- "outputs/comparison"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
tables <- list()
dynamics <- list()
for (model_id in cfg$comparison_models) {
  tables[[model_id]] <- read_csv(file.path("outputs/models", model_id, "estimates.csv"),
                                 show_col_types = FALSE) |> mutate(model_id = model_id)
  dynamic_file <- file.path("outputs/models", model_id, "event_study.csv")
  if (file.exists(dynamic_file)) {
    dynamics[[model_id]] <- read_csv(dynamic_file, show_col_types = FALSE) |>
      mutate(model_id = model_id)
  }
}
estimates <- bind_rows(tables) |> filter(outcome %in% cfg$comparison_outcomes)
if (nrow(estimates) == 0) stop("No saved estimates match comparison_outcomes. Run the selected models first.")
if (!cfg$comparison_allow_different_samples) {
  sample_counts <- estimates |> group_by(outcome) |> summarise(samples = n_distinct(sample_id))
  if (any(sample_counts$samples > 1)) stop("Models use different samples or timing settings. Set comparison_allow_different_samples = TRUE only for an explicit comparison of different targets.")
}
write_csv(estimates, file.path(output_dir, "aggregate_estimates.csv"))
p <- ggplot(estimates, aes(estimate_display, method)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(xmin = conf_low_display, xmax = conf_high_display),
                orientation = "y", width = .15) + geom_point(size = 2.5) +
  facet_wrap(~outcome, scales = "free_x", labeller = as_labeller(panel_labels)) +
  labs(x = "Estimate in the configured display unit", y = NULL) + theme_minimal(base_size = 12)
ggsave(file.path(output_dir, "aggregate_estimates.png"), p, width = 9, height = 5, dpi = 200)
dynamic <- tibble()
if (length(dynamics) > 0) dynamic <- bind_rows(dynamics) |> filter(outcome %in% cfg$comparison_outcomes,
                                        between(k, cfg$plot_window[1], cfg$plot_window[2]))
if (nrow(dynamic) > 0) {
  reference <- dynamic |> distinct(method, outcome, reference_period, treatment_lag, multiplier) |>
    mutate(h = reference_period + treatment_lag, estimate_display = 0,
           conf_low_display = 0, conf_high_display = 0)
  dynamic <- bind_rows(dynamic, reference) |> arrange(method, outcome, h)
  write_csv(dynamic, file.path(output_dir, "event_study.csv"))
  q <- ggplot(dynamic, aes(h, estimate_display, color = method, fill = method)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#6B43A5") +
    geom_ribbon(aes(ymin = conf_low_display, ymax = conf_high_display), alpha = .10, color = NA) +
    geom_line() + geom_point() +
    scale_x_continuous(breaks = seq(min(dynamic$h), max(dynamic$h))) +
    facet_wrap(~outcome, scales = "free_y", labeller = as_labeller(panel_labels)) +
    labs(x = "Years relative to the institutional event", y = "Coefficient (units shown above)",
         color = NULL, fill = NULL) + theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  ggsave(file.path(output_dir, "event_study.png"), q, width = 10, height = 5, dpi = 200)
  if (cfg$show_plots) print(q)
}
if (cfg$show_plots) print(p)
