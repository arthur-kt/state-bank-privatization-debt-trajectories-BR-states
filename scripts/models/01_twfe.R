# Estimate static and dynamic two-way fixed-effects specifications.
output_dir <- "outputs/models/twfe"
method_name <- "TWFE"

source("R/setup.R", local = TRUE)
source("R/model_data.R", local = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
all_estimates <- list()
all_dynamics <- list()
for (outcome in cfg$outcomes) {
  sample <- model_panel |> mutate(.outcome = .data[[outcome]]) |> filter(!is.na(.outcome))
  multiplier <- outcome_info$multiplier[match(outcome, outcome_info$name)]
  sample_id <- paste(paste(sort(unique(sample$state_code)), collapse = "-"),
                     min(sample$year), max(sample$year), nrow(sample), cfg$treatment_lag,
                     cfg$reference_period, unname(tools::md5sum("data/metadata/state_bank_events.csv")),
                     unname(tools::md5sum("data/processed/state_panel.csv")), sep = "_")

  fitted <- feols(.outcome ~ post_loss_control | state_code + year,
                   data = sample, cluster = ~state_code)
  dynamic <- feols(as.formula(sprintf(".outcome ~ i(relative_time, loss_of_control, ref = %d) | state_code + year", cfg$reference_period)), data = sample, cluster = ~state_code)
  estimates <- tidy(fitted, conf.int = TRUE) |> filter(term == "post_loss_control")

  estimates <- estimates |>
    mutate(method = method_name, outcome = outcome, sample_id = sample_id,
           observations = nrow(sample), states = n_distinct(sample$state_code),
           reference_period = cfg$reference_period, treatment_lag = cfg$treatment_lag,
           multiplier = multiplier, estimate_display = multiplier * estimate,
           conf_low_display = multiplier * conf.low, conf_high_display = multiplier * conf.high)
  dynamic_table <- tidy(dynamic, conf.int = TRUE) |>
    mutate(k = as.integer(str_extract(term, "(?<=::)-?[0-9]+")),
           method = method_name, outcome = outcome, sample_id = sample_id,
           h = k + cfg$treatment_lag, reference_period = cfg$reference_period,
           treatment_lag = cfg$treatment_lag,
           multiplier = multiplier, estimate_display = multiplier * estimate,
           conf_low_display = multiplier * conf.low, conf_high_display = multiplier * conf.high)
  all_estimates[[outcome]] <- estimates
  all_dynamics[[outcome]] <- dynamic_table
  saveRDS(list(aggregate = fitted, dynamic = dynamic),
          file.path(output_dir, paste0(outcome, "_models.rds")))
  plot_data <- dynamic_table |> filter(between(k, cfg$plot_window[1], cfg$plot_window[2])) |>
    bind_rows(tibble(h = cfg$reference_period + cfg$treatment_lag,
                     estimate_display = 0, conf_low_display = 0, conf_high_display = 0)) |>
    arrange(h)
  p <- ggplot(plot_data, aes(h, estimate_display)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#6B43A5") +
    geom_ribbon(aes(ymin = conf_low_display, ymax = conf_high_display),
                fill = "#2F6FA3", alpha = .15) +
    geom_line(color = "#2F6FA3") + geom_point(color = "#2F6FA3") +
    labs(title = paste(method_name, outcome_info$label[match(outcome, outcome_info$name)]),
         x = "Years relative to the institutional event", y = outcome_info$display_unit[match(outcome, outcome_info$name)]) +
    theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, paste0(outcome, "_event_study.png")), p,
         width = 9, height = 5, dpi = 200)
  if (cfg$show_plots) print(p)
}
write_csv(bind_rows(all_estimates), file.path(output_dir, "estimates.csv"))
write_csv(bind_rows(all_dynamics), file.path(output_dir, "event_study.csv"))
