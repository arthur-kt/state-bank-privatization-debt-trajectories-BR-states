# Estimate each method on states within the same selected region set.
source("R/setup.R", local = TRUE)
source("R/extension_data.R", local = TRUE)
output_dir <- "outputs/robustness/regional"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
region_sets <- list(south_southeast = c("South", "Southeast"), northeast = "Northeast", north_northeast = c("North", "Northeast"))
estimates <- list()
dynamics <- list()
for (region_name in names(region_sets)) {
  regional <- causal_sample |> filter(region %in% region_sets[[region_name]])
  for (method in c("TWFE", "Sun-Abraham")) {
    if (method == "TWFE") {
      fit <- feols(debt_to_gdp ~ post_loss_control | state_code + year,
                   data = regional, cluster = ~state_code)
      result <- tidy(fit, conf.int = TRUE) |> filter(term == "post_loss_control")
      dynamic <- feols(as.formula(sprintf("debt_to_gdp ~ i(relative_time, loss_of_control, ref = %d) | state_code + year", cfg$reference_period)), data = regional, cluster = ~state_code)
    } else {
      dynamic <- feols(as.formula(sprintf("debt_to_gdp ~ sunab(treatment_cohort, year, ref.p = %d) | state_code + year", cfg$reference_period)), data = regional, cluster = ~state_code)
      result <- tidy(summary(dynamic, agg = "ATT"), conf.int = TRUE)
    }
    key <- paste(region_name, method)
    estimates[[key]] <- result |> mutate(region_set = region_name, method = method)
    dynamics[[key]] <- tidy(dynamic, conf.int = TRUE) |>
      mutate(k = as.integer(str_extract(term, "(?<=::)-?[0-9]+")),
             h = k + cfg$treatment_lag, region_set = region_name, method = method)
    saveRDS(dynamic, file.path(output_dir, paste0(region_name, "_", method, ".rds")))
  }
}
write_csv(bind_rows(estimates), file.path(output_dir, "estimates.csv"))
dynamic <- bind_rows(dynamics)
write_csv(dynamic, file.path(output_dir, "event_study.csv"))
p <- ggplot(dynamic |> filter(between(k, cfg$plot_window[1], cfg$plot_window[2])),
            aes(h, 100 * estimate, color = method)) +
  geom_hline(yintercept = 0, linetype = "dashed") + geom_line() + geom_point() +
  facet_wrap(~region_set) + labs(x = "Years relative to the event", y = "Percentage points", color = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave(file.path(output_dir, "event_study.png"), p, width = 10, height = 5, dpi = 200)
if (cfg$show_plots) print(p)


# Average controls within each treated-state event before averaging across events.
regional_panels <- list()
for (region_name in names(region_sets)) {
  regional_panels[[region_name]] <- causal_sample |>
    filter(region %in% region_sets[[region_name]]) |>
    mutate(region_set = region_name,
           group = if_else(retained_bank == 1, "Bank retained", "Lost bank control"))
}
regional_panel <- bind_rows(regional_panels)
year_means <- regional_panel |>
  group_by(region_set, year, group) |>
  summarise(states = n_distinct(state_code), debt_to_gdp = mean(debt_to_gdp), .groups = "drop")
write_csv(year_means, file.path(output_dir, "group_year_means.csv"))
anchors <- regional_panel |> filter(loss_of_control == 1) |>
  distinct(region_set, comparison_state = state_code, comparison_year = event_year)
aligned_treated <- regional_panel |> filter(loss_of_control == 1) |>
  mutate(comparison_state = state_code, h = year - event_year)
aligned_controls <- regional_panel |> filter(retained_bank == 1) |>
  inner_join(anchors, by = "region_set", relationship = "many-to-many") |>
  mutate(h = year - comparison_year)
event_means <- bind_rows(aligned_treated, aligned_controls) |>
  filter(between(h, cfg$plot_window[1], cfg$plot_window[2])) |>
  group_by(region_set, comparison_state, h, group) |>
  summarise(debt_to_gdp = mean(debt_to_gdp), .groups = "drop") |>
  group_by(region_set, h, group) |>
  summarise(events = n_distinct(comparison_state), debt_to_gdp = mean(debt_to_gdp), .groups = "drop")
write_csv(event_means, file.path(output_dir, "group_event_time_means.csv"))
year_plot <- ggplot(year_means, aes(year, debt_to_gdp, color = group)) +
  geom_line() + geom_point() + facet_wrap(~region_set, scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c("Bank retained" = "#B23A48", "Lost bank control" = "#2C699A")) +
  labs(title = "Financial debt trajectories within regions", x = "Year",
       y = "Average financial debt / GDP", color = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
event_plot <- ggplot(event_means, aes(h, debt_to_gdp, color = group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#6A3D9A") +
  geom_line() + geom_point() + facet_wrap(~region_set, scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c("Bank retained" = "#B23A48", "Lost bank control" = "#2C699A")) +
  labs(title = "Regional debt trajectories relative to the bank event",
       subtitle = "Retained states are aligned to the treated comparison's event year",
       x = "Years relative to the institutional event", y = "Average financial debt / GDP", color = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave(file.path(output_dir, "group_year_trajectories.png"), year_plot, width = 11, height = 5, dpi = 200)
ggsave(file.path(output_dir, "group_event_time_trajectories.png"), event_plot, width = 11, height = 5, dpi = 200)
pretrends <- dynamic |> filter(between(k, -5, -2)) |>
  group_by(region_set, method) |>
  summarise(coefficients = n(), mean_estimate_pp = 100 * mean(estimate),
            maximum_absolute_estimate_pp = 100 * max(abs(estimate)),
            individually_significant_5pct = sum(p.value < .05, na.rm = TRUE), .groups = "drop")
write_csv(pretrends, file.path(output_dir, "pretrend_diagnostics.csv"))
if (cfg$show_plots) { print(year_plot); print(event_plot) }
