# Estimate a common-date two-group DiD for the configured state and controls.
source("R/setup.R", local = TRUE)

source("R/extension_data.R", local = TRUE)
table_dir <- "outputs/models/classic_did/tables"
figure_dir <- "outputs/models/classic_did/figures"
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

baseline_controls <- cfg$classic_controls
classic_first_full <- unique(panel$first_full_treatment_year[panel$state_code == cfg$classic_state])
classic_did_sample <- panel %>%
  filter(
    state_code %in% c(cfg$classic_state, baseline_controls),
    between(year, cfg$start_year, cfg$classic_end_year)
  ) %>%
  mutate(
    treated = as.integer(state_code == cfg$classic_state),
    post = as.integer(year >= classic_first_full),
    group = if_else(treated == 1, cfg$classic_state, "Retained-bank controls"),
    period = if_else(post == 1, "Post", "Pre")
  )

classic_did_means <- classic_did_sample %>%
  group_by(group, treated, period, post) %>%
  summarise(
    observations = n(),
    states = n_distinct(state_code),
    mean_debt_to_gdp = mean(debt_to_gdp),
    .groups = "drop"
  )

write_csv(
  classic_did_means,
  file.path(table_dir, "classic_did_group_means.csv")
)

classic_did_model <- feols(
  debt_to_gdp ~ treated + post + treated:post,
  data = classic_did_sample,
  cluster = ~ state_code
)

classic_did_result <- tidy(classic_did_model, conf.int = TRUE) %>%
  filter(term == "treated:post") %>%
  transmute(
    comparison = paste(cfg$classic_state, "versus", paste(baseline_controls, collapse = ", ")),
    pre_period = paste(cfg$start_year, classic_first_full - 1L, sep = "-"),
    post_period = paste(classic_first_full, cfg$classic_end_year, sep = "-"),
    estimate,
    estimate_percentage_points = 100 * estimate,
    standard_error = std.error,
    p_value = p.value,
    confidence_low = conf.low,
    confidence_high = conf.high
  )

write_csv(
  classic_did_result,
  file.path(table_dir, "classic_did_result.csv")
)

classic_did_yearly <- classic_did_sample %>%
  group_by(year, group) %>%
  summarise(mean_debt_to_gdp = mean(debt_to_gdp), .groups = "drop")

classic_did_plot <- ggplot(
  classic_did_yearly,
  aes(x = year, y = mean_debt_to_gdp, color = group)
) +
  geom_vline(xintercept = classic_first_full - 0.5, linetype = "dashed", color = "grey45") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = cfg$start_year:cfg$classic_end_year) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(
    values = setNames(c("#D95F02", "#1B9E77"), c(cfg$classic_state, "Retained-bank controls"))
  ) +
  labs(
    title = paste("Classic difference-in-differences:", cfg$classic_state),
    subtitle = "The dashed line separates the pre and post periods",
    x = "Year",
    y = "Average financial debt / GDP",
    color = "Group"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "classic_did_trajectory.png"),
  classic_did_plot,
  width = 10,
  height = 6,
  dpi = 300
)

comparison_table <- tidy(classic_did_model, conf.int = TRUE) |>
  filter(term == "treated:post") |>
  mutate(method = paste("Classic DiD:", cfg$classic_state), outcome = "debt_to_gdp",
         sample_id = paste("classic", paste(sort(unique(classic_did_sample$state_code)), collapse = "-"),
                           cfg$start_year, cfg$classic_end_year, classic_first_full, sep = "_"),
         observations = nrow(classic_did_sample), states = n_distinct(classic_did_sample$state_code),
         estimate_display = 100 * estimate, conf_low_display = 100 * conf.low,
         conf_high_display = 100 * conf.high)
write_csv(comparison_table, "outputs/models/classic_did/estimates.csv")
saveRDS(classic_did_model, "outputs/models/classic_did/model.rds")
if (cfg$show_plots) print(classic_did_plot)
