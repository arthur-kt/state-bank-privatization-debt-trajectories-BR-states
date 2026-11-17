# Apply saved log-ratio coefficients to an illustrative retained-state scenario.
source("R/setup.R", local = TRUE)

source("R/model_data.R", local = TRUE)
panel <- read_csv("data/processed/state_panel.csv", show_col_types = FALSE)
causal_sample <- model_panel
saved <- readRDS("outputs/models/sun_abraham/log_debt_to_gdp_models.rds")
dynamic_log_debt <- tidy(saved$dynamic, conf.int = TRUE) |>
  mutate(event_time = as.integer(str_remove(term, "year::")),
         confidence_low = conf.low, confidence_high = conf.high)
table_dir <- "outputs/exploration/hypothetical_scenario/tables"
figure_dir <- "outputs/exploration/hypothetical_scenario/figures"
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
hypothetical_treatment_year <- as.integer(
  median(unique(causal_sample$first_full_treatment_year[causal_sample$loss_of_control == 1]))
)

support <- causal_sample |>
  filter(loss_of_control == 1) |>
  count(relative_time, name = "states")
dynamic_log_debt <- dynamic_log_debt |>
  left_join(support, by = c("event_time" = "relative_time"))
dynamic_effects_for_projection <- dynamic_log_debt %>%
  filter(between(event_time, 0, 10)) %>%
  select(
    hypothetical_event_time = event_time,
    estimated_log_effect = estimate,
    log_effect_confidence_low = conf.low,
    log_effect_confidence_high = conf.high,
    supporting_treated_states = states
  )

retained_counterfactual <- panel %>%
  filter(retained_bank == 1) %>%
  mutate(
    hypothetical_treatment_year = hypothetical_treatment_year,
    hypothetical_event_time = year - hypothetical_treatment_year
  ) %>%
  left_join(
    dynamic_effects_for_projection,
    by = "hypothetical_event_time"
  ) %>%
  mutate(
    estimated_log_effect = if_else(
      year < hypothetical_treatment_year,
      0,
      estimated_log_effect
    ),
    log_effect_confidence_low = if_else(
      year < hypothetical_treatment_year,
      0,
      log_effect_confidence_low
    ),
    log_effect_confidence_high = if_else(
      year < hypothetical_treatment_year,
      0,
      log_effect_confidence_high
    ),
    projected_debt_to_gdp_if_sold = debt_to_gdp * exp(estimated_log_effect),
    projected_confidence_low = debt_to_gdp * exp(log_effect_confidence_low),
    projected_confidence_high = debt_to_gdp * exp(log_effect_confidence_high)
  ) %>%
  select(
    state_code, state_name, year, hypothetical_treatment_year,
    hypothetical_event_time, debt_to_gdp, estimated_log_effect,
    supporting_treated_states, projected_debt_to_gdp_if_sold,
    projected_confidence_low, projected_confidence_high
  )

write_csv(
  retained_counterfactual,
  file.path(table_dir, "retained_states_hypothetical_sale_projection.csv")
)

counterfactual_group <- retained_counterfactual %>%
  group_by(year) %>%
  summarise(
    observed_mean = mean(debt_to_gdp),
    projected_mean_if_sold = mean(projected_debt_to_gdp_if_sold),
    projected_confidence_low = mean(projected_confidence_low),
    projected_confidence_high = mean(projected_confidence_high),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(observed_mean, projected_mean_if_sold),
    names_to = "trajectory",
    values_to = "debt_to_gdp"
  ) %>%
  mutate(
    trajectory = recode(
      trajectory,
      observed_mean = "Observed: banks retained",
      projected_mean_if_sold = "Projected: hypothetical loss of control"
    )
  ) %>%
  filter(!is.na(debt_to_gdp))

counterfactual_group_plot <- ggplot(
  counterfactual_group,
  aes(x = year, y = debt_to_gdp, color = trajectory)
) +
  geom_vline(
    xintercept = hypothetical_treatment_year - 0.5,
    linetype = "dashed",
    color = "grey45"
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1996, 2012, by = 2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("#1B9E77", "#6A3D9A")) +
  labs(
    title = "Hypothetical sale scenario for retained-bank states",
    subtitle = paste0(
      "Projection applies Sun-Abraham effects as if treatment began in ",
      hypothetical_treatment_year
    ),
    x = "Year",
    y = "Average financial debt / GDP",
    color = "Trajectory",
    caption = paste0(
      "Projection uses proportional effects from the log model. Scenario only; ",
      "it is not an identified causal effect for untreated states. ",
      "Only estimated horizons from k=0 through k=10 are shown."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "06_retained_states_hypothetical_sale.png"),
  counterfactual_group_plot,
  width = 10,
  height = 6,
  dpi = 300
)

counterfactual_state_plot_data <- retained_counterfactual %>%
  select(
    state_code, state_name, year, debt_to_gdp,
    projected_debt_to_gdp_if_sold
  ) %>%
  pivot_longer(
    cols = c(debt_to_gdp, projected_debt_to_gdp_if_sold),
    names_to = "trajectory",
    values_to = "value"
  ) %>%
  mutate(
    trajectory = recode(
      trajectory,
      debt_to_gdp = "Observed: bank retained",
      projected_debt_to_gdp_if_sold = "Projected: hypothetical loss of control"
    )
  ) %>%
  filter(!is.na(value))

counterfactual_state_plot <- ggplot(
  counterfactual_state_plot_data,
  aes(x = year, y = value, color = trajectory)
) +
  geom_vline(
    xintercept = hypothetical_treatment_year - 0.5,
    linetype = "dashed",
    color = "grey45"
  ) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ state_code, scales = "free_y") +
  scale_x_continuous(breaks = seq(1996, 2012, by = 4)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("#1B9E77", "#6A3D9A")) +
  labs(
    title = "State-level hypothetical sale scenarios",
    subtitle = paste0("Hypothetical first full treatment year: ", hypothetical_treatment_year),
    x = "Year",
    y = "Financial debt / GDP",
    color = "Trajectory"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "07_retained_states_hypothetical_sale_by_state.png"),
  counterfactual_state_plot,
  width = 12,
  height = 8,
  dpi = 300
)
