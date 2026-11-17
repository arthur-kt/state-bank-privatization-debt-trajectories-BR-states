# Construct the common model sample before selecting an outcome.
model_panel <- read_csv("data/processed/state_panel.csv", show_col_types = FALSE) |>
  arrange(state_code, year) |>
  group_by(state_code) |>
  mutate(log_debt_to_gdp = log(debt_to_gdp),
         log_financial_debt = log(financial_debt_brl),
         annual_log_debt_change_percent = 100 * (log_financial_debt - lag(log_financial_debt)),
         annual_change_debt_to_gdp_pp = 100 * (debt_to_gdp - lag(debt_to_gdp))) |>
  ungroup() |>
  filter(between(year, cfg$start_year, cfg$end_year),
         !state_code %in% cfg$exclude_states,
         retained_bank == 1 | (loss_of_control == 1 & event_type %in% cfg$treatment_types)) |>
  mutate(first_full_treatment_year = if_else(loss_of_control == 1,
                                            event_year + cfg$treatment_lag, NA_integer_),
         post_loss_control = as.integer(loss_of_control == 1 & year >= first_full_treatment_year),
         treatment_cohort = if_else(loss_of_control == 1, first_full_treatment_year, 10000L),
         relative_time = if_else(loss_of_control == 1, year - first_full_treatment_year, 0L))
outcome_info <- read_csv("config/outcomes.csv", show_col_types = FALSE)
