# Historical extension specifications can use a different control sample.
source("R/model_data.R", local = TRUE)
causal_sample <- model_panel |>
  filter(!state_code %in% cfg$extension_exclude_states)
panel <- read_csv("data/processed/state_panel.csv", show_col_types = FALSE) |>
  arrange(state_code, year) |>
  group_by(state_code) |>
  mutate(log_financial_debt = log(financial_debt_brl),
         annual_log_debt_change_percent = 100 * (log_financial_debt - lag(log_financial_debt)),
         annual_change_debt_to_gdp_pp = 100 * (debt_to_gdp - lag(debt_to_gdp))) |>
  ungroup()
control_states <- sort(unique(causal_sample$state_code[causal_sample$retained_bank == 1]))
