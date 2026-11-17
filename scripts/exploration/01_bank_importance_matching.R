# Parse bank balances, construct the importance index and select state pairs.
source("R/setup.R", local = TRUE)

panel_file <- "data/processed/state_panel.csv"
event_file <- "data/processed/state_bank_events.csv"

raw_dir <- "data/raw/bcb_ifdata"
output_dir <- "outputs/exploration/bank_matching/tables"
figure_dir <- "outputs/exploration/bank_matching/figures"

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(panel_file) || !file.exists(event_file)) {
  stop("Run scripts/01_prepare_data.R before this script.")
}

snapshot_year <- 1996L
snapshot_date <- 199612L

bcb_rest_url <- "https://www3.bcb.gov.br/ifdata/rest/arqleg"
bcb_page_url <- "https://www3.bcb.gov.br/ifdata/legado.html"

summary_report_name <- "199612/Top501996120P.txt"
asset_report_name <- "199612/Top501996121P.txt"

summary_raw_file <- file.path(raw_dir, "ifdata_summary_1996_12.csv")
asset_raw_file <- file.path(raw_dir, "ifdata_assets_1996_12.csv")

# 2. Parse the historical IF.data files

read_ranked_lines <- function(path, expected_columns) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  bank_lines <- lines[stringr::str_detect(lines, "^[0-9]+;")]

  split_lines <- stringr::str_split(bank_lines, ";")
  padded <- lapply(split_lines, function(values) {
    length(values) <- expected_columns
    replace(values, is.na(values), NA_character_)
  })

  as_tibble(do.call(rbind, padded), .name_repair = "minimal")
}

parse_brl_thousand <- function(x) {
  readr::parse_number(
    x,
    locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
  ) * 1000
}

summary_raw <- read_ranked_lines(summary_raw_file, 15)
names(summary_raw) <- c(
  "ranking", "bank_name", "aggregation_type", "control_type", "note",
  "balance_date", "city", "bank_state_code", "assets_minus_intermediation",
  "bank_assets_brl", "bank_equity_brl", "net_income_brl",
  "bank_deposits_brl", "employees", "bank_branches"
)

summary_data <- summary_raw %>%
  transmute(
    bank_name = str_squish(bank_name),
    aggregation_type,
    control_type = as.integer(control_type),
    balance_date,
    city = str_squish(city),
    bank_state_code,
    bank_assets_brl = parse_brl_thousand(bank_assets_brl),
    bank_deposits_brl = parse_brl_thousand(bank_deposits_brl),
    bank_equity_brl = parse_brl_thousand(bank_equity_brl),
    employees = readr::parse_number(
      employees,
      locale = locale(decimal_mark = ",", grouping_mark = ".")
    ),
    bank_branches = readr::parse_number(
      bank_branches,
      locale = locale(decimal_mark = ",", grouping_mark = ".")
    )
  )

asset_raw <- read_ranked_lines(asset_raw_file, 18)
names(asset_raw) <- c(
  "ranking", "bank_name", "aggregation_type", "control_type", "note",
  "balance_date", "bank_assets_brl", "cash_brl", "interbank_investments_brl",
  "securities_brl", "interfinancial_relations_brl",
  "interdependence_relations_brl", "bank_credit_brl", "credit_provision_brl",
  "other_credits_brl", "other_assets_brl", "leased_assets_brl",
  "permanent_assets_brl"
)

asset_data <- asset_raw %>%
  transmute(
    bank_name = str_squish(bank_name),
    aggregation_type,
    control_type = as.integer(control_type),
    balance_date,
    bank_credit_brl = parse_brl_thousand(bank_credit_brl)
  )

bank_balance_sheets <- summary_data %>%
  left_join(
    asset_data,
    by = c(
      "bank_name", "aggregation_type", "control_type", "balance_date"
    )
  )

# 3. Map the principal state-owned banks to their home states

bank_state_map <- read_csv("data/metadata/bank_state_map.csv", show_col_types = FALSE)

state_bank_totals <- bank_state_map %>%
  left_join(bank_balance_sheets, by = "bank_name") %>%
  group_by(state_code) %>%
  summarise(
    banks_expected = paste(bank_name, collapse = " + "),
    banks_found = paste(bank_name[!is.na(bank_assets_brl)], collapse = " + "),
    number_of_banks_found = sum(!is.na(bank_assets_brl)),
    bank_assets_brl = if_else(
      number_of_banks_found > 0,
      sum(bank_assets_brl, na.rm = TRUE),
      NA_real_
    ),
    bank_credit_brl = if_else(
      number_of_banks_found > 0,
      sum(bank_credit_brl, na.rm = TRUE),
      NA_real_
    ),
    bank_deposits_brl = if_else(
      number_of_banks_found > 0,
      sum(bank_deposits_brl, na.rm = TRUE),
      NA_real_
    ),
    bank_equity_brl = if_else(
      number_of_banks_found > 0,
      sum(bank_equity_brl, na.rm = TRUE),
      NA_real_
    ),
    bank_branches = if_else(
      number_of_banks_found > 0,
      sum(bank_branches, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

# 4. Construct the common 1996 state profile and dependence index

panel <- read_csv(panel_file, show_col_types = FALSE)
events <- read_csv(event_file, show_col_types = FALSE)

state_profile <- panel %>%
  filter(year == snapshot_year) %>%
  select(
    state_code, state_name, region, retained_bank, bank_group,
    loss_of_control, event_type, main_bank, event_year,
    state_gdp_nominal_brl, financial_debt_brl, union_debt_brl,
    debt_to_gdp, union_debt_to_gdp
  ) %>%
  left_join(state_bank_totals, by = "state_code") %>%
  mutate(
    bank_data_available = as.integer(!is.na(bank_assets_brl)),
    bank_assets_to_gdp = bank_assets_brl / state_gdp_nominal_brl,
    bank_credit_to_gdp = bank_credit_brl / state_gdp_nominal_brl,
    bank_deposits_to_gdp = bank_deposits_brl / state_gdp_nominal_brl,
    bank_branches_per_billion_gdp =
      bank_branches / (state_gdp_nominal_brl / 1e9),
    log_state_gdp = log(state_gdp_nominal_brl),
    bank_source = bcb_page_url,
    bank_snapshot = "December 1996"
  )

state_profile <- state_profile %>%
  mutate(
    asset_importance_rank = percent_rank(bank_assets_to_gdp),
    credit_importance_rank = percent_rank(bank_credit_to_gdp),
    deposit_importance_rank = percent_rank(bank_deposits_to_gdp),
    branch_importance_rank = percent_rank(bank_branches_per_billion_gdp),
    bank_dependence_index = rowMeans(
      across(c(
        asset_importance_rank,
        credit_importance_rank,
        deposit_importance_rank,
        branch_importance_rank
      )),
      na.rm = FALSE
    ),
    bank_dependence_rank = if_else(
      !is.na(bank_dependence_index),
      min_rank(desc(bank_dependence_index)),
      NA_integer_
    )
  )

write_csv(
  state_profile,
  file.path(output_dir, "state_bank_dependence_1996.csv")
)

# 5. Calculate economic and combined similarity distances

matching_sample <- state_profile %>%
  filter(
    retained_bank == 1 | loss_of_control == 1,
    bank_data_available == 1
  )

matching_variables <- c(
  "log_state_gdp",
  "debt_to_gdp",
  "union_debt_to_gdp",
  "bank_dependence_index"
)

matching_z <- matching_sample %>%
  mutate(
    across(
      all_of(matching_variables),
      ~ as.numeric(scale(.x)),
      .names = "z_{.col}"
    )
  )

retained_states <- matching_z %>%
  filter(retained_bank == 1) %>%
  arrange(state_code)

treated_states <- matching_z %>%
  filter(loss_of_control == 1) %>%
  arrange(state_code)

economic_z_variables <- c(
  "z_log_state_gdp",
  "z_debt_to_gdp",
  "z_union_debt_to_gdp"
)

combined_z_variables <- c(
  economic_z_variables,
  "z_bank_dependence_index"
)

pair_distances <- tidyr::crossing(
  retained_row = seq_len(nrow(retained_states)),
  treated_row = seq_len(nrow(treated_states))
) %>%
  rowwise() %>%
  mutate(
    retained_state = retained_states$state_code[retained_row],
    treated_state = treated_states$state_code[treated_row],
    economic_distance = sqrt(sum(
      (
        as.numeric(retained_states[retained_row, economic_z_variables]) -
          as.numeric(treated_states[treated_row, economic_z_variables])
      )^2
    )),
    combined_distance = sqrt(sum(
      (
        as.numeric(retained_states[retained_row, combined_z_variables]) -
          as.numeric(treated_states[treated_row, combined_z_variables])
      )^2
    ))
  ) %>%
  ungroup() %>%
  select(
    retained_state, treated_state, economic_distance, combined_distance
  ) %>%
  group_by(retained_state) %>%
  arrange(combined_distance, .by_group = TRUE) %>%
  mutate(combined_similarity_rank = row_number()) %>%
  arrange(economic_distance, .by_group = TRUE) %>%
  mutate(economic_similarity_rank = row_number()) %>%
  ungroup()

write_csv(
  pair_distances,
  file.path(output_dir, "state_similarity_distances_1996.csv")
)

# 6. Optimal one-to-one selection without replacement

find_optimal_pairs <- function(distance_table, distance_column) {
  controls <- sort(unique(distance_table$retained_state))
  candidates <- sort(unique(distance_table$treated_state))

  distance_matrix <- distance_table %>%
    select(retained_state, treated_state, all_of(distance_column)) %>%
    pivot_wider(names_from = treated_state, values_from = all_of(distance_column)) %>%
    arrange(match(retained_state, controls))

  distance_values <- as.matrix(distance_matrix[, -1])
  colnames(distance_values) <- names(distance_matrix)[-1]

  best_cost <- Inf
  best_assignment <- rep(NA_integer_, length(controls))
  number_of_candidates <- length(candidates)

  for (a in seq_len(number_of_candidates)) {
    for (b in setdiff(seq_len(number_of_candidates), a)) {
      for (c in setdiff(seq_len(number_of_candidates), c(a, b))) {
        for (d in setdiff(seq_len(number_of_candidates), c(a, b, c))) {
          for (e in setdiff(seq_len(number_of_candidates), c(a, b, c, d))) {
            assignment <- c(a, b, c, d, e)
            cost <- sum(distance_values[cbind(seq_along(controls), assignment)])

            if (cost < best_cost) {
              best_cost <- cost
              best_assignment <- assignment
            }
          }
        }
      }
    }
  }

  tibble(
    retained_state = controls,
    treated_state = colnames(distance_values)[best_assignment],
    distance = distance_values[cbind(seq_along(controls), best_assignment)],
    total_assignment_distance = best_cost,
    matching_specification = distance_column
  )
}

economic_pairs <- find_optimal_pairs(pair_distances, "economic_distance")
combined_pairs <- find_optimal_pairs(pair_distances, "combined_distance")

matched_pairs <- bind_rows(economic_pairs, combined_pairs) %>%
  left_join(
    state_profile %>%
      select(
        retained_state = state_code,
        retained_state_name = state_name,
        retained_gdp_brl = state_gdp_nominal_brl,
        retained_debt_to_gdp = debt_to_gdp,
        retained_bank_dependence = bank_dependence_index
      ),
    by = "retained_state"
  ) %>%
  left_join(
    state_profile %>%
      select(
        treated_state = state_code,
        treated_state_name = state_name,
        treated_gdp_brl = state_gdp_nominal_brl,
        treated_debt_to_gdp = debt_to_gdp,
        treated_bank_dependence = bank_dependence_index,
        treated_event_type = event_type,
        treated_event_year = event_year
      ),
    by = "treated_state"
  ) %>%
  arrange(matching_specification, retained_state)

write_csv(
  matched_pairs,
  file.path(output_dir, "matched_state_pairs_1996.csv")
)

# 7. Balance diagnostics for the recommended combined match

recommended_pairs <- combined_pairs
matched_treated_codes <- recommended_pairs$treated_state

balance_data <- matching_z %>%
  mutate(
    analysis_group = case_when(
      retained_bank == 1 ~ "Retained-bank states",
      state_code %in% matched_treated_codes ~ "Matched treated states",
      loss_of_control == 1 ~ "All treated states",
      TRUE ~ NA_character_
    )
  )

balance_table <- map_dfr(
  matching_variables,
  function(variable) {
    z_variable <- paste0("z_", variable)

    retained_mean <- balance_data %>%
      filter(retained_bank == 1) %>%
      summarise(value = mean(.data[[z_variable]])) %>%
      pull(value)

    all_treated_mean <- balance_data %>%
      filter(loss_of_control == 1) %>%
      summarise(value = mean(.data[[z_variable]])) %>%
      pull(value)

    matched_treated_mean <- balance_data %>%
      filter(state_code %in% matched_treated_codes) %>%
      summarise(value = mean(.data[[z_variable]])) %>%
      pull(value)

    tibble(
      variable,
      standardized_difference_before = all_treated_mean - retained_mean,
      standardized_difference_after = matched_treated_mean - retained_mean,
      absolute_difference_before = abs(standardized_difference_before),
      absolute_difference_after = abs(standardized_difference_after)
    )
  }
)

write_csv(
  balance_table,
  file.path(output_dir, "matching_balance_1996.csv")
)

# 8. Figures

dependence_plot_data <- state_profile %>%
  filter(!is.na(bank_dependence_index)) %>%
  mutate(
    group = if_else(retained_bank == 1, "Retained bank", "Lost control"),
    state_code = forcats::fct_reorder(state_code, bank_dependence_index)
  )

dependence_plot <- ggplot(
  dependence_plot_data,
  aes(x = bank_dependence_index, y = state_code, color = group)
) +
  geom_point(size = 2.6) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Lost control" = "#1F78B4", "Retained bank" = "#E31A1C")) +
  labs(
    title = "State-owned bank importance relative to the state economy",
    subtitle = "December 1996 common pre-treatment snapshot",
    x = "Bank dependence index",
    y = "State",
    color = "Bank group",
    caption = paste0(
      "Index: equal-weight percentile average of assets/GDP, credit/GDP, ",
      "deposits/GDP and branches per R$1bn of GDP."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "01_bank_dependence_index_1996.png"),
  dependence_plot,
  width = 10,
  height = 7,
  dpi = 300
)

pair_plot_data <- recommended_pairs %>%
  left_join(
    state_profile %>% select(state_code, bank_dependence_index),
    by = c("retained_state" = "state_code")
  ) %>%
  rename(retained_dependence = bank_dependence_index) %>%
  left_join(
    state_profile %>% select(state_code, bank_dependence_index),
    by = c("treated_state" = "state_code")
  ) %>%
  rename(treated_dependence = bank_dependence_index) %>%
  pivot_longer(
    cols = c(retained_dependence, treated_dependence),
    names_to = "group",
    values_to = "bank_dependence_index"
  ) %>%
  mutate(
    state_code = if_else(
      group == "retained_dependence",
      retained_state,
      treated_state
    ),
    pair = retained_state,
    group = recode(
      group,
      retained_dependence = "Retained bank",
      treated_dependence = "Matched loss-of-control state"
    )
  )

pair_plot <- ggplot(
  pair_plot_data,
  aes(
    x = bank_dependence_index,
    y = pair,
    color = group,
    group = pair
  )
) +
  geom_line(color = "grey70", linewidth = 0.7) +
  geom_point(size = 3) +
  geom_text(aes(label = state_code), nudge_y = 0.15, show.legend = FALSE) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = c(
    "Retained bank" = "#E31A1C",
    "Matched loss-of-control state" = "#1F78B4"
  )) +
  labs(
    title = "Recommended one-to-one state matches",
    subtitle = "Matching uses GDP, fiscal debt ratios and bank dependence in 1996",
    x = "Bank dependence index",
    y = "Retained-bank state",
    color = "State type"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "02_recommended_state_matches_1996.png"),
  pair_plot,
  width = 10,
  height = 6,
  dpi = 300
)

balance_plot_data <- balance_table %>%
  select(
    variable,
    `Before matching` = absolute_difference_before,
    `After matching` = absolute_difference_after
  ) %>%
  pivot_longer(
    cols = -variable,
    names_to = "sample",
    values_to = "absolute_standardized_difference"
  )

balance_plot <- ggplot(
  balance_plot_data,
  aes(
    x = absolute_standardized_difference,
    y = forcats::fct_reorder(variable, absolute_standardized_difference),
    color = sample
  )
) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0.25, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c(
    "Before matching" = "#7570B3",
    "After matching" = "#1B9E77"
  )) +
  labs(
    title = "Covariate balance before and after matching",
    x = "Absolute standardized mean difference",
    y = NULL,
    color = "Sample"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "03_matching_balance_1996.png"),
  balance_plot,
  width = 9,
  height = 5.5,
  dpi = 300
)
