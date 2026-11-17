# Rank state banks by assets and compare their balance-sheet measures.
source("R/setup.R", local = TRUE)

audit_dir <- "outputs/exploration/bank_selection"
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
book <- read_csv("data/metadata/bank_assets_1996.csv",
                 show_col_types = FALSE)
events <- read_csv("data/metadata/state_bank_events.csv", show_col_types = FALSE)

ranked <- book %>%
  mutate(eligible = bank_role %in% c("commercial", "savings")) %>%
  group_by(state_code) %>%
  mutate(listed_assets_brl = sum(assets_brl),
         eligible_assets_brl = sum(assets_brl[eligible]),
         share_all_listed = assets_brl / listed_assets_brl,
         share_eligible = if_else(eligible, assets_brl / eligible_assets_brl, NA_real_)) %>%
  ungroup()
write_csv(ranked, file.path(audit_dir, "bank_size_comparison_1996.csv"))

principal <- ranked %>% filter(eligible) %>%
  group_by(state_code) %>% slice_max(assets_brl, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(state_code, principal_by_assets = bank_name, assets_brl,
         share_all_listed, share_eligible, printed_page)
principal <- events %>%
  select(state_code, current_selected_bank = main_bank, event_year, event_type) %>%
  left_join(principal, by = "state_code") %>%
  mutate(coverage = case_when(
    state_code == "RN" ~ "No 1996 operating-bank observation; earlier liquidation predates panel",
    state_code %in% c("MS", "TO") ~ "No comparable own state bank",
    event_type == "retained" ~ "Common 1996 reference; no exit event",
    TRUE ~ "1996 precedes selected exit; may follow earlier intervention or negotiations"
  ))
write_csv(principal, file.path(audit_dir, "principal_banks_1996.csv"))

summary_lines <- readLines("data/raw/bcb_ifdata/ifdata_summary_1996_12.csv", warn = FALSE)
asset_lines <- readLines("data/raw/bcb_ifdata/ifdata_assets_1996_12.csv", warn = FALSE)
summary <- read_delim(I(paste(summary_lines[str_detect(summary_lines, "^[0-9]+;")],
                              collapse = "\n")), delim = ";", col_names = FALSE,
                      col_types = cols(.default = col_character()), trim_ws = TRUE)
assets <- read_delim(I(paste(asset_lines[str_detect(asset_lines, "^[0-9]+;")],
                             collapse = "\n")), delim = ";", col_names = FALSE,
                     col_types = cols(.default = col_character()), trim_ws = TRUE)
br_locale <- locale(decimal_mark = ",", grouping_mark = ".")
summary <- summary %>% transmute(
  bank_name = str_squish(X2), td = X3, tc = X4, balance_date = X6,
  state_code = X8, assets_brl = parse_number(X10, locale = br_locale) * 1000,
  deposits_brl = parse_number(X13, locale = br_locale) * 1000,
  branches = parse_number(X15, locale = br_locale))
assets <- assets %>% transmute(
  bank_name = str_squish(X2), td = X3, tc = X4, balance_date = X6,
  credit_brl = parse_number(X13, locale = br_locale) * 1000)
balances <- summary %>% filter(tc == "2") %>%
  left_join(assets, by = c("bank_name", "td", "tc", "balance_date"))
write_csv(balances, file.path(audit_dir, "ifdata_state_bank_balances_1996.csv"))

competing <- balances %>%
  filter(bank_name %in% c("BANESPA", "NOSSA CAIXA", "CREDIREAL", "BEMGE")) %>%
  select(state_code, bank_name, assets_brl, credit_brl, deposits_brl, branches) %>%
  pivot_longer(cols = c(assets_brl, credit_brl, deposits_brl, branches),
               names_to = "metric", values_to = "value") %>%
  group_by(state_code, metric) %>%
  mutate(pair_total = sum(value), share = value / pair_total,
         rank = min_rank(desc(value))) %>% ungroup()
write_csv(competing, file.path(audit_dir, "sp_mg_balance_sheet_comparison_1996.csv"))
