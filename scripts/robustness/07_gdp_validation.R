# Compare overlapping official GDP releases without replacing the main series.
source("R/setup.R", local = TRUE)

panel_file <- "data/processed/state_panel.csv"
sidra_file <- "data/raw/ibge_validation/sidra_5938_gdp_2002_2012.json"
direct_release_file <- "data/raw/ibge_validation/tab001_2010_2013.xls"

table_dir <- "outputs/robustness/gdp_validation/tables"
figure_dir <- "outputs/robustness/gdp_validation/figures"

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

sidra_url <- paste0(
  "https://apisidra.ibge.gov.br/values/t/5938/n3/all/v/37/",
  "p/2002-2012?formato=json"
)

direct_release_url <- paste0(
  "https://ftp.ibge.gov.br/Contas_Regionais/2013/xls/",
  "tab001.xls"
)

dir.create(dirname(sidra_file), recursive = TRUE, showWarnings = FALSE)
refresh_downloads <- cfg$refresh_downloads

if (refresh_downloads || !file.exists(sidra_file)) {
  download.file(sidra_url, sidra_file, mode = "wb")
}

if (refresh_downloads || !file.exists(direct_release_file)) {
  download.file(direct_release_url, direct_release_file, mode = "wb")
}

# 2. Main panel: IBGE reference-2002 series reconstructed from official shares

main_panel <- read_csv(panel_file, show_col_types = FALSE) |>
  filter(year >= 2002, year <= 2012) |>
  select(
    state_code,
    state_name,
    region,
    year,
    retained_bank,
    financial_debt_brl,
    union_debt_brl,
    gdp_share_percent,
    gdp_reference_2002_brl = state_gdp_nominal_brl,
    debt_to_gdp_reference_2002 = debt_to_gdp,
    union_debt_to_gdp_reference_2002 = union_debt_to_gdp
  )

share_check <- main_panel |>
  group_by(year) |>
  summarise(
    sum_state_shares_percent = sum(gdp_share_percent),
    distance_from_100 = sum_state_shares_percent - 100,
    .groups = "drop"
  )

# 3. Alternative GDP: direct state values from SIDRA, reference 2010

ibge_code_dictionary <- tribble(
  ~ibge_code, ~state_code,
  "11", "RO", "12", "AC", "13", "AM", "14", "RR", "15", "PA",
  "16", "AP", "17", "TO", "21", "MA", "22", "PI", "23", "CE",
  "24", "RN", "25", "PB", "26", "PE", "27", "AL", "28", "SE",
  "29", "BA", "31", "MG", "32", "ES", "33", "RJ", "35", "SP",
  "41", "PR", "42", "SC", "43", "RS", "50", "MS", "51", "MT",
  "52", "GO", "53", "DF"
)

sidra_raw <- fromJSON(sidra_file) |> as_tibble()

sidra_gdp <- sidra_raw |>
  filter(NC == "3", D2C == "37") |>
  transmute(
    ibge_code = D1C,
    year = as.integer(D3N),
    gdp_reference_2010_brl = as.numeric(V) * 1000
  ) |>
  left_join(ibge_code_dictionary, by = "ibge_code") |>
  select(state_code, year, gdp_reference_2010_brl)

stopifnot(
  nrow(sidra_gdp) == 27 * 11,
  !anyNA(sidra_gdp$state_code),
  !anyDuplicated(sidra_gdp[c("state_code", "year")])
)

# 4. Direct 2013 release: independent check of SIDRA for 2010-2012

state_name_dictionary <- sidra_raw |>
  filter(NC == "3") |>
  distinct(ibge_code = D1C, ibge_state_name = D1N) |>
  left_join(ibge_code_dictionary, by = "ibge_code") |>
  mutate(
    state_name_key = stringi::stri_trans_general(ibge_state_name, "Latin-ASCII") |>
      str_to_lower()
  ) |>
  select(state_code, state_name_key)

direct_raw <- read_excel(direct_release_file, col_names = FALSE)

direct_years <- direct_raw |>
  slice(6) |>
  select(2:5) |>
  unlist(use.names = FALSE) |>
  as.integer()

direct_release_gdp <- direct_raw |>
  transmute(
    state_name_key = stringi::stri_trans_general(as.character(.data[["...1"]]), "Latin-ASCII") |>
      str_to_lower(),
    `2010` = .data[["...2"]],
    `2011` = .data[["...3"]],
    `2012` = .data[["...4"]],
    `2013` = .data[["...5"]]
  ) |>
  inner_join(state_name_dictionary, by = "state_name_key") |>
  select(state_code, starts_with("20")) |>
  mutate(across(starts_with("20"), as.character)) |>
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "direct_release_gdp_brl"
  ) |>
  mutate(
    year = as.integer(year),
    direct_release_gdp_brl = as.numeric(direct_release_gdp_brl) * 1e6
  )

stopifnot(
  identical(direct_years, 2010:2013),
  nrow(direct_release_gdp) == 27 * 4,
  !anyDuplicated(direct_release_gdp[c("state_code", "year")])
)

direct_source_check <- sidra_gdp |>
  filter(year >= 2010) |>
  inner_join(direct_release_gdp, by = c("state_code", "year")) |>
  mutate(
    direct_minus_sidra_brl = direct_release_gdp_brl - gdp_reference_2010_brl,
    direct_minus_sidra_percent = 100 *
      (direct_release_gdp_brl / gdp_reference_2010_brl - 1)
  )

# 5. Compare GDP levels and the resulting debt-to-GDP measures

comparison <- main_panel |>
  left_join(sidra_gdp, by = c("state_code", "year")) |>
  mutate(
    gdp_revision_percent = 100 *
      (gdp_reference_2010_brl / gdp_reference_2002_brl - 1),
    debt_to_gdp_reference_2010 =
      financial_debt_brl / gdp_reference_2010_brl,
    union_debt_to_gdp_reference_2010 =
      union_debt_brl / gdp_reference_2010_brl,
    debt_ratio_difference_pp = 100 *
      (debt_to_gdp_reference_2010 - debt_to_gdp_reference_2002),
    union_debt_ratio_difference_pp = 100 *
      (union_debt_to_gdp_reference_2010 -
         union_debt_to_gdp_reference_2002)
  ) |>
  arrange(state_code, year) |>
  group_by(state_code) |>
  mutate(
    gdp_growth_reference_2002_percent = 100 *
      (gdp_reference_2002_brl / lag(gdp_reference_2002_brl) - 1),
    gdp_growth_reference_2010_percent = 100 *
      (gdp_reference_2010_brl / lag(gdp_reference_2010_brl) - 1),
    debt_ratio_change_reference_2002_pp = 100 *
      (debt_to_gdp_reference_2002 - lag(debt_to_gdp_reference_2002)),
    debt_ratio_change_reference_2010_pp = 100 *
      (debt_to_gdp_reference_2010 - lag(debt_to_gdp_reference_2010))
  ) |>
  ungroup()

stopifnot(!anyNA(comparison$gdp_reference_2010_brl))

overall_summary <- tibble(
  metric = c(
    "State-year observations",
    "Correlation between GDP levels",
    "Correlation between annual GDP growth rates",
    "Correlation between debt-to-GDP levels",
    "Correlation between annual debt-to-GDP changes",
    "Mean GDP revision (%)",
    "Median absolute GDP revision (%)",
    "Maximum absolute GDP revision (%)",
    "Mean absolute debt-ratio difference (percentage points)",
    "Maximum absolute debt-ratio difference (percentage points)",
    "Maximum annual share-sum error (percentage points)",
    "Maximum SIDRA vs direct-release difference (%)"
  ),
  value = c(
    nrow(comparison),
    cor(
      comparison$gdp_reference_2002_brl,
      comparison$gdp_reference_2010_brl
    ),
    cor(
      comparison$gdp_growth_reference_2002_percent,
      comparison$gdp_growth_reference_2010_percent,
      use = "complete.obs"
    ),
    cor(
      comparison$debt_to_gdp_reference_2002,
      comparison$debt_to_gdp_reference_2010
    ),
    cor(
      comparison$debt_ratio_change_reference_2002_pp,
      comparison$debt_ratio_change_reference_2010_pp,
      use = "complete.obs"
    ),
    mean(comparison$gdp_revision_percent),
    median(abs(comparison$gdp_revision_percent)),
    max(abs(comparison$gdp_revision_percent)),
    mean(abs(comparison$debt_ratio_difference_pp)),
    max(abs(comparison$debt_ratio_difference_pp)),
    max(abs(share_check$distance_from_100)),
    max(abs(direct_source_check$direct_minus_sidra_percent))
  )
)

summary_by_state <- comparison |>
  group_by(state_code, state_name, region, retained_bank) |>
  summarise(
    years = n(),
    mean_gdp_reference_2002_brl = mean(gdp_reference_2002_brl),
    mean_gdp_reference_2010_brl = mean(gdp_reference_2010_brl),
    mean_gdp_revision_percent = mean(gdp_revision_percent),
    mean_absolute_gdp_revision_percent = mean(abs(gdp_revision_percent)),
    maximum_absolute_gdp_revision_percent = max(abs(gdp_revision_percent)),
    correlation_gdp_growth = cor(
      gdp_growth_reference_2002_percent,
      gdp_growth_reference_2010_percent,
      use = "complete.obs"
    ),
    mean_debt_to_gdp_reference_2002_percent =
      100 * mean(debt_to_gdp_reference_2002),
    mean_debt_to_gdp_reference_2010_percent =
      100 * mean(debt_to_gdp_reference_2010),
    mean_debt_ratio_difference_pp = mean(debt_ratio_difference_pp),
    maximum_absolute_debt_ratio_difference_pp =
      max(abs(debt_ratio_difference_pp)),
    .groups = "drop"
  ) |>
  arrange(desc(mean_absolute_gdp_revision_percent))

summary_by_year <- comparison |>
  group_by(year) |>
  summarise(
    mean_gdp_revision_percent = mean(gdp_revision_percent),
    median_gdp_revision_percent = median(gdp_revision_percent),
    mean_absolute_gdp_revision_percent = mean(abs(gdp_revision_percent)),
    minimum_gdp_revision_percent = min(gdp_revision_percent),
    maximum_gdp_revision_percent = max(gdp_revision_percent),
    mean_debt_to_gdp_reference_2002_percent =
      100 * mean(debt_to_gdp_reference_2002),
    mean_debt_to_gdp_reference_2010_percent =
      100 * mean(debt_to_gdp_reference_2010),
    mean_debt_ratio_difference_pp = mean(debt_ratio_difference_pp),
    correlation_debt_ratios = cor(
      debt_to_gdp_reference_2002,
      debt_to_gdp_reference_2010
    ),
    .groups = "drop"
  )

summary_by_bank_group <- comparison |>
  mutate(
    bank_group = if_else(
      retained_bank == 1,
      "Retained bank",
      "Other states"
    )
  ) |>
  group_by(bank_group) |>
  summarise(
    states = n_distinct(state_code),
    observations = n(),
    mean_gdp_revision_percent = mean(gdp_revision_percent),
    mean_absolute_gdp_revision_percent = mean(abs(gdp_revision_percent)),
    mean_debt_to_gdp_reference_2002_percent =
      100 * mean(debt_to_gdp_reference_2002),
    mean_debt_to_gdp_reference_2010_percent =
      100 * mean(debt_to_gdp_reference_2010),
    mean_debt_ratio_difference_pp = mean(debt_ratio_difference_pp),
    .groups = "drop"
  )

group_gap_by_year <- comparison |>
  group_by(year, retained_bank) |>
  summarise(
    debt_ratio_reference_2002 = mean(debt_to_gdp_reference_2002),
    debt_ratio_reference_2010 = mean(debt_to_gdp_reference_2010),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = retained_bank,
    values_from = c(
      debt_ratio_reference_2002,
      debt_ratio_reference_2010
    ),
    names_sep = "_group_"
  ) |>
  transmute(
    year,
    retained_minus_other_reference_2002_pp = 100 *
      (debt_ratio_reference_2002_group_1 -
         debt_ratio_reference_2002_group_0),
    retained_minus_other_reference_2010_pp = 100 *
      (debt_ratio_reference_2010_group_1 -
         debt_ratio_reference_2010_group_0),
    change_in_group_gap_pp =
      retained_minus_other_reference_2010_pp -
      retained_minus_other_reference_2002_pp
  )

# 6. Save tables

write_csv(
  comparison,
  file.path(table_dir, "gdp_series_comparison_2002_2012.csv")
)
write_csv(
  overall_summary,
  file.path(table_dir, "gdp_comparison_summary.csv")
)
write_csv(
  summary_by_state,
  file.path(table_dir, "gdp_comparison_by_state.csv")
)
write_csv(
  summary_by_year,
  file.path(table_dir, "gdp_comparison_by_year.csv")
)
write_csv(
  summary_by_bank_group,
  file.path(table_dir, "gdp_comparison_by_bank_group.csv")
)
write_csv(
  group_gap_by_year,
  file.path(table_dir, "debt_ratio_group_gap_by_year.csv")
)
write_csv(
  share_check,
  file.path(table_dir, "reference_2002_share_sum_check.csv")
)
write_csv(
  direct_source_check,
  file.path(table_dir, "sidra_direct_release_check_2010_2012.csv")
)

# 7. Figures

theme_set(theme_minimal(base_size = 12))

plot_gdp_levels <- ggplot(
  comparison,
  aes(gdp_reference_2002_brl, gdp_reference_2010_brl)
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey45") +
  geom_point(aes(color = factor(retained_bank)), alpha = 0.65, size = 2) +
  scale_x_log10(labels = label_number(scale = 1e-9, suffix = " bn")) +
  scale_y_log10(labels = label_number(scale = 1e-9, suffix = " bn")) +
  scale_color_manual(
    values = c("0" = "#738290", "1" = "#B23A48"),
    labels = c("0" = "Other states", "1" = "Retained bank")
  ) +
  labs(
    title = "State GDP under two IBGE methodological references",
    subtitle = "Each point is one state-year, 2002-2012; dashed line is equality",
    x = "GDP reconstructed from reference-2002 sources (BRL, log scale)",
    y = "Direct SIDRA GDP, reference 2010 (BRL, log scale)",
    color = "Bank group"
  ) +
  theme(legend.position = "bottom")

plot_gdp_revision <- summary_by_state |>
  mutate(state_name = fct_reorder(state_name, mean_gdp_revision_percent)) |>
  ggplot(aes(mean_gdp_revision_percent, state_name, color = factor(retained_bank))) +
  geom_vline(xintercept = 0, color = "grey55", linetype = "dashed") +
  geom_point(size = 2.8) +
  scale_color_manual(
    values = c("0" = "#738290", "1" = "#B23A48"),
    labels = c("0" = "Other states", "1" = "Retained bank")
  ) +
  labs(
    title = "Average GDP revision by state",
    subtitle = "Reference-2010 GDP relative to the project reference-2002 series, 2002-2012",
    x = "Average difference (%)",
    y = NULL,
    color = "Bank group"
  ) +
  theme(legend.position = "bottom")

annual_debt_ratios <- comparison |>
  select(
    year,
    debt_to_gdp_reference_2002,
    debt_to_gdp_reference_2010
  ) |>
  pivot_longer(
    -year,
    names_to = "series",
    values_to = "debt_to_gdp"
  ) |>
  mutate(
    series = recode(
      series,
      debt_to_gdp_reference_2002 = "IBGE reference 2002",
      debt_to_gdp_reference_2010 = "IBGE reference 2010"
    )
  ) |>
  group_by(year, series) |>
  summarise(mean_debt_to_gdp = mean(debt_to_gdp), .groups = "drop")

plot_debt_trajectory <- ggplot(
  annual_debt_ratios,
  aes(year, mean_debt_to_gdp, color = series)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  scale_color_manual(values = c("#2C699A", "#D1495B")) +
  labs(
    title = "Average state debt-to-GDP trajectory under each GDP series",
    subtitle = "Unweighted mean across 27 federative units, 2002-2012",
    x = NULL,
    y = "Financial debt / GDP",
    color = "GDP denominator"
  ) +
  theme(legend.position = "bottom")

retained_trajectories <- comparison |>
  filter(retained_bank == 1) |>
  select(
    state_name,
    year,
    debt_to_gdp_reference_2002,
    debt_to_gdp_reference_2010
  ) |>
  pivot_longer(
    starts_with("debt_to_gdp"),
    names_to = "series",
    values_to = "debt_to_gdp"
  ) |>
  mutate(
    series = recode(
      series,
      debt_to_gdp_reference_2002 = "IBGE reference 2002",
      debt_to_gdp_reference_2010 = "IBGE reference 2010"
    )
  )

plot_retained_states <- ggplot(
  retained_trajectories,
  aes(year, debt_to_gdp, color = series)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(vars(state_name), scales = "free_y") +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  scale_color_manual(values = c("#2C699A", "#D1495B")) +
  labs(
    title = "Debt-to-GDP trajectories in the retained-bank states",
    subtitle = "The numerator is identical; only the IBGE GDP reference changes",
    x = NULL,
    y = "Financial debt / GDP",
    color = "GDP denominator"
  ) +
  theme(legend.position = "bottom")

plot_heatmap <- ggplot(
  comparison,
  aes(year, fct_rev(factor(state_code)), fill = debt_ratio_difference_pp)
) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#2C699A",
    mid = "white",
    high = "#D1495B",
    midpoint = 0
  ) +
  labs(
    title = "Change in debt-to-GDP caused by the alternative GDP series",
    subtitle = "Reference 2010 minus reference 2002, in percentage points",
    x = NULL,
    y = "State",
    fill = "Difference\n(pp)"
  )

plot_group_gap <- group_gap_by_year |>
  select(
    year,
    retained_minus_other_reference_2002_pp,
    retained_minus_other_reference_2010_pp
  ) |>
  pivot_longer(
    -year,
    names_to = "series",
    values_to = "group_gap_pp"
  ) |>
  mutate(
    series = recode(
      series,
      retained_minus_other_reference_2002_pp = "IBGE reference 2002",
      retained_minus_other_reference_2010_pp = "IBGE reference 2010"
    )
  ) |>
  ggplot(aes(year, group_gap_pp, color = series)) +
  geom_hline(yintercept = 0, color = "grey60", linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#2C699A", "#D1495B")) +
  labs(
    title = "Debt-to-GDP gap between retained-bank and other states",
    subtitle = "Positive values mean a higher average ratio in retained-bank states",
    x = NULL,
    y = "Retained-bank mean minus other-state mean (percentage points)",
    color = "GDP denominator"
  ) +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "01_gdp_levels_reference_comparison.png"),
  plot_gdp_levels,
  width = 9,
  height = 6,
  dpi = 300
)
ggsave(
  file.path(figure_dir, "02_average_gdp_revision_by_state.png"),
  plot_gdp_revision,
  width = 9,
  height = 8,
  dpi = 300
)
ggsave(
  file.path(figure_dir, "03_average_debt_to_gdp_trajectory.png"),
  plot_debt_trajectory,
  width = 9,
  height = 6,
  dpi = 300
)
ggsave(
  file.path(figure_dir, "04_retained_state_debt_to_gdp_trajectories.png"),
  plot_retained_states,
  width = 11,
  height = 7,
  dpi = 300
)
ggsave(
  file.path(figure_dir, "05_debt_ratio_difference_heatmap.png"),
  plot_heatmap,
  width = 10,
  height = 8,
  dpi = 300
)
ggsave(
  file.path(figure_dir, "06_bank_group_gap_comparison.png"),
  plot_group_gap,
  width = 9,
  height = 6,
  dpi = 300
)

if (interactive()) {
  print(plot_gdp_levels)
  print(plot_gdp_revision)
  print(plot_debt_trajectory)
  print(plot_retained_states)
  print(plot_heatmap)
  print(plot_group_gap)
}
