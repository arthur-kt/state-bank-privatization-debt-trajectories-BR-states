# Download official inputs and construct the state-year analysis table.
source("R/setup.R", local = TRUE)
stopifnot(cfg$start_year >= 1996, cfg$end_year <= 2012)
sources <- read_csv("config/data_sources.csv", show_col_types = FALSE)
for (i in seq_len(nrow(sources))) {
  destination <- sources$path[i]
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (cfg$refresh_downloads || !file.exists(destination)) {
    if (sources$format[i] == "ifdata") {
      response <- fromJSON(sources$url[i])
      writeLines(str_replace_all(response$csvContent, "<br>", "\n"), destination)
    } else {
      download.file(sources$url[i], destination, mode = "wb")
    }
  }
}
unzip("data/raw/ibge/tab01_serie_xls.zip", exdir = "data/raw/ibge")
unzip("data/raw/ibge/tab02_serie_xls.zip", exdir = "data/raw/ibge")
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
treasury_file <- "data/raw/treasury/relatorio-estoque-final.csv"
ibge_gdp_file <- "data/raw/ibge/tab01.xls"
ibge_share_file <- "data/raw/ibge/tab02.xls"
treasury_url <- sources$url[sources$source_id == "treasury"]
ibge_gdp_url <- sources$url[sources$source_id == "ibge_gdp"]
ibge_share_url <- sources$url[sources$source_id == "ibge_shares"]
state_dictionary <- read_csv("data/metadata/states.csv", show_col_types = FALSE) |>
  mutate(ibge_state_name = stringi::stri_trans_general(ibge_state_name, "Latin-ASCII"),
         retained_bank = as.integer(state_code %in% c("DF", "ES", "PA", "RS", "SE")),
         bank_group = if_else(retained_bank == 1,
             "Retained state-owned commercial bank", "All other states"))
event_dictionary <- read_csv("data/metadata/state_bank_events.csv", show_col_types = FALSE) |>
  mutate(first_full_treatment_year = if_else(loss_of_control == 1,
                                            event_year + cfg$treatment_lag, NA_integer_))

debt_raw <- read_delim(
  treasury_file,
  delim = ";",
  skip = 3,
  col_names = FALSE,
  col_types = cols(.default = col_character()),
  name_repair = "minimal",
  show_col_types = FALSE
) |>
  select(1:29)

names(debt_raw) <- c(
  "creditor_category",
  "year",
  state_dictionary$state_code
)

financial_debt <- debt_raw |>
  filter(
grepl("Financeira", creditor_category, fixed = TRUE, useBytes = TRUE),
!grepl("/", creditor_category, fixed = TRUE, useBytes = TRUE)
  ) |>
  pivot_longer(
cols = all_of(state_dictionary$state_code),
names_to = "state_code",
values_to = "financial_debt_brl"
  ) |>
  transmute(
state_code,
year = as.integer(year),
financial_debt_brl = parse_number(
  financial_debt_brl,
  locale = locale(decimal_mark = ",", grouping_mark = ".")
)
  )

union_direct_debt <- debt_raw |>
  filter(startsWith(creditor_category, "1.1.1.1 - ")) |>
  pivot_longer(
cols = all_of(state_dictionary$state_code),
names_to = "state_code",
values_to = "union_direct_debt_brl"
  ) |>
  transmute(
state_code,
year = as.integer(year),
union_direct_debt_brl = parse_number(
  union_direct_debt_brl,
  locale = locale(decimal_mark = ",", grouping_mark = ".")
)
  )

union_indirect_debt <- debt_raw |>
  filter(startsWith(creditor_category, "3.1.1.1 - ")) |>
  pivot_longer(
cols = all_of(state_dictionary$state_code),
names_to = "state_code",
values_to = "union_indirect_debt_brl"
  ) |>
  transmute(
state_code,
year = as.integer(year),
union_indirect_debt_brl = parse_number(
  union_indirect_debt_brl,
  locale = locale(decimal_mark = ",", grouping_mark = ".")
)
  )

debt_panel <- financial_debt |>
  left_join(union_direct_debt, by = c("state_code", "year")) |>
  left_join(union_indirect_debt, by = c("state_code", "year")) |>
  filter(year >= cfg$start_year, year <= cfg$end_year) |>
  mutate(
union_indirect_debt_brl = replace_na(union_indirect_debt_brl, 0),
union_debt_brl = union_direct_debt_brl + union_indirect_debt_brl
  )

# 4. State GDP from one consistent IBGE series

gdp_brazil_raw <- read_excel(
  ibge_gdp_file,
  col_names = FALSE,
  .name_repair = "minimal"
)

gdp_brazil <- tibble(
  year = suppressWarnings(as.integer(gdp_brazil_raw[[1]])),
  gdp_brazil_million_brl = suppressWarnings(
parse_number(as.character(gdp_brazil_raw[[4]]))
  )
) |>
  filter(year >= cfg$start_year, year <= cfg$end_year)

gdp_share_raw <- read_excel(
  ibge_share_file,
  col_names = FALSE,
  .name_repair = "minimal"
)
gdp_share_raw[[1]] <- stringi::stri_trans_general(
  gdp_share_raw[[1]],
  "Latin-ASCII"
)
gdp_years <- as.character(as.integer(unlist(gdp_share_raw[4, 2:19])))
names(gdp_share_raw)[1:19] <- c("ibge_state_name", gdp_years)

gdp_shares <- gdp_share_raw |>
  select(ibge_state_name, all_of(gdp_years)) |>
  filter(ibge_state_name %in% state_dictionary$ibge_state_name) |>
  mutate(across(all_of(gdp_years), as.character)) |>
  pivot_longer(
cols = -ibge_state_name,
names_to = "year",
values_to = "gdp_share_percent"
  ) |>
  mutate(
year = as.integer(year),
gdp_share_percent = as.numeric(gdp_share_percent)
  ) |>
  filter(year >= cfg$start_year, year <= cfg$end_year) |>
  left_join(
state_dictionary |> select(state_code, ibge_state_name),
by = "ibge_state_name"
  )

state_gdp <- gdp_shares |>
  left_join(gdp_brazil, by = "year") |>
  transmute(
state_code,
year,
gdp_share_percent,
state_gdp_nominal_brl =
  gdp_brazil_million_brl * 1e6 * gdp_share_percent / 100
  )

# 5. Final model table

model_table <- expand_grid(
  state_code = state_dictionary$state_code,
  year = cfg$start_year:cfg$end_year
) |>
  left_join(state_dictionary, by = "state_code") |>
  left_join(event_dictionary, by = "state_code") |>
  left_join(debt_panel, by = c("state_code", "year")) |>
  left_join(state_gdp, by = c("state_code", "year")) |>
  mutate(
transition_year = as.integer(
  loss_of_control == 1 & year == event_year
),
post_loss_control = as.integer(
  loss_of_control == 1 & year >= first_full_treatment_year
),
event_time = if_else(
  loss_of_control == 1,
  year - first_full_treatment_year,
  NA_integer_
),
event_study_eligible = as.integer(
  retained_bank == 1 | loss_of_control == 1
),
debt_to_gdp = financial_debt_brl / state_gdp_nominal_brl,
union_debt_to_gdp = union_debt_brl / state_gdp_nominal_brl,
debt_source = treasury_url,
gdp_source = paste(ibge_gdp_url, ibge_share_url, sep = " | "),
gdp_series = "IBGE Regional Accounts, reference 2002"
  ) |>
  select(
state_code,
state_name,
region,
year,
retained_bank,
bank_group,
loss_of_control,
event_type,
main_bank,
event_date,
event_year,
event_date_precision,
first_full_treatment_year,
transition_year,
post_loss_control,
event_time,
event_study_eligible,
event_note,
event_source,
financial_debt_brl,
union_direct_debt_brl,
union_indirect_debt_brl,
union_debt_brl,
gdp_share_percent,
state_gdp_nominal_brl,
debt_to_gdp,
union_debt_to_gdp,
gdp_series,
debt_source,
gdp_source
  ) |>
  arrange(state_code, year)

# Essential integrity checks protect the input to every downstream script.
stopifnot(!anyDuplicated(model_table[c("state_code", "year")]),
          nrow(model_table) == 27 * (cfg$end_year - cfg$start_year + 1),
          !anyNA(model_table$financial_debt_brl), !anyNA(model_table$state_gdp_nominal_brl))
write_csv(model_table, "data/processed/state_panel.csv")
write_csv(event_dictionary, "data/processed/state_bank_events.csv")
sources$md5 <- unname(tools::md5sum(sources$path))
write_csv(sources, "data/raw/source_manifest.csv")
