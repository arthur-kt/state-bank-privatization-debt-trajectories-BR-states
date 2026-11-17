# Re-estimate each main method after omitting selected states or changing timing.
source("R/setup.R", local = TRUE)
source("R/model_data.R", local = TRUE)
output_dir <- "outputs/robustness/sample_sensitivity"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
variants <- c(list(baseline = character(), exclude_df = "DF", exclude_pb = "PB",
                   exclude_rn = "RN", end_2010 = character(), direct_sales = character()),
              setNames(as.list(sort(unique(model_panel$state_code))),
                       paste0("omit_", sort(unique(model_panel$state_code)))))
estimates <- list()
for (variant in names(variants)) {
  sample <- model_panel |> filter(!state_code %in% variants[[variant]])
  if (variant == "end_2010") sample <- sample |> filter(year <= 2010)
  if (variant == "direct_sales") sample <- sample |> filter(retained_bank == 1 | event_type == "direct_sale")
  for (method in c("TWFE", "Sun-Abraham")) {
    if (method == "TWFE") {
      model <- feols(debt_to_gdp ~ post_loss_control | state_code + year,
                     data = sample, cluster = ~state_code)
      result <- tidy(model, conf.int = TRUE) |> filter(term == "post_loss_control")
    } else {
      model <- feols(as.formula(sprintf("debt_to_gdp ~ sunab(treatment_cohort, year, ref.p = %d) | state_code + year", cfg$reference_period)), data = sample, cluster = ~state_code)
      result <- tidy(summary(model, agg = "ATT"), conf.int = TRUE)
    }
    estimates[[paste(variant, method)]] <- result |>
      mutate(variant = variant, method = method, observations = nrow(sample))
  }
}
write_csv(bind_rows(estimates), file.path(output_dir, "estimates.csv"))
