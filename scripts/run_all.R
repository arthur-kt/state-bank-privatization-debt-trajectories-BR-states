# Run each script in an isolated environment and in dependency order.
source("config/settings.R", local = TRUE)
core_scripts <- c("scripts/01_prepare_data.R", "scripts/02_eda.R",
                  "scripts/exploration/01_bank_importance_matching.R",
                  "scripts/exploration/02_matched_trajectories.R",
                  "scripts/exploration/03_bank_selection.R",
                  "scripts/models/01_twfe.R", "scripts/models/02_sun_abraham.R",
                  "scripts/03_compare_models.R")
extension_scripts <- character()
if (cfg$run_extensions) {
  extension_scripts <- c(list.files("scripts/robustness", "\\.R$", full.names = TRUE),
                        list.files("scripts/models", "^0[3-5].*\\.R$", full.names = TRUE),
                        "scripts/exploration/04_alternative_trajectories.R",
                        "scripts/exploration/05_hypothetical_scenario.R")
}
for (script in c(core_scripts, extension_scripts)) {
  message("Running ", script)
  sys.source(script, envir = new.env(parent = globalenv()))
}
