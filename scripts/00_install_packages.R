# Install packages into a local library that is excluded from version control.
dir.create(".R-library", showWarnings = FALSE)
.libPaths(c(normalizePath(".R-library"), .libPaths()))
packages <- c("tidyverse", "readxl", "jsonlite", "fixest", "broom", "scales",
              "stringi", "remotes", "HonestDiD", "Synth")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
if (!requireNamespace("synthdid", quietly = TRUE)) {
  remotes::install_github("synth-inference/synthdid@70c1ce3eac58e28c30b67435ca377bb48baa9b8a",
                         upgrade = "never")
}
