# Script guide

Run every file from the project root. Preparation must precede other scripts.
Outputs are written only to the named exercise's directory under `outputs/`.

| Script | Purpose | Output directory |
|---|---|---|
| `scripts/00_install_packages.R` | Install core and extension packages | `.R-library/` |
| `scripts/01_prepare_data.R` | Download/cache debt, GDP and IF.data; rebuild panel | `data/raw/`, `data/processed/` |
| `scripts/02_eda.R` | Summary statistics, missingness, distributions, correlations, group/state trajectories, nominal values and log diagnostics | `outputs/eda/` |
| `scripts/models/01_twfe.R` | Static TWFE and dynamic TWFE event study | `outputs/models/twfe/` |
| `scripts/models/02_sun_abraham.R` | Sun–Abraham event study and aggregated ATT | `outputs/models/sun_abraham/` |
| `scripts/models/03_classic_did.R` | Configurable single-state/common-date two-group DiD; BA and four retained controls by default | `outputs/models/classic_did/` |
| `scripts/models/04_synthetic_control.R` | Single-state synthetic control; PB by default, debt/GDP, clean pre-event years | `outputs/models/synthetic_control/` |
| `scripts/models/05_synthetic_did.R` | Synthetic DiD by common-event cohort, at least three clean pre-event years; transition year omitted | `outputs/models/synthetic_did/` |
| `scripts/03_compare_models.R` | Read model exports; compare aggregate estimates and available event studies | `outputs/comparison/` |
| `scripts/robustness/01_sample_sensitivity.R` | Omit DF/PB/RN, end in 2010, direct sales only, leave one state out; debt/GDP | `outputs/robustness/sample_sensitivity/` |
| `scripts/robustness/02_regional_comparisons.R` | South/Southeast, Northeast and North/Northeast models and trajectories | `outputs/robustness/regional/` |
| `scripts/robustness/03_treatment_horizons.R` | Immediate and later horizons with truncated and extended trajectory plots | `outputs/robustness/treatment_horizons/` |
| `scripts/robustness/04_same_year_did.R` | Common-event-year DiD and aligned trajectories | `outputs/robustness/same_year_did/` |
| `scripts/robustness/05_alternative_outcomes.R` | Debt/GDP, nominal log debt and annual debt changes | `outputs/robustness/alternative_outcomes/` |
| `scripts/robustness/06_honest_did.R` | HonestDiD sensitivity based on Sun–Abraham coefficients and their full covariance; fixed reference k=-1 and horizons -4 to 8 | `outputs/robustness/honest_did/` |
| `scripts/robustness/07_gdp_validation.R` | Compare overlapping reference-2002 and reference-2010 official GDP releases; never splice series | `outputs/robustness/gdp_validation/` |
| `scripts/exploration/01_bank_importance_matching.R` | December 1996 bank-importance index and economic/combined one-to-one matching | `outputs/exploration/bank_matching/` |
| `scripts/exploration/02_matched_trajectories.R` | Trajectories for the saved matching pairs | `outputs/exploration/matched_trajectories/` |
| `scripts/exploration/03_bank_selection.R` | Credit, deposits, branches and assets used to compare banks within a state | `outputs/exploration/bank_selection/` |
| `scripts/exploration/04_alternative_trajectories.R` | Alternative-outcome trajectories by region and event cohort | `outputs/exploration/alternative_trajectories/` |
| `scripts/exploration/05_hypothetical_scenario.R` | Illustrative retained-state projection from saved Sun–Abraham log-ratio coefficients; run that model first | `outputs/exploration/hypothetical_scenario/` |

Each main estimator writes `estimates.csv`, `event_study.csv`, figures and RDS
objects. Additional estimators expose `estimates.csv` in the same comparison
schema; their detailed paths/weights remain in their own `tables/` subfolder.
The model comparison is descriptive when the methods use different samples or
target populations. An empty confidence interval for synthetic control means
that this implementation does not estimate its uncertainty.
