# State-bank privatization and debt trajectories of Brazilian states

Semester project — Foundations in Econometrics and Machine Learning, HEC Lausanne.

**Team:** Marina Canals Jiménez, Brigid Halliday, Arthur Kafrouni Tomazi,
Alex Prem Martin, Julian Van Der Kwaak and Mona Vesala.

## Research question

Did Brazilian states that lost control of their commercial state bank follow different
public-debt trajectories from states that retained one? We examine financial debt/GDP,
debt owed to the Federal Government/GDP, nominal debt and annual debt changes.
Possible mechanisms include debt relief, changes in access to bank financing and
the transfer of liabilities to the Federal Government. These are hypotheses, not
established causal conclusions.

## Structure

```text
config/                 Analysis settings, outcomes and exact download URLs
data/metadata/          Versioned event dictionary and manually curated bank mappings
data/raw/               Downloaded source files and checksum manifest (not versioned)
data/processed/         Rebuilt analysis panel (not versioned)
R/                      Shared package setup and sample construction
scripts/                Preparation, EDA, comparison and pipeline runner
  models/               One script for each estimator
  robustness/           One script for each robustness exercise
  exploration/          Bank importance, matching and descriptive trajectories
outputs/                Generated tables, figures and fitted models (not versioned)
docs/                   Sources, methods, script guide and event-selection reference
```

## Run the project

Open `state-banks-debt.Rproj` in RStudio, or set the working directory to this folder.
Install the R dependencies once:

```r
source("scripts/00_install_packages.R")
```

Run the default pipeline:

```r
source("scripts/run_all.R")
```

Or run individual stages:

```r
source("scripts/01_prepare_data.R")
source("scripts/02_eda.R")
source("scripts/models/01_twfe.R")
source("scripts/models/02_sun_abraham.R")
source("scripts/03_compare_models.R")
```

Scripts use cached inputs when present and download missing files. Set
`refresh_downloads = TRUE` in `config/settings.R` to download all core inputs again.
The manual event dictionary is versioned, not scraped or overwritten by downloads.
Its sources and historical asset transcription are documented in `docs/data_sources.md`.
Internet access is required for installation and missing downloads. A local
`.R-library/` holds packages and is excluded from Git.

## Parameters and comparisons

Edit **`config/settings.R`**, then rerun preparation and the affected analyses.
The main defaults are 1996–2012, all eligible states including DF, three outcomes,
treatment in the first full calendar year after the event, and state-clustered errors.
`config/outcomes.csv` defines the display units and the additional outcome names.
`show_plots` controls display in RStudio; figures are saved in either case.

TWFE and Sun–Abraham are estimated in separate scripts. The comparison script
**reads their saved tables without estimating either model**. Choose methods in
`comparison_models` and outcomes in `comparison_outcomes`. Available method IDs:
`twfe`, `sun_abraham`, `classic_did`, `synthetic_control`, `synthetic_did`.
Run each selected model first. By default, different sample/timing identifiers
stop the comparison. Set `comparison_allow_different_samples = TRUE` to compare
deliberately different targets; this does not make their estimands equivalent.
Synthetic control has no confidence interval from the current implementation.

Set `run_extensions = TRUE` to include every additional exercise in the runner,
or run the specific file listed in `docs/scripts.md`. Historical extensions use
`extension_exclude_states = "DF"`; this is explicit and separate from the main
model setting `exclude_states = character()`. Edit both if a common sample is needed.
Some extensions keep the originally specified outcome or time window; their
scope is described in the script guide.

## Data and interpretation

The complete panel contains **27 units × 17 years = 459 observations**. The
default model comparison uses 20 loss-of-control states and five retained-bank
states (DF, ES, PA, RS, SE): 425 observations before outcome-specific missingness.
MS and TO remain in the complete panel but are ineligible for the event models.
The main period is the overlap of the Treasury debt data and the official
IBGE reference-2002 GDP series, which ends in 2012.

The event dictionary retains the project's existing dates, including BANESPA
(SP, 1997) and BEMGE (MG, 1998). RN/AP liquidation timing and the interpretation
of different liquidation stages remain issues for review. Reorganization does
not resolve these historical choices. See `docs/methodology.md` and the detailed
event-selection document.

There are few control states, limited pre-event histories for early exits,
concurrent fiscal reforms and potentially heterogeneous treatment effects.
TWFE is the working baseline; Sun–Abraham addresses staggered timing and
heterogeneous dynamic effects but still requires identifying assumptions.
Matching and descriptive trajectories do not establish causality.

## Next research steps

- Review the liquidation stages selected for AP and RN and document alternatives.
- Check sensitivity to state composition, regions, horizons and outcome definitions.
- Compare the TWFE baseline with Sun–Abraham under matching samples and timing.
- Assess pre-event support, bank-importance proxies and the suitability of matched states.

## Reproducibility and GitHub

Source code, configuration and curated metadata belong in Git. Downloaded data,
model objects, package libraries, session files and generated outputs are
excluded by `.gitignore` and can be recreated. Exact URLs are recorded in
`config/data_sources.csv`; preparation writes file checksums to
`data/raw/source_manifest.csv`. Package versions used for verification are in
`docs/package_versions.csv`. No repository has been published or pushed.
