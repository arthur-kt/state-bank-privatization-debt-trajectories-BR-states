# Definitions and analysis choices

## Outcomes and units

`debt_to_gdp = financial_debt_brl / state_gdp_nominal_brl` and
`union_debt_to_gdp = union_debt_brl / state_gdp_nominal_brl` are ratios, not
percentages. Multiply by 100 for display in percentage points. Nominal stocks
and GDP are current BRL; their levels are not inflation-adjusted.
`log_financial_debt = log(financial_debt_brl)` and
`log_debt_to_gdp = log(debt_to_gdp)` are distinct outcomes.
Annual log debt growth is `100 * (log(debt[t]) - log(debt[t-1]))`;
annual debt/GDP change is `100 * (ratio[t] - ratio[t-1])`.

## Groups and dates

`retained_bank` equals one for DF, ES, PA, RS and SE and zero otherwise.
`loss_of_control` equals one for the 20 selected exits. MS and TO have both
indicators equal to zero: they are retained in the complete panel but are not
assigned a treatment year or used as event-model controls.

Let E be the selected institutional event year and G = E + treatment_lag
(default one). `post_loss_control = 1` for an exit state when year >= G.
Internal event time is k = year - G. Thus k=-1 is the institutional event year
under the default lag; k=0 is its first full subsequent calendar year. Main
model figures use h = k + treatment_lag = year - E so the event appears at zero.
Retained states have no historical event. For descriptive cohort/pair plots,
their observations are aligned to the treated comparison's calendar year,
not to an invented event in the retained state.

SP follows BANESPA's federalization in 1997, although its private sale occurred
in 2000 and Nossa Caixa remained under state control until its later transfer.
MG follows BEMGE's 1998 sale rather than CREDIREAL's 1997 sale. Selection uses
credit, deposits and branch networks as well as assets. The bank-selection
exploration preserves the source calculations behind this choice.

The dictionary's liquidation-stage choices remain unresolved historical
limitations. AP currently uses its later ordinary-liquidation stage in 1999,
following extrajudicial liquidation in 1997. RN currently uses the 2000
ordinary-liquidation stage, following the earlier extrajudicial liquidations
of BANDERN (1990) and BDRN (1994). A later procedural change does not imply
resumed operations. MT and RO also require care when comparing liquidation
stages and special administration. Consult the detailed reference document
before changing any date; rerun preparation and all analyses after a change.

## TWFE and Sun–Abraham

The static baseline is Y_it = alpha_i + lambda_t + beta D_it + epsilon_it,
with state and year fixed effects and state-clustered standard errors.
The dynamic TWFE replaces D_it with interactions of exit status and relative
year, omitting k=-1 by default. With staggered treatment and heterogeneous
effects, TWFE coefficients need not recover a simple average treatment effect.

Sun–Abraham estimates cohort × relative-year interactions with the same fixed
effects. `fixest::sunab()` aggregates supported interactions into dynamic
coefficients and an overall ATT. Never-treated retained states supply the
comparison group. The main comparison keeps the same sample, outcome, timing
and reference period for both methods. This does not remove concerns about
parallel trends, anticipation, concurrent reforms or few clusters.

## Bank importance and matching

The 1996 bank-importance index averages the percentile ranks of assets/GDP,
credit/GDP, deposits/GDP and branches per BRL billion of state GDP. It lies
between zero and one. Measures refer to the bank's whole reported operations,
not only activity located inside its home state. This is a proxy for bank
importance relative to the state economy, not a direct measure of local
borrowing dependence. Missing bank records remain missing.

For each available matching variable x, z_i = (x_i - mean(x)) / sd(x) over
the eligible state profiles. Economic distance is the Euclidean distance
using log GDP, financial debt/GDP and federal debt/GDP in 1996. Combined
distance adds the bank-importance index. The procedure selects five distinct
exit-state partners for the five retained states by minimizing total distance,
without replacement. It requires the complete five-control profile used in
the current exercise; it is separate from the configurable model sample.
A 1996 snapshot is not an unaffected baseline if the relevant bank exit
actually occurred before 1996. Review event timing before causal matching.

## Extensions

Common-year DiD, alternative outcomes, horizon restrictions, regional subsets,
synthetic control, Synthetic DiD and HonestDiD are separate exercises. Their
populations, reference windows and targets are not interchangeable. The
retained-state hypothetical projection is an illustrative extrapolation of
saved log-ratio coefficients, not an identified counterfactual for those states.
