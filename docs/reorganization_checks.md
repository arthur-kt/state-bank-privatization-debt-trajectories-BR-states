# Reorganization verification — 17 November 2026

- The preparation script ran from an empty raw-data cache and downloaded all five
  core inputs: Treasury debt, two IBGE ZIPs and two IF.data reports.
- The rebuilt panel matches the previous 459 state-year observations and all 30
  columns, including the financial and federal debt variables and event dictionary.
- TWFE and Sun–Abraham were rerun separately for all three default outcomes.
  Their point estimates and standard errors match the previous exports at
  numerical tolerance 1e-10.
- The matching-pairs CSV has the same SHA-256 checksum as the previous version:
  `A3084319B123B7FB6A7ECA9ADCC1E8CD7C20B0E95FF7C4A3397195716C82A0FF`.
- The complete core runner finished successfully. All additional model and
  robustness scripts also executed with their reorganized input/output paths.
  Matched and alternative-outcome trajectories and the hypothetical scenario ran.
- An isolated configuration test changed the reference period to -2, excluded DF
  and selected annual debt/GDP changes. Both models and their comparison ran.
- Another isolated test compared saved classic DiD, synthetic-control and
  Synthetic DiD estimates, with different targets explicitly allowed.
- All 22 executable R scripts parsed successfully. The installer was inspected;
  installation was not repeated because the required packages were available.

These checks concern reproducibility of the reorganization. They do not validate
the causal assumptions or resolve the pending historical event classifications.
Local package versions are recorded in `package_versions.csv`; dependencies are
recorded rather than fully locked, except for the pinned `synthdid` Git revision.
