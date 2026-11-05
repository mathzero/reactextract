# Synthetic Clinical Study validation

Generated 05 November 2026 with reactextract 0.6.0.9011 (clinical-profile-v1, built from clinical-inventory-v1.2) for the synthetic people of `react_synthetic(n_per_round = 1000L, seed = 20261105)`. The wrangled list (31 tables) took 46 seconds; the omics files (n = 3000, RNA-seq left out) 41 seconds.

Every figure compares synthetic output with what the profile itself holds. **Synthetic data are for writing code, not for findings.**

## Structure

- Tables whose column names and order match the inventory: 31 of 31.
- Tables whose column classes match: 31 of 31.
- Tables whose row count differs from the profile's by more than 5%: 1 (infection_dates: population tables cover the synthetic population, and tables with more people than registered are capped).

## Distributions

| Measure | Columns | Median | 90th percentile | Share above 0.10 |
|---|---|---|---|---|
| Categorical: total variation distance from the profile's shares | 3131 | 0.007 | 0.037 | 0.020 |
| Continuous: largest p10/p50/p90 error, as a share of the profile's p10-p90 range | 312 | 0.000 | 0.077 | 0.074 |
| Missingness: absolute difference in share missing | 4300 | 0.001 | 0.006 | 0.012 |

Categorical values outside the profile's levels: 0.0000 of answered values (anchor columns hold the person's anchor label).

Largest categorical differences outside population tables: `registration_data$symptoms_duration_29` (0.20), `health_survey/survey_t1$lc_current_exclude_notsures` (0.19), `health_survey/survey_t1_raw$sympttime_11` (0.18), `health_survey/survey_t1_raw$sympttime_12` (0.18), `health_survey/survey_t1_raw$lc_current_exclude_notsures` (0.18), `health_survey/survey_t1_raw$covrecov` (0.18), `registration_data$registered_under` (0.18), `health_survey/survey_t1_raw$sympttime_8` (0.18). Anchored columns move with the cohort's anchor mix, and columns with few answers are noisy. Population tables (infection_dates) cover the whole synthetic population, in which the cohort is a far larger share than in REACT, so their overall shares lean towards registrants'.

## Dependencies

| Measure | Pairs | Mean absolute difference | Correlation with the profile |
|---|---|---|---|
| Within-table rank correlations (sample of 2000) | 1705 | 0.037 | 0.918 |
| Baseline to follow-up rank correlations | 347 | 0.183 | 0.866 |

Intervals between dates in days (p10 / p50 / p90):

| Interval | Profile | Synthetic |
|---|---|---|
| onset_to_registration | 125 / 309 / 445 | 125 / 310 / 444 |
| registration_to_baseline_clinic | 6 / 16 / 34 | 6 / 16 / 34 |
| onset_to_baseline_blood | 138 / 331 / 444 | 139 / 328 / 446 |
| baseline_to_followup_clinic | 136 / 193 / 280 | 134 / 192 / 281 |
| baseline_to_followup_blood | 136 / 193 / 280 | 135 / 192 / 281 |
| onset_to_followup_blood | 323 / 577 / 622 | 330 / 521 / 671 |

## Omics (wide tables)

| Platform | Samples | Features | First component's share: profile | synthetic | Median feature median error |
|---|---|---|---|---|---|
| lcms | 2731 | 298 | 0.074 | 0.077 | 0.006 |
| metabolomics_rpos | 2736 | 83 | 0.225 | 0.236 | 0.008 |
| metabolomics_rneg | 2731 | 89 | 0.204 | 0.205 | 0.005 |
| metabolomics_xcms | 2729 | 2209 | 0.110 | 0.110 | 0.005 |
| lipids | 461 | 132 | 0.174 | 0.169 | 0.016 |

Feature median error is the absolute difference from the profile's median, as a share of its p10-p90 range.
