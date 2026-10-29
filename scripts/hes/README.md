# Building the HES profile

`inst/extdata/hes-profile.rds` is the profile behind synthetic
`react_hes()`. It is built from public NHS England files and from aggregate
REACT outputs. No participant-level data are used, and the REACT aggregates
stay on the developer's machine: only the built profile is committed.

```sh
sh scripts/hes/00_download_sources.sh   # public files into scripts/hes/sources/ (git-ignored)
REACT_HES_AGGREGATES=/path/to/schema_profile \
  Rscript --no-init-file scripts/hes/01_build_hes_profile.R
# pin the printed SHA-256 as .hes_profile_sha256 in R/hes_profile.R and
# record it in inst/extdata/hes-profile-approval.csv
REACT_HES_AGGREGATES=/path/to/schema_profile \
  Rscript --no-init-file scripts/hes/02_validate_hes.R   # writes scripts/hes/VALIDATION.md
```

`--no-init-file` skips the project's renv profile; the scripts need
`pkgload` from your user library.

## Public sources (Open Government Licence v3)

| File | What it gives | Source |
|---|---|---|
| `artificial_hes_apc_202302_v1_sample.zip` | Value frequencies for admitted-care columns REACT shares with the artificial data, and secondary-diagnosis frequencies | [NHS England artificial data](https://digital.nhs.uk/services/artificial-data), Feb 2023 v1, 10,000 rows per financial year |
| `artificial_hes_op_202302_v1_sample.zip` | The same for outpatients, and outpatient procedure frequencies | as above |
| `hosp-epis-stat-admi-diag-2023-24.csv` | Primary diagnoses (4-character ICD-10) by age band, sex, admission type, day cases and length of stay | [Hospital Admitted Patient Care Activity 2023-24](https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity/2023-24), open data |
| `hosp-epis-stat-admi-proc-2023-24.csv` | Main procedures (4-character OPCS-4) on the same breakdowns | as above |

The artificial data generate each column independently from national
frequencies, so they supply formats and code lists, not relationships. Only
financial years 2015-16 onwards are used. SHA-256 checksums are in
`00_download_sources.sh`. The 2023-24 files were downloaded on 29 October
2026, after the publisher's correction of 3 October 2024.

## REACT aggregates (`REACT_HES_AGGREGATES`, local only)

The schema profile of REACT's linked NHS tables produced inside the enclave by
`vaccine_exposure_expotypes/R/01_profile_source_tables.R` (run started 22
October 2026) and returned through output checking. Its disclosure rules:
counts of 1-9 suppressed and other counts rounded to the nearest 5; identifier,
geography and date-of-birth columns summarised by completeness and character
patterns only; value tables only for low-cardinality columns. The build treats
suppressed counts as zero and never reconstructs them. The profile records the
SHA-256 of every input file.

| Used for | Files |
|---|---|
| Column names, order and Oracle types | `dictionary_columns.csv` |
| Whole-table rows and people (Oracle optimiser statistics) | `dictionary_tables.csv`, `dictionary_columns.csv` |
| Calendar trend (1% block sample) | `date_distributions.csv` |
| Value frequencies, completeness, patterns and quantiles of other columns | `value_frequencies.csv`, `column_summary.csv`, `value_patterns.csv`, `numeric_quantiles.csv` |
| COVID-19 admissions by month and diagnosis position | `hes_code_by_period.csv`, `hes_code_positions.csv` |

The linked population is approximated by the number of people with any GDPPR
record (2,874,625). Whole-table counts are optimiser statistics, so treat the
volumes as approximate.

## What the profile contains

- column layouts and Oracle types;
- value frequencies from rounded counts of at least 10, and completeness
  shares;
- 5th to 95th percentiles of numeric columns;
- monthly COVID-19 admission counts (rounded, at least 10);
- national HES 2023-24 counts by age band and sex, and artificial-HES value
  frequencies;
- the fitted rate parameters below.

No records, identifiers or small counts.

## Assumptions in the build

- Admission and appointment rates follow a cubic in age with a sex term and an
  extra term for women aged 18-44, multiplied by a calendar factor and a
  gamma-distributed personal frailty. Parameters are fitted so this package's
  synthetic participants reproduce REACT's records per person, share with any
  record, age percentiles and (admitted care) female share.
- Calendar factors are yearly, from the 1% block sample after removing the
  effect of the cohort ageing. 2020 keeps its monthly shape (the first wave).
  Finer monthly detail in the block sample reflects how blocks were sampled,
  so it is not used.
- Outpatients reuse admitted care's sex terms (REACT's outpatient profile has
  no sex breakdown).
- In-hospital death risk doubles every eight years of age up to 95, scaled to
  REACT's share of episodes ending in death.
- Admission types and day cases are rescaled from England's mix to REACT's.
