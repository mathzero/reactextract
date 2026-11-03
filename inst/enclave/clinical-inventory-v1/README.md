# Clinical cohort inventory: enclave run kit (clinical-inventory-v1)

This kit describes every dataset held for the REACT Clinical Study, so that
code, and later synthetic data, can be developed outside the enclave. It reads
the outputs of the canonical wrangling pipeline (`code/revised/` stages 01 and
02 in `react_ge_lc_wrangle`) and the omics and follow-up files that stage 02
reads. Then it writes two things: an **inventory** (what exists, its structure
and size) and a **profile** (distributions, dependencies, timing and omics
structure), both protected for output checking.

It reads files only. It does not connect to the database, change any file it
reads, or write row-level data.

## Before running

- R 4.1 or later. The kit uses base R only.
- To read the two workbook sources (transcript manifests and autoantibodies)
  it needs `openxlsx` or `readxl`. The wrangling pipeline already uses
  openxlsx. Without either, those two sources are reported as not read.
- `bit64` is needed only if a table holds `integer64` columns.
- The revised pipeline must have run (stage 01, then stage 02). The kit reads
  `ge_lc_wrangled.rds`, `combined_data_set.rds`, `ge_lc_sample_lookup.rds` and
  `cohort_availability_sets.rds` from `REACT_DATA_ROOT/metadata`.
- Copy the complete kit folder into the enclave and don't edit its files:
  `00_load.R` checks each one against `kit-checksums.csv`.

## Run

Start a fresh R session and set the working directory to the kit folder.

```r
source("03_self_test.R")   # invented data only, in a temporary folder; about a minute
```

The self-test runs the whole kit on invented data and checks that the
candidate it writes contains no invented identifiers, free text, rare
categories, extreme values or one-person months. Run it once in the enclave
before the real run, to confirm that the kit works with the enclave's R.

```r
Sys.setenv(REACT_GELC_PROJECT = "D:/path/to/react_ge_lc_wrangle")  # holds subject_ids_main_cohort.rds
source("01_run_inventory.R")    # reads each source in turn; aggregates go to clinical-inventory-INTERNAL
source("02_prepare_export.R")   # writes clinical-inventory-v1-candidate
```

`REACT_SAVED_ROOT` (default `D:/saved_objects`) and `REACT_DATA_ROOT` (default
`//se-sandisk-vm01/react_data03$`) have the same defaults as the pipeline. If
`REACT_GELC_PROJECT` is not set, the main cohort is rebuilt from
`cohort_availability_sets.rds`, and `01_context.csv` records which route was used.

Each source is saved as soon as it is done. If the run stops, start a new
session and run `01_run_inventory.R` again: sources whose input files have not
changed (same size and modification time) are reused. The expression matrices
(`tx_baseline.rds`, `tx_recall.rds`) need the most memory: about twice the size
of the loaded file, since R reads each RDS whole. They are read one at a time,
and only genes non-zero in at least 10% of samples are profiled. On invented
data at cohort scale (11,000 people, four 280-column survey tables, 30,000
genes by 3,000 samples), the whole run took under a minute on a laptop, with
peak memory of about 6.5 GB.

## What it reads

| Source | File |
|---|---|
| `wrangled` | `REACT_DATA_ROOT/metadata/ge_lc_wrangled.rds`: every table in the nested list |
| `master` | `metadata/combined_data_set.rds`, including the availability flags |
| `sample_lookup`, `availability_sets`, `provenance` | Stage 01–02 support files in `metadata/` |
| `proteomics` | `REACT_SAVED_ROOT/ge_lc/ge_lc_omics_objects/olink_alamar_lcms_combined.rds` (Olink, Alamar, LC-MS) |
| `metabolomics_rpos`, `metabolomics_rneg` | PeakPantheR `combinedData.csv` files (2024-12-13 release) |
| `lipids` | `Signaling-Lipids-Analysis/lipids_with_subject_id.rds`, written by stage 02 |
| `transcriptomics_t0`, `_t1` | `tx_baseline.rds`, `tx_recall.rds` (tximport lists) |
| `transcriptomics_manifests` | `transcriptomics/manifests/*.xlsx` |
| `autoantibodies` | `autoantibodies/REACT LC Aab data.xlsx`, sheets 4 (controls) and 5 (cases) |
| `genomics_samples` | `genomics/REACT_PGEN/*.psam` and the sample-well lookup (sample lists only) |
| `followup_2022`, `followup_2025` | Later questionnaires, cohort members only |
| `cognitron_1`, `cognitron_2` | Cognitron files in `cog_data/`, cohort members only |
| `nhs_hes`, `nhs_gp` | `gelc_hes_data.rds`, `gelc_gpes_data.rds`, cohort members only |
| `linkage_ids`, `subject_identifiers`, `invites` | Linkage ID sets, the identifier crosswalk, invitations |

`sources.R` lists the exact paths. The kit also lists the data folders (names,
file counts and sizes, with long digit runs masked) to find datasets that the
pipeline doesn't read, and records the R version and installed packages.

## What comes back

`clinical-inventory-v1-candidate/` holds:

- `inventory/`: sources and their status, the nested structure, every table
  and column (type, role, completeness), the folder listing, the R environment
  and pipeline provenance. The output checker can review and release this
  folder on its own.
- `profile/`: quantiles of numeric columns, overall and by anchor (sex, age
  group, long COVID group, registration route, study, ethnicity, main-cohort
  membership); category and month counts; ID shapes such as `AAA9999999`;
  data availability and its patterns; rank correlations within tables and
  across clinic and blood tables; baseline-to-follow-up stability; intervals
  between key dates; and, for each omics platform, feature quantiles,
  missingness, detection floors, low-rank structure, batch and anchor R², and
  visit stability.

Request output checking for the candidate folder. **Never transfer
`clinical-inventory-INTERNAL`**: it holds unsuppressed aggregates.
`REVIEWER.md` explains the protections for the output checker.

## What it does not do

- It queries no database views. The wiki's Stage 2 kit profiles the raw
  registration and survey views; this kit profiles the wrangled release.
- It doesn't count NHS outcome codes. Outcome definitions and their counts
  belong to reactAtlas (react-hub decision H10). For linked records, the kit
  reports only structure, record dates and records per person.
- It reads no genotypes. Whole-genome data are accessed through Genomics
  England; the kit lists samples only.
- It doesn't generate synthetic data. Its checked output is the input for the
  Clinical Study generator planned in reactextract.
