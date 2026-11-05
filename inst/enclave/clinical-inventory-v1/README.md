# Clinical cohort inventory: enclave run kit (clinical-inventory-v1.2)

This kit describes every dataset held for the REACT Clinical Study, so that
code, and later synthetic data, can be developed outside the enclave. It reads
the outputs of the canonical wrangling pipeline (`code/revised/` stages 01 and
02 in `react_ge_lc_wrangle`) and the omics and follow-up files that stage 02
reads. Then it writes two things: an **inventory** (what exists, its structure
and size) and a **profile** (distributions, dependencies, timing and omics
structure), both protected for output checking.

It reads files only. It does not connect to the database, change any file it
reads, or write row-level data.

## Version 1.2: one full run

v1.2 profiles every source afresh and writes one candidate,
`clinical-inventory-v1.2-candidate`. Once released, it replaces both earlier
candidates: v1's (2 November 2026, 16 of 23 sources) and the v1.1 supplement
(3 November 2026, the seven sources that had failed plus XCMS and APOE4).
v1.1 fixed the failures:

- Windows-1252 free text is converted on reading, with the count of converted
  values reported per column. Each column, table and cross-table step runs on
  its own, so an error in one is recorded in `00_source_issues.csv` and the
  rest still run (status `partial`).
- `sources.R` declares each proteomics table's feature, value and sample
  columns. Raw and normalised values are profiled as separate platforms, and
  `df_allplats` per platform.
- CSV column names are repaired as `read_csv()` repairs them (`...1`).
- RNA-seq columns (BioSample names) are linked to sample barcodes through the
  manifests.
- The folder listing gives each collapsed folder its own count, `evbirth` and
  `numbirth` are profiled, and dates stored as text are profiled as dates.

v1.2 adds:

- **Placeholder dates by range.** Any date up to 1900-01-01, or in the year
  9999, counts as a placeholder and is left out of date quantiles. v1.1
  listed exact days, and the enclave's SPSS zero dates fell a day away from
  1582-10-14, so none were caught; nor were 0001-01-01 and HES's 1801-01-01.
- **Lookup and QC tables.** The proteomics list's `prot_names_lookup`,
  `prot_summary`, `olink_missing_summary`, `alamar_detectability` and
  `qc_log` are profiled as ordinary tables.
- **Column order of long omics tables**, in `inventory/04b_source_columns.csv`,
  so that synthetic tables can match it.
- **No reuse across versions.** A checkpoint is reused only if this kit
  version wrote it, so a v1.2 run never mixes in v1 or v1.1 results.
  `02_prepare_export.R` stops if any source was profiled by an earlier
  version.

A full run also applies v1.1's text-date fix to sources the supplement didn't
rerun: `cognitron_1`'s `test_date` and `followup_2025`'s `_DATE_OTHER` columns.

## Before running

- R 4.1 or later. The kit uses base R only.
- To read the two workbook sources (transcript manifests and autoantibodies)
  it needs `openxlsx` or `readxl`. The wrangling pipeline already uses
  openxlsx. Without either, those two sources are reported as not read.
- `bit64` is needed only if a table holds `integer64` columns.
- The revised pipeline must have run (stage 01, then stage 02). The kit reads
  `ge_lc_wrangled.rds`, `combined_data_set.rds`, `ge_lc_sample_lookup.rds` and
  `cohort_availability_sets.rds` from `REACT_DATA_ROOT/metadata`.
- Unzip the kit into a new, empty folder in the enclave (for example under
  `D:/projects`), not into an earlier kit's folder, and don't edit its files:
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
source("01_run_inventory.R")    # profiles every source; aggregates go to clinical-inventory-INTERNAL
source("02_prepare_export.R")   # writes clinical-inventory-v1.2-candidate
```

`REACT_SAVED_ROOT` (default `D:/saved_objects`) and `REACT_DATA_ROOT` (default
`//se-sandisk-vm01/react_data03$`) have the same defaults as the pipeline. If
`REACT_GELC_PROJECT` is not set, the main cohort is rebuilt from
`cohort_availability_sets.rds`, and `01_context.csv` records which route was used.

Each source is saved as soon as it is done. If the run stops, start a new
session and source `01_run_inventory.R` again: a source is reused when this
kit version profiled it, its input files have not changed (same size and
modification time) and its last run finished. Sources that failed, wholly or
in part, are profiled again. The first full run took about two hours, 76
minutes of it for `linkage_ids`; expect about the same. The expression matrices
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
| `metabolomics_xcms` | `metabolomics/metabolomics_lc_ms/XCMS_metabolomics_intensity_data_2_noSampleID_CL.csv` (no sample IDs) |
| `lipids` | `Signaling-Lipids-Analysis/lipids_with_subject_id.rds`, written by stage 02 |
| `transcriptomics_t0`, `_t1` | `tx_baseline.rds`, `tx_recall.rds` (tximport lists) |
| `transcriptomics_manifests` | `transcriptomics/manifests/*.xlsx` |
| `autoantibodies` | `autoantibodies/REACT LC Aab data.xlsx`, sheets 4 (controls) and 5 (cases) |
| `genomics_samples` | `genomics/REACT_PGEN/*.psam` and the sample-well lookup (sample lists only) |
| `apoe4` | `genomics/APOE4/apoe4_carrier.csv`, linked through the sample-well lookup, cohort members only |
| `followup_2022`, `followup_2025` | Later questionnaires, cohort members only |
| `cognitron_1`, `cognitron_2` | Cognitron files in `cog_data/`, cohort members only |
| `nhs_hes`, `nhs_gp` | `gelc_hes_data.rds`, `gelc_gpes_data.rds`, cohort members only |
| `linkage_ids`, `subject_identifiers`, `invites` | Linkage ID sets, the identifier crosswalk, invitations |

`sources.R` lists the exact paths. The kit also lists the data folders (names,
file counts and sizes, with long digit runs masked) to find datasets that the
pipeline doesn't read, and records the R version and installed packages.

## What comes back

The candidate folder (`clinical-inventory-v1.2-candidate/`) holds:

- `inventory/`: sources and their status, steps that failed, the nested
  structure, every table and column (type, role, completeness), the column
  order of long omics tables, the folder listing, the R environment and
  pipeline provenance. The output checker can review and release this
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
  Clinical Study generator in reactextract (in development).
