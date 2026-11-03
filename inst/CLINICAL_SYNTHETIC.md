# Synthetic REACT Clinical Study data (draft design)

Status: draft, 3 November 2026. The generator is a scaffold. It runs on a
local inventory candidate or on invented fixtures. No profile from REACT
aggregates is bundled yet.

`react_clinical()` returns the REACT Clinical Study's wrangled release: the
nested list that the canonical wrangling pipeline saves as
`ge_lc_wrangled.rds`. As with `react_extract()` and `react_hes()`, only the
source changes:

```r
profile <- react_clinical_profile()          # path from REACTEXTRACT_CLINICAL_PROFILE
source  <- react_synthetic(seed = 1L)
gelc    <- react_clinical(source, profile = profile)

gelc$registration_data
gelc$clinic_data$clinical_tests
```

In the enclave, the same code reads the real list:

```r
source <- react_wrangled(file.path(Sys.getenv("REACT_DATA_ROOT"), "metadata", "ge_lc_wrangled.rds"))
gelc   <- react_clinical(source)
```

Given a profile as well, `react_clinical()` checks the real tables against its
inventory and warns about any table whose column names, order or classes
differ, which is how drift between the synthetic and real structure shows up.

> **Synthetic data are for learning and writing code, not for findings.**
> Never present results computed on them as REACT results.

## Decisions this design follows

| Decision | Choice (3 November 2026) |
|---|---|
| Hub H08 | Reproduce the analysis-ready layer: table and column names, order, types, codings and keys |
| Hub H09 | The generator lives in reactextract behind the source interface. A profile from REACT aggregates is bundled only when it has no counts below 10, is checksum-pinned and has an approval record. Suppressed counts are treated as zero |
| Hub H13 | react_twin uses this generator and doesn't copy it |
| Open decision 1 | Students load the **wrangled list as it is**. If a flat release is built later, it is a reactextract function that runs on the real and the synthetic list alike, so this generator doesn't change |
| Open decision 2 | The cohort **links to reactextract's synthetic people**: it is drawn from the people of a `react_synthetic()` source, so linked synthetic records can be generated for the same people |
| Open decision 4 | The Clinical Study comes **before** further NHS linkage (GP records) for term 2 |

## Where the profile comes from

The input is the checked output of the enclave inventory kit
(`clinical-inventory-v1`, first run 2 November 2026). It has an `inventory/`
folder (structure) and a `profile/` folder (distributions). The kit's
`README.md` and `REVIEWER.md` explain each file.

Until a profile derived from it is approved and checksum-pinned, the package
bundles nothing from it:

- `react_clinical_profile()` reads a candidate folder from a local path, by
  default the environment variable `REACTEXTRACT_CLINICAL_PROFILE`;
- a v1.1 supplement candidate can be layered on top
  (`REACTEXTRACT_CLINICAL_SUPPLEMENT`): for each source the supplement lists,
  its rows replace that source's rows in the first candidate, as the kit
  specifies;
- the package's tests use a small invented candidate
  (`tests/testthat/fixtures/clinical-candidate/`, written by
  `make-clinical-candidate.R` beside it). It copies the kit's file layout and
  disclosure rules, not REACT content: table paths, column names and values
  are invented;
- nothing from the candidate, including its column names and counts, is
  committed. The profile object records the folder path and the candidate's
  manifest checksums, and is marked `local_candidate`.

Approval later follows the HES route: derive a compact profile from the
candidate, check that it has no counts below 10, write it to
`inst/extdata/clinical-profile.rds` with an approval CSV, and pin its SHA-256
in the code. `react_clinical_profile()` then defaults to the bundled profile,
and the environment variable becomes a development override.

## Which tables and columns are generated

Every table of the wrangled list that `inventory/02_structure.csv` names is
generated, at the same path in the nested list. The public description of
the list (react-hub `docs/datasets.md`) is:

- `registration_data`: the registration survey, one row per registrant;
- `clinic_data`: `clinical_tests`, `assay` and `illumina`, one table for each
  visit (baseline T0, follow-up T1);
- `health_survey`: the health questionnaire for each visit, both cleaned and
  raw, plus its data dictionary;
- `original_react_data`: the cohort members' REACT-1/2 survey records.

For each table, `03_tables.csv` gives the number of rows and people, how many
of them are in the main cohort, the visit and the distribution of rows per
person. `04_columns.csv` gives each column's position, class, role and
completeness. The generator copies the column names, order and classes
(`numeric`, `integer`, `character`, `logical`, `Date`, `POSIXct`). Tables
whose real class is a tibble get the tibble class vector (no tibble package
needed). The one `data.table` table becomes a plain `data.frame`, since a
`data.table` can't be made correctly in base R; code that relies on
`data.table` syntax should call `data.table::setDT()` first.

How each column's values are made depends on its role in `04_columns.csv`:

| Role | Synthetic values |
|---|---|
| `identifier` | The person key (`subject_id`) comes from the linked synthetic person. Other identifiers take a character shape from `14_id_shapes.csv` (letters to `A`, digits to `9`), filled at random and unique per person; one person keeps the same value in every table |
| `categorical` | Released levels and counts from `11_categorical.csv` |
| `coded_numeric` | Levels from `11_categorical.csv` where present, otherwise as `continuous` |
| `continuous` | Quantiles from `10_numeric.csv`, with its decimals, share of whole numbers and missing codes (`-77`, `-91`, `-92`, `-99`, `-66`, `-555`, `-999`) |
| `date` | Month counts from `13_date_months.csv` (or the month quantiles in `12_date_summary.csv`), with a uniform day; `POSIXct` columns also get a uniform time of day, in UTC (to confirm against the real list) |
| `high_cardinality` | Shapes from `14_id_shapes.csv` where present, otherwise placeholder values such as `column_017` |
| `free_text` | The placeholder `"Synthetic free text"`, at the column's completeness, so code that tests for an answer still runs |
| `empty` | Missing throughout |
| `withheld` | Missing throughout. Withheld columns (such as dates of birth) are never generated |

Each column is missing at the rate `04_columns.csv` gives. A blank count
there is suppressed and counts as zero.

## People, anchors and table membership

**People.** With `react_synthetic()`, the generator extracts the source's
synthetic people (`people-households/demographics`) and draws the cohort
from adults with a recorded sex, weighted so that age group and sex follow
the registration table's anchored counts. Each clinical `subject_id` is a
whole number made from the person's reactextract `SUBJECT_ID`:
`100000000 + 1000000 × round index + person number`, so it has the real
key's type (double, nine digits) and the link can be reversed. `people` can
instead be supplied as a data frame (`SUBJECT_ID`, `sex`, `age`) or an
extraction result. Drawing only people who had a positive test, as the real
invitation did, is a next step.

**Anchors.** The kit profiles by seven anchors, named in
`inventory/01_context.csv`: sex, age group, long COVID group, registration
route, study, ethnicity and main-cohort membership. Each synthetic person gets
all seven first:

1. sex and age group from the linked person;
2. main-cohort membership for the right number of registrants;
3. long COVID group, route, study and ethnicity drawn from the registration
   table's counts of that anchor by age group and by sex.

The anchor columns in every table hold the person's anchor values, so they
agree across tables.

**Membership.** Each table takes its number of main-cohort people and of
other registrants from `03_tables.csv`. Within a visit, people are ranked
once, and a table with fewer people takes the top of the ranking. Tables of
the same visit are therefore nested: everyone with a health survey also has
clinic data, as in the real availability sets (`inventory/08_id_sets.csv`).
The follow-up (T1) group is drawn from the main cohort, weighted by long
COVID group so that its mix matches the T1 tables' anchored counts (the real
follow-up was enriched for persistent symptoms). Tables with several rows per
person follow `03_tables.csv`'s rows-per-person bands. Tables without a person
key keep their row count. `n` scales every person count; tables without
people are not scaled.

## Dependencies

Fidelity goes on what analyses rely on (synthetic data plan, principle 4).

**On the anchors.** For each column, the anchored rows of `10_numeric.csv` and
`11_categorical.csv` give its distribution within each level of each anchor,
one anchor at a time. The generator combines them:

- categorical columns: the level probabilities for a person multiply the
  overall probabilities by the ratio of each anchor's within-level
  probability to the overall one (a log-linear model with no interactions).
  Within-level counts are shrunk towards the overall distribution with a
  pseudo-count of 5, so a suppressed cell lowers a level's probability but
  doesn't remove it within that anchor level;
- numeric columns: the person's quantile function is the overall one plus
  each anchor's shift from it at the same probability, then clamped to the
  overall range.

Anchors whose rows are missing, or whose quantiles are suppressed for a
small group, are skipped for that column.

**Within tables (`18_correlations.csv`).** Columns of one table that have
rank correlations are drawn from a Gaussian copula: Spearman's rho becomes a
Pearson correlation (`2 sin(pi rho / 6)`), the matrix is made positive
definite by clipping eigenvalues, and each column's normal score sets the
probability at which its anchor-conditional distribution is read.
Categorical columns read it through their level order. Columns without
correlations get independent probabilities.

**Waiting for v1.1.** The first run's cross-table steps stopped with the
`wrangled` source, so these files are empty in v1:

| File | Use once released |
|---|---|
| `15_availability.csv`–`17_availability_pairs.csv` | Availability flags and their joint patterns, replacing the nested-ranking rule above |
| `18_correlations.csv` (across clinic and blood tables) | One copula across a person's clinic and blood tables, sharing normal scores |
| `19_visit_pairs.csv` | Baseline to follow-up stability: a T1 normal score is `rho × T0 score + sqrt(1 − rho²) × noise` |
| `20_timing.csv` | Intervals between key dates (registration, clinic, sample, follow-up), replacing independent month draws |

The scaffold already shares one ranking per visit, so these slot in without
changing the interface.

## Omics

Omics files sit outside the wrangled list, and their releases (proteomics
platforms, metabolomics, lipids, RNA-seq) will get their own tables once
their layouts are confirmed. The engine is already in place
(`.clinical_omics_matrix()`). For each platform, `33_omics_eigen.csv` and
`34_omics_loadings.csv` summarise the correlation of rank-based normal scores
by up to ten components, so a sample's normal scores are

```
z = sum over components k of sqrt(eigenvalue_k) × s_k × loading_k + e
```

with `s_k` standard normal and `e` independent noise whose variance tops each
feature up to one. Each feature's normal score is then read through its
quantiles in `31_omics_features.csv`, and values are set missing at the
feature's share missing. The plan for the rest:

- `30_omics_platform.csv` and `32_omics_samples.csv`: samples per person and
  visit, layout (long or wide), feature, value and sample columns, and
  per-sample missingness;
- `35_omics_r2.csv`: shift component scores by visit, batch and anchors so
  that each component's R² with them matches;
- `36_omics_visit.csv`: per-feature baseline to follow-up stability, as for
  `19_visit_pairs.csv`;
- `37_omics_tokens.csv`: non-numeric tokens such as `<LLOQ`, written into
  character value columns at their released counts.

## Suppressed cells

The kit suppresses counts below 10 (zeros included), hides a second cell where
one hidden cell could be recovered, rounds other counts to 5, blanks
quantiles that rest on fewer than 30 values or fewer than 10 on either side,
and shows labels only when 10 people share them. The generator:

- reads every blank count as zero (H09). A level that is blank overall is
  never generated; rare real levels are therefore absent from synthetic data;
- interpolates quantile functions only through the quantiles released, with
  linear tails a quarter of the nearest step beyond the outermost one (and
  not below zero when no released quantile is negative). A
  column with fewer than two released quantiles gets no values (missing
  throughout), since it rests on fewer than 30 people;
- falls back to the overall distribution where a whole anchor level is
  suppressed;
- treats a blank `n_missing` in `04_columns.csv` as no missing values.

## What waits for the v1.1 supplement

Seven of the first run's 23 sources stopped on kit bugs; v1.1 (3 November
2026) fixes them and adds two sources. Until it is released:

- **`wrangled` and `master` are partial.** Most wrangled tables have their
  full column list and profile. Where `04_columns.csv` lists fewer columns
  than `03_tables.csv` counts (the REACT-1/2 records table), the generator
  warns and generates only the listed columns; H08 isn't met for that table
  until v1.1. `master` (`combined_data_set.rds`) isn't part of the wrangled
  list and isn't generated.
- **Text-coded dates and births.** v1.1 profiles dates stored as text as
  dates and stops withholding the birth questions; until then those columns
  are `high_cardinality` placeholders or missing.
- **Cross-table files 15–17, 19 and 20** (see Dependencies).
- **Omics.** Proteomics (Olink, Alamar, LC-MS), both PeakPantheR metabolomics
  files and both RNA-seq matrices are partial; XCMS metabolomics and APOE4
  are new in v1.1. Lipids and autoantibodies are complete in v1.

## Next steps

1. Load the v1.1 supplement and check every wrangled table against its
   inventory (column count, order, classes).
2. Add availability patterns, visit pairs and timing.
3. Draw the cohort from synthetic people with a positive test, and generate
   `original_react_data` from `react_extract()` for the same people where
   the columns map.
4. Omics release tables on top of the engine.
5. Build the compact profile, check it, and request approval for bundling.
6. Validation note comparing synthetic output with the candidate's
   aggregates, as `scripts/hes/VALIDATION.md` does for HES.
