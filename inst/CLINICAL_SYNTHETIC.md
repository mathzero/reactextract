# Synthetic REACT Clinical Study data

Status: in reactextract 0.6.0.9011 (development) since 5 November 2026. The
generator runs on the bundled profile `clinical-profile-v1` (formally approved
for release on 5 November 2026), on a local inventory candidate
(`clinical-inventory-v1.2`) for development, or on invented fixtures in the
tests. [Still open](#still-open) lists what it doesn't generate yet.

`react_clinical()` returns the REACT Clinical Study's wrangled release: the
nested list that the canonical wrangling pipeline saves as
`ge_lc_wrangled.rds`. `react_clinical_omics()` returns the omics files for the
same synthetic cohort. As with `react_extract()` and `react_hes()`, only the
source changes for the wrangled list:

```r
profile <- react_clinical_profile()          # bundled, or REACTEXTRACT_CLINICAL_PROFILE
source  <- react_synthetic(seed = 1L)
gelc    <- react_clinical(source, profile = profile)
omics   <- react_clinical_omics(source, sources = "proteomics", profile = profile)

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
The omics files have no real-data path yet: in the enclave, read them from
their files (the kit's `sources.R` lists the paths).

> **Synthetic data are for learning and writing code, not for findings.**
> Never present results computed on them as REACT results.

## Decisions this design follows

| Decision | Choice |
|---|---|
| Hub H08 | Reproduce the analysis-ready layer: table and column names, order, types, codings and keys |
| Hub H09 | The generator lives in reactextract behind the source interface. A profile from REACT aggregates is bundled only when it has no counts below 10, is checksum-pinned and has an approval record. Suppressed counts are treated as zero |
| Hub H13 | react_twin uses this generator and doesn't copy it |
| Open decision 1 (3 November 2026) | Students load the **wrangled list as it is**. If a flat release is built later, it is a reactextract function that runs on the real and the synthetic list alike, so this generator doesn't change |
| Open decision 2 (3 November 2026) | The cohort **links to reactextract's synthetic people**: it is drawn from the people of a `react_synthetic()` source, so linked synthetic records can be generated for the same people |
| Open decision 4 (3 November 2026) | The Clinical Study comes **before** further NHS linkage (GP records) for term 2 |

## Where the profile comes from

The input is the checked output of the enclave inventory kit. The
`clinical-inventory-v1.2` candidate (passed output checking on 5 November
2026) profiles all 25 sources with kit v1.2. It replaces the first run (2
November) and the v1.1 supplement (3 November), which re-ran the sources that
had stopped on kit bugs. Each candidate has an `inventory/` folder
(structure) and a `profile/` folder (distributions). The kit's `README.md`
and `REVIEWER.md` explain each file.

The candidate itself is never distributed:

- `scripts/clinical/01_build_clinical_profile.R` builds the bundled profile
  `clinical-profile-v1` (`inst/extdata/clinical-profile.rds`) from it: only
  what the generator reads, suppressed level counts as zero, and a check that
  no count of people or rows lies between 1 and 9. `profile$sources` records
  the SHA-256 of every candidate file. `scripts/clinical/README.md` lists what
  is kept and left out, and `DISCLOSURE_REVIEW.md` what would become public;
- by default, `react_clinical_profile()` returns the bundled profile, but only
  when `inst/extdata/clinical-profile-approval.csv` says `formally_approved`
  and its SHA-256 matches the one pinned in `R/clinical_profile.R` (as for
  `hes-profile-v1`);
- for development, `react_clinical_profile(path)` (by default
  `REACTEXTRACT_CLINICAL_PROFILE`) reads a candidate folder and verifies its
  `manifest.csv` checksums. The profile's `version` is then the kit version
  recorded in `00_sources.csv`. Files a candidate lacks (an earlier kit wrote
  fewer) read as empty tables;
- the package's tests use a small invented candidate in the v1.2 layout
  (`tests/testthat/fixtures/clinical-candidate/`, written by
  `make-clinical-candidate.R` beside it). It copies the kit's file layout and
  disclosure rules, not REACT content: table paths, column names and values
  are invented;
- nothing from the candidate beyond the approved profile is committed. The
  release profile produces the same synthetic data as the candidate it was
  built from (a test checks this on the fixture).

## The wrangled list

Every leaf of the wrangled list that `inventory/02_structure.csv` names is
generated at the same path. A full-scale run on the local v1.2 candidate gives
every table the inventory's column names, order and classes.

The leaves are of several kinds, each recognised from the inventory rather
than by name:

| Kind | How it is recognised | Synthetic rows |
|---|---|---|
| Person tables | `has_persons` in `03_tables.csv` | The table's number of main-cohort people and other registrants, scaled by `n`; several rows per person where the rows-per-person bands say so |
| Tables without people | No person key | The inventory's row count |
| Population tables | Person-level, with more than twice as many people as registered (the whole REACT study) | One row for every person in the synthetic REACT population, the cohort included |
| Branch tables | A subtree mirroring top-level tables (registrants who weren't REACT participants) | Their own small group of invented people, with nine-digit IDs from 900,000,001; a structure-only table borrows the distributions of the table it mirrors |
| Suppressed tables | Blank row or people count | No rows (suppressed counts are zero), with the right columns |
| Values | A leaf that isn't a data frame (such as a date limit) | One value: a date whose name says min or max takes the first or last day of the released months of related tables; anything else is missing |

Column classes (`numeric`, `integer`, `character`, `logical`, `Date`,
`POSIXct`, `factor`) are copied. Tibble classes are copied (no tibble package
needed); a `data.table` becomes a plain `data.frame`, since a `data.table`
can't be made correctly in base R.

Each column's values depend on its role in `04_columns.csv`:

| Role | Synthetic values |
|---|---|
| `identifier` | The person key (`subject_id`, however written) comes from the person. Other identifiers take a shape from `14_id_shapes.csv` (letters to `A`, digits to `9`), unique per person and the same in every table |
| `categorical` | Released levels and counts from `11_categorical.csv` |
| `coded_numeric` | Levels from `11_categorical.csv` where present, otherwise as `continuous` |
| `continuous` | Quantiles from `10_numeric.csv`, with its decimals, share of whole numbers and missing codes (`-77`, `-91`, `-92`, `-99`, `-66`, `-555`, `-999`) |
| `date` | Timing intervals where they apply (below), otherwise month counts from `13_date_months.csv` (or month quantiles in `12_date_summary.csv`) with a uniform day; `POSIXct` columns also get a uniform time of day, in UTC (to confirm against the real list) |
| `high_cardinality` | Shapes from `14_id_shapes.csv` where present, otherwise placeholders such as `column_017` |
| `free_text` | `"Synthetic free text"`, at the column's completeness |
| `empty`, `withheld` | Missing throughout; withheld columns (such as dates of birth) are never generated |

Each column is missing at the rate `04_columns.csv` gives. A blank count
there is suppressed and counts as zero.

## People and anchors

**People.** The generator extracts the source's synthetic people
(`people-households/demographics`) as the synthetic REACT population. The
cohort's registrants are drawn from its adults with a recorded sex, weighted
so that age group and sex follow the anchors' counts. Each clinical
`subject_id` is a whole number made from the person's reactextract
`SUBJECT_ID`: `100000000 + 1000000 × round index + person number`, which has
the real key's type (double, nine digits) and can be reversed. `people` can be
supplied instead as a data frame (`SUBJECT_ID`, `sex`, `age`) or an extraction
result.

**Anchors.** The kit profiles by seven anchors (sex, age group, long COVID
group, registration route, study, ethnicity, main-cohort membership), named in
`inventory/01_context.csv`. The kit takes them from `combined_data_set.rds`,
so their counts are in that source's rows, which the generator reads. Each
registrant gets all seven: sex and age group from the person; main-cohort
membership for the right number of registrants (weighted by age group); the
others drawn from their counts by age group, sex and main cohort. Anchor
columns that appear in a table hold the person's anchor. People in a
population table who didn't register have "(missing)" anchors (and aren't in
the main cohort), as in the kit, whose anchors exist only for registrants.

**Membership.** Each table takes its people from one ranking per visit, so
tables of a visit nest. The follow-up group is drawn from the main cohort,
weighted by long COVID group so that its mix matches the follow-up tables'.
Tables with more people than registrants are capped at the registrants.

**Availability.** Each main-cohort member gets availability flags from the
kit's patterns (`16_availability_patterns.csv`): one pattern per group of
flags (baseline, follow-up, linkage, later studies), in proportion to its
count, with follow-up patterns only for the follow-up group. Suppressed
patterns count as zero. The flags decide who has omics samples (below).

## Dependencies

Fidelity goes on what analyses rely on (synthetic data plan, principle 4).

**On the anchors.** For each column, the anchored rows of `10_numeric.csv` and
`11_categorical.csv` give its distribution within each level of each anchor,
one anchor at a time:

- categorical columns: a person's level probabilities multiply the overall
  probabilities by each anchor's within-level ratio (a log-linear model with
  no interactions). Within an anchor level, a blank cell keeps the level's
  overall share and the released cells share the rest (see Suppressed cells);
- numeric columns: each anchor's shift from the overall quantile function at
  the person's probability (the kit gives anchored quantiles from p10 to p90;
  the shift is held flat beyond) is added, and the values are then mapped back
  to the overall quantiles by rank. The anchors overlap (route, study and age
  group, for example), so added shifts overshoot; mapping back keeps the
  overall distribution while the anchors decide who is high or low. Checked
  against the profile's anchored medians, adding the shifts this way fits
  better than averaging them.

**Within tables (`18_correlations.csv`).** Columns of one table with rank
correlations are drawn from a Gaussian copula: Spearman's rho becomes a
Pearson correlation (`2 sin(pi rho / 6)`), the matrix is made positive
definite, and each column's normal score sets the probability at which its
anchor-conditional distribution is read.

**Between visits (`19_visit_pairs.csv`).** A family (such as clinical tests)
names a path element; a follow-up table's baseline table is its path with
`t1` changed to `t0`. For each column with a rank correlation, the follow-up
normal score is `r × baseline score + sqrt(1 − r²) × its own score` (with `r`
converted as above), so both the visit correlation and the follow-up table's
own correlations are approximately kept. The baseline score is that of the
person's baseline *value* (its mid-rank percentile), which carries the
anchors' effect: a column that follows an anchor at baseline but whose
anchored cells are suppressed at follow-up still tracks the baseline value. A follow-up table makes its baseline
table first, in that table's own random stream, so the same baseline is used
whichever tables are requested.

**Timing (`20_timing.csv`).** Each interval links two person-level dates.
Dates that only start intervals are drawn from their months; each end date is
its start date plus an interval drawn from the interval's quantiles. A start
or end in `combined_data_set.rds`, which isn't generated, is taken from the
first wrangled table holding the same column (for example the onset date in
the population table of infection dates). A date that is a placeholder doesn't
start an interval: another person's real date stands in for it.

**Placeholder dates.** Some date columns hold placeholders for unknown dates:
dates up to 1900-01-01 or in 9999 (the kit's cut-off from v1.2, and react-hub
H18's for HES and GP dates), mostly SPSS's zero date, 1582-10-14. Kit v1.2
counts them in `n_placeholder` (`12_date_summary.csv`) and leaves them out of
the date quantiles; their months stay in `13_date_months.csv`. They are kept
as real messiness (plan principle 5), so downstream code meets them as it
does in the real data:

- a placeholder month gives the placeholder date itself (1582-10-14 for
  October 1582, 9999-12-31 for December 9999, the first of the month
  otherwise), at its released share, including for dates set by intervals;
- where the placeholder months are suppressed one by one but their count
  isn't, 1582-10-14 is used at the counted share;
- a placeholder doesn't start an interval: another person's real date stands
  in for it.

## Omics

`react_clinical_omics()` returns each omics source shaped like the object read
from its file: a data frame for a CSV or a single table, or a named list for an
RDS holding several objects (the proteomics tables) or a tximport list of
RNA-seq matrices. It uses the same people as `react_clinical()` for the same
arguments.

**Samples.** A platform's samples at each visit (`30_omics_platform.csv`) go
to people with the matching availability flag: the baseline or follow-up flag
that shares a word with the platform's name, otherwise the flag with the
fewest people that still covers the samples wanted (so RNA-seq samples come
from people flagged in the sequencing manifests, a superset as in the real
data, and genotypes from people with a genome). Flags are matched by their
names in the profile; none is written in the code.

**Values.** For each platform, `33_omics_eigen.csv` and `34_omics_loadings.csv`
summarise the correlation of rank-based normal scores by up to ten components.
A sample's normal scores are

```
z = sum over components k of sqrt(eigenvalue_k) × s_k × loading_k + e
```

with `s_k` standard normal and `e` independent noise topping each feature up
to variance one. A follow-up sample is correlated feature by feature with the
same person's baseline sample (`36_omics_visit.csv`). Each feature's normal
score is read through its quantiles (`31_omics_features.csv`, which the kit
computes on the raw values; its `log2(x + 1)` transform is only for the
low-rank summary, which uses ranks), and set missing at the feature's share
missing.

**Shared measurements.** Platforms of one source with the same samples and
features (raw and normalised values, the same measurements in wide and long
tables, and each platform's rows in the table that stacks all three proteomics
platforms) form one group with one set of samples, sample IDs and
probabilities. Features are matched across a group by name, then by identical
quantiles (the stacked table names proteins differently but holds the same
values), then by position. Raw and normalised values therefore keep their
monotone relation, and the stacked table is built from the same generated
values as the platform tables, split by its platform column.

**Layouts.**

- Wide tables: the sample-level columns, then one column per feature.
- Long tables: one row per sample and feature. The kit profiles them as
  sample-level and row-level columns; `04b_source_columns.csv` (kit v1.2)
  then gives the source table's column order and classes. A row-level column
  with no more distinct values than there are features describes the feature
  (one value per feature); the rest vary by row.
- tximport lists: `abundance` (TPM, from the profile), `counts` (abundance
  times a length per gene, scaled to a library size per sample from
  `32_omics_samples.csv`), `length` and `countsFromAbundance`.
- Person-level tables (such as genotype carriers) are made like wrangled
  tables, from the people with the matching flag.
- Reference tables without people (such as the proteomics name lookup,
  protein summary and detectability table) are made from their profiles, with
  the inventory's row count, or no rows where it is suppressed. Their name
  columns hold the generated protein names, so they join to the tables: with
  two or more name columns, each row pairs a platform's own name for a
  protein with the stacked table's name for it (a lookup); a single name
  column holds the matched platform's own names, or else the stacked table's
  names. The pairing follows the generator's feature matching, not the real
  lookup.

Identifiers that share a name (ignoring case) with one in the wrangled list,
such as a barcode, take the person's value from there, so the files link.

**Not linked.** The untargeted XCMS table has no sample IDs, so it is
generated unlinked: its samples belong to no one.

## Suppressed cells

The kit suppresses counts below 10 (zeros included), hides a second cell where
one hidden cell could be recovered, rounds other counts to 5, blanks
quantiles that rest on fewer than 30 values or fewer than 10 on either side,
and shows labels only when 10 people share them. The generator:

- reads every blank count as zero (H09). A level, pattern or table that is
  blank is never generated;
- interpolates quantile functions only through the quantiles released, with
  linear tails a quarter of the nearest step beyond the outermost one (and not
  below zero when no released quantile is negative). A column with fewer than
  two released quantiles is missing throughout;
- within an anchor level, gives a blank cell the level's overall share and
  rescales the released cells to the rest. Complementary suppression can blank
  a common level within an anchor level, so a blank there isn't evidence of a
  rare level; this uses no count that wasn't released. A whole anchor level
  that is blank falls back to the overall distribution;
- treats a blank `n_missing` in `04_columns.csv` as no missing values.

## Still open

- **Sources not generated.** The later questionnaires (2022, 2025), the
  Cognitron tests, the clinical cohort's extracts of NHS records, the
  identifier and linkage files and `combined_data_set.rds` aren't generated;
  `combined_data_set.rds`'s anchors and availability flags are used. Kit v1.2
  profiles them, so they could be added as person tables (the 2025
  questionnaire's `date` is now a date; the 2022 questionnaire's interview
  start and finish times are free text and would need inventing).
- **Omics gaps.** RNA-seq matrices have only the profiled genes (expressed in
  at least 10% of samples), and their samples don't yet link to people (the
  sequencing manifests, which map them, are workbooks and aren't generated).
  Component R² with visit, batch and anchors (`35_omics_r2.csv`), per-sample
  missingness and non-numeric tokens aren't used yet. Sample-level columns
  that record the visit are drawn independently of the sample's visit. Long
  tables' columns that the kit's tables don't hold are missing.
- **Memory.** At full scale the omics files take several gigabytes (the long
  proteomics tables and RNA-seq matrices dominate). Use `n` or `sources`.
- **Cohort selection.** Registrants are drawn from all synthetic adults, not
  only people with a positive test, and the population table's dates for
  people outside the cohort are drawn without regard to their synthetic test
  results.
- **REACT-1/2 records.** The cohort's REACT-1/2 survey records are generated
  from their profile, not from `react_extract()` for the same people.
