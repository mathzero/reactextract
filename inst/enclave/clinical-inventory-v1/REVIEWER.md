# Output checking: clinical inventory candidate

The candidate describes the REACT Clinical Study's wrangled datasets and omics
files. It contains aggregate tables only: no participant identifiers, sample
barcodes, free text, dates of birth, postcodes, or individual values.

## The v1.2 candidate

`clinical-inventory-v1.2-candidate` covers every source in one run of one kit
version. It replaces the two earlier releases: the first candidate
(2 November 2026, 16 of 23 sources) and the v1.1 supplement (3 November 2026,
nine sources). The rules are the same, so where the data haven't changed,
most rows match the earlier releases. Differences come from these changes:

- `evbirth` and `numbirth` (births) are profiled; v1 withheld them as if they
  were dates of birth;
- dates stored as text are profiled as dates (months and quantiles) rather
  than as text. In v1 this affected `cognitron_1` and `followup_2025` too;
- placeholder dates are any date up to 1900-01-01 or in the year 9999 (the
  SPSS zero date in October 1582, 0001-01-01, 1800-01-01, 1801-01-01). They
  are counted in `n_placeholder` and left out of date quantiles. The count of
  dates that the quantiles use is not written, so it can't be subtracted from
  `n` to recover a suppressed placeholder count. In the earlier releases the
  placeholders were in the quantiles;
- `n_recoded` in `04_columns.csv` counts text values converted from
  Windows-1252, with the usual count protection;
- each proteomics table is profiled with its declared columns. Where a table
  has both a raw and a normalised value, both are profiled as separate
  platforms (`alamar:npq`, `alamar:npq_normalised`). The proteomics lookup and
  QC tables are profiled as ordinary tables, under the same rules;
- `inventory/04b_source_columns.csv` is new. It lists the columns of each
  long omics table in their original order, with their class: names only,
  no values. The same names already appear in `04_columns.csv`.

The untargeted XCMS table has no sample IDs, so its profile is not linked to
people: it has no breakdowns by anchor. The APOE4 file is linked to cohort
members through the genotyping sample wells. Its categories follow the usual
10-person label rule.

Because this candidate overlaps the earlier releases, review it with them in
mind: where a count was suppressed in one release and shown in another, the
two could be compared. Rows for unchanged data are produced the same way
each time, so they match; changed rows come from the list above.

## Protections applied by the kit

- **Counts.** Counts below 10, zeros included, are suppressed. Where a single
  hidden cell could be recovered from its margin, a second cell is hidden:
  the levels of a column within one stratum, and each level across one
  anchor's strata together with its overall count. Every other count is
  rounded to 5. A blank count cell means suppressed. Before writing, the kit
  stops if any count column holds a value below 10 or one that isn't a
  multiple of 5.
- **Shares.** A proportion is blanked when the count it implies, on either
  side, is between 1 and 9.
- **Numeric summaries.** No minimum, maximum, mean, SD or skewness is written,
  because one extreme value can dominate a mean or SD and be recovered from
  them. A quantile is shown only when it rests on at least 30 values with at
  least 10 on each side, so the 1st and 99th percentiles need 1,000 values.
  Quantiles are rounded to 3 significant figures; intervals between dates are
  given in whole days. A 10% trimmed mean is shown only with at least 100
  values.
- **Dates.** Dates are reported by month only, with month counts protected as
  above.
- **Categories.** A label appears only when at least 10 people share it.
  Columns named as identifiers, columns with more than 50 distinct values, and
  free-text columns never contribute values. Identifier-like columns
  contribute only character shapes (letters to `A`, digits to `9`) that at
  least 10 rows share. Columns whose names suggest direct identifiers (date of
  birth, postcode, address, contact details, small-area codes) are listed by
  name and completeness only.
- **Small tables.** Tables with fewer than 50 people, or 50 rows where no
  person key exists, are listed with their columns and types only. This
  includes the non-REACT recruitment branch.
- **Correlations.** These are rank correlations on at least 50 pairs, rounded
  to 2 decimal places.
- **Omics.** Feature quantiles follow the rules above. The low-rank summaries
  (eigenvalues and the loadings of up to 10 components) are computed on
  rank-based normal scores. These cap each sample's influence, so no
  component can reflect one person's profile. R² between components and
  covariates needs 50 samples; categories with fewer than 10 samples are
  excluded. Non-numeric tokens such as `<LLOQ` are counted per platform and
  shown only with at least 10 occurrences.
- **Folder listing.** Long digit runs in file and folder names are masked.
  Folders with more than 25 entries are summarised by extension, with
  approximate counts.

## Points for the reviewer

- These controls prepare a candidate. They are not output-checking approval,
  and they are not proof against every differencing attack. Review the linked
  tables together: overall counts, the same counts by anchor, and the
  availability patterns.
- Anchored tables cross every categorical column with up to 15 levels by sex,
  age group, long COVID group and main-cohort membership, with cells
  suppressed one by one. Numeric quantiles are also given by registration
  route, study and ethnicity; a small group, such as a rare ethnicity
  category, keeps its count but loses its quantiles under the 30-value rule.
  Remove a whole table if that is safer.
- `inventory/` can be released before `profile/`; it holds structure, sizes,
  completeness, package versions and file names only.
- The largest files are `profile/11_categorical.csv` (every categorical
  column, overall and by anchor) and `profile/31_omics_features.csv` (one row
  per protein, metabolite, lipid and expressed gene). The RNA-seq rows of the
  second can be withheld without affecting the other platforms.
- Approving this candidate doesn't approve later recombinations of it with
  other outputs.
