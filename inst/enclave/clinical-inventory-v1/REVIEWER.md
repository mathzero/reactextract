# Output checking: clinical inventory candidate

The candidate describes the REACT Clinical Study's wrangled datasets and omics
files. It contains aggregate tables only: no participant identifiers, sample
barcodes, free text, dates of birth, postcodes, or individual values.

## The v1.1 supplement

`clinical-inventory-v1.1-supplement-candidate` holds nine sources only:
`wrangled`, `master`, `proteomics`, `metabolomics_rpos`, `metabolomics_rneg`,
`metabolomics_xcms`, `transcriptomics_t0`, `transcriptomics_t1` and `apoe4`.
The first seven stopped on kit bugs in the first run, so the first candidate
held their tables only up to the point of failure. The last two are new. The
rows for these nine replace their rows in the first candidate. The rules are
the same, so where a table was profiled the same way in both runs, its rows
are identical. Differences come from the v1.1 fixes:

- `evbirth` and `numbirth` (births) are now profiled; they were withheld as
  if they were dates of birth;
- dates stored as text are profiled as dates (months and quantiles) rather
  than as text;
- placeholder dates (such as 1582-10-14, the SPSS zero date) are counted in
  `n_placeholder` and left out of date quantiles. The count of dates that the
  quantiles use is not written, so it can't be subtracted from `n` to recover
  a suppressed placeholder count;
- `n_recoded` in `04_columns.csv` counts text values converted from
  Windows-1252, with the usual count protection;
- each proteomics table is profiled with its declared columns. Where a table
  has both a raw and a normalised value, both are profiled as separate
  platforms (`alamar:npq`, `alamar:npq_normalised`).

The untargeted XCMS table has no sample IDs, so its profile is not linked to
people: it has no breakdowns by anchor. The APOE4 file is linked to cohort
members through the genotyping sample wells. Its categories follow the usual
10-person label rule.

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
