# Approval: clinical-profile-v1

Status: **formally approved** by `mathzero` on 2026-11-05, after the
disclosure review in `scripts/clinical/DISCLOSURE_REVIEW.md`. The record is in
`clinical-profile-approval.csv`; `react_clinical_profile()` does not load the
profile without it.

- Profile file: `clinical-profile.rds`
- SHA-256: `ac78e27a67c33630d14c28a1f5e29ae94efb18ced711af0821db45233d03eaf0`
- Built: 5 November 2026 by `scripts/clinical/01_build_clinical_profile.R`
- Approval scope: release of these exact profile bytes in the public package.
- External approval reference: not supplied.

What the profile contains, all from the REACT clinical inventory kit's v1.2
candidate (run in the enclave on 4 November 2026, passed output checking on 5
November 2026):

- **Structure** of the 31 leaves of the Clinical Study's wrangled release and
  of the omics files the generator makes: table paths, column names, order and
  classes, row and people counts, completeness counts and rows-per-person
  bands.
- **Distributions**: category counts (overall and by sex, age group, long
  COVID group and main-cohort membership), numeric quantiles (overall, and
  p10–p90 by those anchors and by registration route, study and ethnicity),
  missing-code counts, date counts by month and placeholder counts, and the
  character shapes of identifiers (letters as `A`, digits as `9`).
- **From `combined_data_set.rds`**: only the anchors' category counts and the
  availability-flag patterns of the main cohort.
- **Coarsened after the disclosure review (5 November 2026):** the clinic
  sites at baseline and follow-up and the vaccination providers' organisation
  codes are replaced by neutral labels numbered by count, and the Olink kit lot
  number by a placeholder. No mapping to the original labels is kept.
- **Dependencies**: rank correlations within tables, baseline to follow-up
  rank correlations, and quantiles of the intervals between key dates.
- **Omics**: per platform, sample counts by visit, feature names and
  quantiles, eigenvalues and loadings of up to ten components (computed on
  rank-based normal scores), feature visit stability, library-size quantiles
  and the column order of long proteomics tables.

The kit's disclosure controls apply throughout: counts below 10 suppressed
(zeros included), complementary suppression, other counts rounded to 5,
quantiles only from 30 values with 10 on each side (p01 and p99 from 1,000),
no minima, maxima, means or SDs, labels only when shared by at least 10
people, free text and direct identifiers withheld. Suppressed counts are
treated as zero and never reconstructed; the build checks that no count of
people or rows lies between 1 and 9. Left out: the kit's folder listing,
environment, provenance, ID sets and overlaps, source issues, per-flag
availability, component R² and tokens. The candidate is not distributed; its
files' SHA-256 checksums are in `profile$sources`.

It contains no records, identifiers or counts below 10.
