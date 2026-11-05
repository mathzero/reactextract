# Building the Clinical Study profile

`inst/extdata/clinical-profile.rds` (`clinical-profile-v1`) is the profile
behind synthetic `react_clinical()` and `react_clinical_omics()`, so that they
work without a local candidate (react-hub H17). It is built from the checked
output of reactextract's clinical inventory kit. No participant-level data are
used, the kit's candidate stays on the developer's machine, and the profile is
committed only once its exact bytes are approved for release (H09).

```sh
REACT_CLINICAL_CANDIDATE=/path/to/clinical-inventory-v1.2-candidate \
  Rscript --no-init-file scripts/clinical/01_build_clinical_profile.R
# pin the printed SHA-256 as .clinical_profile_sha256 in R/clinical_profile.R
# and in inst/extdata/clinical-profile-approval.csv
Rscript --no-init-file scripts/clinical/02_validate_clinical.R     # writes scripts/clinical/VALIDATION.md
Rscript --no-init-file scripts/clinical/03_disclosure_inventory.R  # writes dist/clinical-profile-review/
# review scripts/clinical/DISCLOSURE_REVIEW.md with the listing; once the
# exact bytes are approved, set the approval record to formally_approved and
# commit the profile, VALIDATION.md and DISCLOSURE_REVIEW.md:
# react_clinical_profile() does not load the profile without the record
```

`--no-init-file` skips the project's renv profile; the scripts need
`pkgload` (and `digest`, or a `sha256sum`/`shasum` command) from your user
library.

## Input (`REACT_CLINICAL_CANDIDATE`, local only)

The `clinical-inventory-v1.2` candidate, written by
`inst/enclave/clinical-inventory-v1` (kit v1.2) in the enclave on 4 November
2026 and passed through output checking on 5 November 2026. It profiles all 25
of the kit's sources. Its disclosure controls are in the kit's `REVIEWER.md`:
counts below 10 suppressed (zeros included) with complementary suppression,
other counts rounded to 5, quantiles only from 30 values with 10 on each side,
no minima, maxima, means or SDs, labels only when shared by at least 10
people, free text and direct identifiers withheld. Reading the candidate
checks every file against its `manifest.csv`; the profile records the SHA-256
of each file.

## What the build keeps

The build (`.clinical_profile_release()` in `R/clinical_profile.R`) keeps only
what the generator reads, in the generator's own tables, and checks that no
count of people or rows lies between 1 and 9:

| Kept | From |
|---|---|
| Structure of the wrangled list and the omics files the generator makes (paths, classes, sizes, columns and completeness) | `inventory/02`–`04`, `04b` |
| Category counts, numeric quantiles and missing codes, date months and placeholder counts, identifier shapes | `profile/10`–`14` |
| The anchors' counts and the availability patterns, from `combined_data_set.rds` only | `profile/11`, `16` |
| Rank correlations, visit pairs (rank correlations only) and timing quantiles | `profile/18`–`20` |
| Omics platforms, features, library sizes, eigenvalues, loadings and visit stability | `profile/30`–`34`, `36` |

Columns the generator doesn't read are dropped (for example people counts per
level, trimmed means and change quantiles between visits). Suppressed level
counts become zero and are never reconstructed. Left out entirely: the folder
listing (05), environment (06), provenance (07), ID sets and overlaps (08,
09), source issues, per-flag availability (15, 17), component R² (35) and
tokens (37), and every source the generator doesn't make.

## Coverage

Generated: the 31 leaves of the wrangled release and the omics files
(proteomics tables and reference tables, PeakPantheR and XCMS metabolomics,
lipids, RNA-seq, autoantibodies, genotyping sample lists and APOE4).

Not covered yet, and what a later profile version would need:

| Source | Would need |
|---|---|
| Later questionnaires (2022, 2025) | Their structure and distributions (already in the candidate) and a disclosure review of their labels; several rows per person. The 2022 interview start and finish times are free text, so would be invented |
| Cognitron (two files) | The same; test scores are numeric, so mostly quantiles |
| The cohort's HES and GP extracts | Code-level counts belong to reactAtlas (H10), so a later version would generate their structure and record dates, and take codes from `react_hes()` or a GP generator |
| Linkage ID sets, identifier crosswalk, invitations | Structure only: they hold identifiers, so synthetic versions would be invented keys for the synthetic people |
| Sample lookup, availability sets, provenance | Derivable from the synthetic people and flags; little to profile |
| `combined_data_set.rds` (2,160 columns) | Its full column profile (most columns repeat wrangled ones) |
| RNA-seq manifests (workbooks) | Their column profile and a link from synthetic BioSample names to people |

## Checks

`02_validate_clinical.R` compares synthetic output with what the profile holds
(structure, category shares, quantiles, missingness, correlations, visit pairs,
timing and omics structure) and writes `VALIDATION.md`.
`03_disclosure_inventory.R` lists every label set, identifier shape, date range
and omics feature set for the approver; `DISCLOSURE_REVIEW.md` summarises it.
