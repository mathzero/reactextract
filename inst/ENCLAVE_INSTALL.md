# Enclave installation and acceptance

`reactextract` itself has no non-base runtime dependency, so its source tarball
can be installed without contacting a package repository. Oracle and Parquet
features use packages normally supplied by the enclave environment.

```r
install.packages("reactextract_0.5.6.tar.gz", repos = NULL, type = "source")

stopifnot(getRversion() >= "4.4.0")
stopifnot(requireNamespace("DBI", quietly = TRUE))
stopifnot(requireNamespace("odbc", quietly = TRUE))

library(reactextract)
react_dictionary_version()
nrow(react_families())
react_harmonisation_notes(open = FALSE)
```

R 4.4.0 is the minimum, not an exact requirement, and there is no artificial
upper-version limit. CI checks R 4.4.1, R 4.5.1 and the current R release
(currently R 4.6.x). The R version in `renv.lock` records the environment used
to lock build dependencies and does not impose an upper runtime limit.

The offline bundle includes an `enclave` directory. Copy
`connection.R.example` to a private working location as `connection.R`, and
supply `REACT_DB_USER` and `REACT_DB_PASSWORD` using the enclave's approved
environment or secret mechanism. Never store credentials in the script.

Run the complete read-only preflight and two-round smoke test with:

```sh
Rscript enclave/run_acceptance.R connection.R reactextract-acceptance
```

The preflight checks all 25 configured views, exact field availability,
observation-key uniqueness and the subject crosswalk. Field checks retrieve zero
rows. Key and crosswalk checks retrieve aggregate counts only. The smoke test
runs only after preflight passes and covers pre-existing-condition fields in
REACT-1 round 1 and REACT-2 round 1.

The four `PREVREACT` fields recorded as absent from the round 19 enclave view
are shown as confirmed unavailable. They do not cause acceptance to fail and no
replacement fields are guessed.

For an interactive preflight using an existing connection:

```r
source <- react_oracle(con)
preflight <- react_validate_oracle(source)
react_write_enclave_report(preflight, "reactextract-acceptance/preflight")
```

Normal extractions default to the efficient wide `data` and `raw_data` tables.
Use `output = "long"` only for specialist typed-value work, or `output = "both"`
when both representations are genuinely required.

The offline bundle also includes `DERIVED_VARIABLES.md`. It contains optional
base-R recipes for adding established PCR positivity (`estbinres`), IMD
quintile, fixed age groups, household size and BMI to a wide result. These are
documented analysis choices and are not applied automatically by the package.

The acceptance runner does not save the row-level smoke result. Keep even its
aggregate report inside the enclave unless it has passed the normal disclosure
review process. Later extraction results must never be copied out of the enclave.
Only aggregate profiles prepared with `react_prepare_profile_export()` should be
submitted for disclosure review.

## Synthetic-data profile

Version 0.5.6 includes the formally approved
`react-synthetic-profile-v5`. It supplies the corrected Ct/Cp distributions,
occurrence-specific laboratory-result categories, both main outcomes and 22
reviewed outcome-centred dependencies. It is checksum-pinned to the rc14
dictionary and works immediately with `react_synthetic()`; no enclave profile
command is needed for normal package use.

The commands below are retained only to document how that approved release was
created. Do not rerun them unless a future dictionary or profile release
requires a reviewed rebuild.

The original v5 candidate was created with:

```sh
Rscript enclave/build_synthetic_profile_v5.R connection.R react-synthetic-profile-v4 react-synthetic-profile-v5-candidate
```

The candidate was then corrected by re-querying only 37
`RESULT`/`FINALRESULT` fields and recalculating the affected REACT-1 outcome and
dependency aggregates. Round 11 lowercase `negative` is negative; Round 2
`FINALRESULT = "Rejected"` and Round 13 `RESULT = "ambiguous"` are
missing/non-evaluable:

```sh
Rscript enclave/repair_synthetic_profile_v5_lab_results.R connection.R react-synthetic-profile-v5-candidate react-synthetic-profile-v5-candidate-corrected
```

If that command reports an unreviewed value, identify it with the aggregate
diagnostic rather than changing or trimming the source:

```sh
Rscript enclave/diagnose_lab_result_values.R connection.R reactextract-lab-result-diagnostic
```

The diagnostic never selects identifiers or respondent rows. Keep
`lab-result-values-enclave-only.csv` inside the enclave because it contains
exact counts. Submit `lab-result-values-for-review.csv` through the normal
disclosure process; it suppresses counts below 10 and rounds all other counts
to the nearest 5. A diagnostic label is not an automatic recoding decision.

That exact corrected profile has now received formal disclosure approval and is
the profile bundled with version 0.5.6.

### Complete profile rebuild

After acceptance passes, run the all-round aggregate profiler:

```sh
Rscript enclave/build_synthetic_profile.R connection.R react-synthetic-profile-v1
```

The command works through the rounds and fields in small batches. It does not
write passcodes, subject IDs, free-text answers, exact minimums or exact
maximums. It writes only mechanically protected aggregate tables and their
checksums.

Keep the resulting `react-synthetic-profile-v1` folder inside the enclave and
submit the complete folder for the usual disclosure review. Automated
suppression is a preparation step, not approval. After disclosure approval,
copy that folder back beside the offline bundle for checksum verification and
the final synthetic-data package build.
