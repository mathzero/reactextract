# Enclave installation and acceptance

`reactextract` itself has no non-base runtime dependency, so its source tarball
can be installed without contacting a package repository. Oracle and Parquet
features use packages normally supplied by the enclave environment.

```r
install.packages("reactextract_0.3.3.tar.gz", repos = NULL, type = "source")

stopifnot(getRversion() >= "4.4.0", getRversion() < "4.5.0")
stopifnot(requireNamespace("DBI", quietly = TRUE))
stopifnot(requireNamespace("odbc", quietly = TRUE))

library(reactextract)
react_dictionary_version()
nrow(react_families())
react_harmonisation_notes(open = FALSE)
```

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

The acceptance runner does not save the row-level smoke result. Keep even its
aggregate report inside the enclave unless it has passed the normal disclosure
review process. Later extraction results must never be copied out of the enclave.
Only aggregate profiles prepared with `react_prepare_profile_export()` should be
submitted for disclosure review.

## Build the synthetic-data profile

### Approved categorical profile

Version 0.3.3 includes the formally approved `react-synthetic-profile-v4`.
The completed correction recovered 151 exact distributions from previously
approved singleton bins and queried only the remaining 17 fields. No additional
profile work is required to use `react_synthetic()`.

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
