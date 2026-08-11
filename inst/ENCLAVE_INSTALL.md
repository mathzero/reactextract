# Enclave installation and acceptance

`reactextract` itself has no non-base runtime dependency, so its source tarball
can be installed without contacting a package repository. Oracle and Parquet
features use packages normally supplied by the enclave environment.

```r
install.packages("reactextract_0.1.3.tar.gz", repos = NULL, type = "source")

stopifnot(getRversion() >= "4.4.0", getRversion() < "4.5.0")
stopifnot(requireNamespace("DBI", quietly = TRUE))
stopifnot(requireNamespace("odbc", quietly = TRUE))

library(reactextract)
react_dictionary_version()
nrow(react_families())
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
