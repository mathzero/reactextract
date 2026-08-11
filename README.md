# reactextract

`reactextract` helps researchers obtain consistent REACT-1 and REACT-2
variables inside the REACT secure enclave. It uses the reviewed
[`react_wiki`](https://github.com/mathzero/react_wiki) metadata dictionary so
you can request a topic—such as acute symptoms—instead of maintaining a long
round-by-round list of raw column names.

The package covers REACT-1 rounds 1–19 and REACT-2 rounds 1–6. It retains every
available exact source value and only applies a harmonisation when the mapping
and recode have been reviewed. It does not calculate BMI, household size,
vaccination intervals, test thresholds, or other new measures.

## Install inside the enclave

Use the offline source bundle prepared outside the enclave:

```r
install.packages("reactextract_0.1.3.tar.gz", repos = NULL, type = "source")
```

Oracle extraction also requires the enclave's existing `DBI`, `odbc`, Oracle
client, and driver configuration. Parquet files require `arrow`. No credentials
are stored by this package.

## Extract from Oracle

Create the connection using the enclave's normal credential process, then hand
the open connection to `reactextract`:

```r
con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "Oracle in instantclient_23_0",
  DBQ = "se-enclaves-db02.sm.med.ic.ac.uk:1521/react",
  SVC = "REACT_V",
  UID = Sys.getenv("REACT_DB_USER"),
  PWD = Sys.getenv("REACT_DB_PASSWORD")
)

source <- react_oracle(con)
result <- react_extract(source)
DBI::dbDisconnect(con)
```

The default request covers all dictionary topics and all 25 rounds. A smaller
request is often quicker:

```r
result <- react_extract(
  source,
  families = "health/acute-symptoms",
  rounds = c("REACT1_R01", "REACT2_S5_R01"),
  progress = TRUE
)
```

## First enclave run

Before extracting a full dataset, run the supplied enclave acceptance workflow.
It checks access to all 25 views, exact field availability, observation keys and
the subject crosswalk without downloading respondent values. If that passes, it
runs a small two-round smoke extraction and immediately discards the row-level
result.

```sh
Rscript enclave/run_acceptance.R connection.R reactextract-acceptance
```

See [the enclave installation guide](inst/ENCLAVE_INSTALL.md) for the credential
and connection setup. Credentials must remain in the enclave and are never
stored by `reactextract`.

## Extract from round files

```r
source <- react_files("D:/react_round_files")
result <- react_extract(source, families = "vaccination")

# Named in-memory data frames are also supported for testing or small jobs.
source <- react_files(list(react1.r01 = round_1_data))
```

Directory files may be named with either their round ID (`react1.r01.rds`) or
survey ID (`REACT1_R01.rds`). RDS, CSV, and Parquet are supported. CSV import
does not treat the literal text `NA` as a missing value.

## Results

For normal analysis, use two straightforward tables:

- `data`: identifiers followed by one cleaned concept column;
- `raw_data`: the same identifiers followed by one column for each exact source
  field name.

For example:

```r
result$data
result$raw_data
```

If a concept was not asked in a requested round, participants from that round
remain in `data` and the concept value is missing. A logical concept such as
overweight appears directly as `TRUE`, `FALSE`, or missing; researchers do not
need to select a type-specific value column.

The detailed tables are retained as a record of exactly how the output was
made:

- `observations`: study, round, passcode, linked subject ID, and visit order;
- `raw_values`: exact source field names and typed values;
- `harmonised_values`: only values produced by approved transforms;
- `issues`: missing fields, unavailable rounds, crosswalk problems, and retained
  unrecognised codes;
- `manifest`: the exact package, dictionary, request, and result counts.

Successful data are returned even when another field or round is unavailable.
Unsafe row-position joins are never used.

## Phase 3 profiles

```r
profile <- react_profile(result)
candidate_export <- react_prepare_profile_export(profile)
```

The export preparation suppresses counts below 10, adds complementary
suppression, rounds released counts to the nearest 5, and omits raw text and
exact extrema. It is only a candidate aggregate output: normal enclave
disclosure review is still required before removal.

## Licences

Package code is MIT licensed. The bundled dictionary metadata is CC BY 4.0 and
retains its attribution and checksums inside
`inst/extdata/dictionary.tar.gz`.
