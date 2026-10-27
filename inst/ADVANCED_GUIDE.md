# Advanced guide: real data, file sources and output options

This guide is for researchers who have, or are preparing for, access to the real
REACT data inside the secure environment, and for anyone who needs more control
than the [README](../README.md) describes. Installation and acceptance testing
inside the enclave are covered separately in the
[enclave installation guide](ENCLAVE_INSTALL.md).

## One interface, three data sources

`react_extract()` accepts three kinds of source and returns the same result
structure for each:

| Source | Created by | Where it is used |
|---|---|---|
| Synthetic data | `react_synthetic()` | Anywhere: laptops, the HPC, teaching |
| Real data from the REACT database | `react_oracle(con)` | Inside the secure enclave only |
| Round files (RDS, CSV or Parquet) | `react_files(path)` | Approved environments, for validation |

All three use the same pinned [REACT data wiki](https://mathzero.github.io/react_wiki/)
dictionary and the same cleaning code, so an analysis usually needs to change
only its source to move between them.

## Extract real data inside the enclave

Create an Oracle connection using the enclave's normal credential process, then
pass that connection to `react_oracle()`. The package never stores credentials.

```r
con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "Oracle in instantclient_23_0",
  DBQ = "se-enclaves-db02.sm.med.ic.ac.uk:1521/react",
  SVC = "REACT_V",
  UID = Sys.getenv("REACT_DB_USER"),
  PWD = Sys.getenv("REACT_DB_PASSWORD")
)

real_result <- react_extract(
  react_oracle(con),
  families = "health/acute-symptoms",
  rounds = c("REACT1_R07", "REACT2_S5_R03"),
  progress = TRUE
)

DBI::dbDisconnect(con)
```

With no `families`, `concepts` or `rounds`, every dictionary topic is requested
for all 25 rounds. The extractor selects only the required identifiers and
fields, works through rounds one at a time, and reports partial failures without
joining independently queried columns by row position.

Results extracted inside the enclave must never be copied out of it. Only
aggregate profiles that have passed disclosure review may leave.

## Move an analysis from synthetic to real data

Put the extraction request in one function and pass it a different source:

```r
get_analysis_data <- function(source) {
  react_extract(
    source,
    families = "health/acute-symptoms",
    rounds = c("REACT1_R07", "REACT2_S5_R03")
  )
}

# Outside the enclave
result <- get_analysis_data(react_synthetic(
  n_per_round = c(REACT1_R07 = 500L, REACT2_S5_R03 = 500L),
  seed = 2026L
))

# Inside the enclave
result <- get_analysis_data(react_oracle(con))
```

Code written against `result$data`, `result$raw_data` or the supporting tables
keeps working because their structure is identical.

## Extract from round files

```r
file_source <- react_files("D:/react_round_files")
result <- react_extract(file_source, families = "vaccination")
```

RDS, CSV and Parquet files are supported (Parquet needs the `arrow` package).
Files may be named by round ID (`react1.r01.rds`) or survey ID
(`REACT1_R01.rds`). Named in-memory data frames are also accepted:

```r
file_source <- react_files(list(react1.r01 = round_1_data))
```

CSV import preserves the literal text `"NA"` rather than converting it to a
missing value.

## Output options

The default `output = "wide"` returns compact, analysis-ready tables:

| Table | Contents |
|---|---|
| `data` | Identifiers followed by cleaned concept columns |
| `raw_data` | The same identifiers followed by exact source fields |
| `observations` | Study, round, participant keys and visit order |
| `column_dictionary` | The fields and rounds contributing to each cleaned column |
| `issues` | Unavailable fields, unexpected codes, fallbacks and partial failures |
| `manifest` | Package, dictionary, source, request, counts and timings |

Detailed typed values are available when genuinely needed:

```r
long_result <- react_extract(source, output = "long")  # raw_values and harmonised_values only
full_result <- react_extract(source, output = "both")  # all eight tables
```

They are not retained by default because an all-round extraction would contain
tens of millions of long rows. `react_profile()` needs one of these long modes;
large enclave profiles should use `react_profile_source()` directly.

`react_write()` saves any result as RDS, or as a Parquet directory:

```r
react_write(result, "outputs/react_result.rds")
react_write(result, "outputs/react_parquet", format = "parquet")
```

### Column names

Single-field concepts use their full concept ID, such as
`people_households.demographics.gender`. Concepts with several component fields
use `concept_id__field__EXACT_RAW_FIELD`. `react_short_names()` drops the topic
prefix for analysis, and `result$column_dictionary` maps every column back to
its concept, rounds and raw fields.

## Cleaning and harmonisation

Exact raw values are always retained in `raw_data`. Cleaned output applies only
decisions reviewed in the shared wiki dictionary:

- approved scientific transforms are applied as written;
- documented category labels and administrative missing codes are standardised;
- fields are merged only where the reviewed evidence supports equivalence; and
- uncertain concepts stay source-preserving rather than being guessed.

Inspect the relevant decisions offline:

```r
react_concept_coding("health.acute_symptoms.cough")
react_harmonisation_notes()
react_harmonisation_decisions()
```

Analysis measures such as `estbinres`, IMD quintile, age groups, household size,
BMI and IgG positivity are not applied automatically. Use the `react_derive_*()`
functions described in the [derived-variable guide](DERIVED_VARIABLES.md).

## Synthetic-data models

| Profile | How to select it | Status |
|---|---|---|
| `v5` | default; `react_synthetic_profile(version = "v5")` | Formally disclosure-approved; recommended |
| `v6` | `react_synthetic_profile(version = "v6")` | Experimental participation preview |
| Development | `react_synthetic_profile(development = TRUE)` | Public response domains only, for package testing |

The `v5` profile reproduces each round's protected distributions, questionnaire
routing, and the reviewed outcome-centred relationships for REACT-1 PCR and
REACT-2 IgG positivity described in the README. The package verifies the profile
archive, its manifest, the exact dictionary checksum and the approval record
before generating any values. The manifest records the profile, seed and model
used.

The `v6` preview adds approved participation, eligibility and round-specific
dependency models, but several of its categorical distributions are currently
implausible. Use it only to test code against the preview. Its design and
remaining gaps are documented in
[Relationships represented in the v6 preview](SYNTHETIC_RELATIONSHIPS.md) and
the [participation and eligibility decisions](PARTICIPATION_V6_DECISIONS.md).

## Progress and validation

With `progress = TRUE`, extraction reports the current round, requested fields,
records received and elapsed time, then each later processing stage. Stage
timings are saved in the result manifest.

Before a first real extraction, run the read-only enclave acceptance workflow
described in the [enclave installation guide](ENCLAVE_INSTALL.md):

```sh
Rscript enclave/run_acceptance.R connection.R reactextract-acceptance
```

## Licences

Package code is MIT licensed. Bundled dictionary metadata is CC BY 4.0 and
retains its attribution and checksums. The dictionary, concept links,
harmonisation rules and questionnaire routing are maintained in the
[REACT data wiki](https://mathzero.github.io/react_wiki/) and included here as a
pinned release rather than duplicated as separate scientific decisions.
