# reactextract

`reactextract` supports two ways of working with REACT data:

1. extract real REACT-1 and REACT-2 data inside the secure enclave; and
2. generate synthetic data outside the enclave so analysis code can be written
   and tested before it is transferred inside.

Both routes use the same `react_extract()` function, the same
[`react_wiki`](https://github.com/mathzero/react_wiki) dictionary, and the same
output tables. In most cases, moving an analysis into the enclave means changing
only the source of the data.

The package covers REACT-1 rounds 1–19 and REACT-2 rounds 1–6.

## Install

Inside the enclave, install the supplied offline source package:

```r
install.packages("reactextract_0.5.6.tar.gz", repos = NULL, type = "source")
library(reactextract)
```

`reactextract` requires R 4.4.0 or later, with no artificial upper-version
limit. Continuous integration checks R 4.4.1, R 4.5.1 and the current R
release (currently R 4.6.x).
The R version recorded in `renv.lock` identifies the environment used to lock
the build and test dependencies; it is not a maximum supported R version.

Outside the enclave, install direct from github:

```r
library(devtools)
install_github("mathzero/reactextract")
library(reactextract)
```

The package does not contain database credentials. Oracle access uses the
enclave's existing `DBI`, `odbc`, Oracle client and driver configuration.

## Start with fictional data

This example creates 500 fictional participants in two rounds and extracts the
acute-symptoms topic:

```r
fake_source <- react_synthetic(
  n_per_round = c(
    REACT1_R07 = 500L,
    REACT2_S5_R03 = 500L
  ),
  seed = 2026L
)

fake_result <- react_extract(
  fake_source,
  families = "health/acute-symptoms",
  rounds = c("REACT1_R07", "REACT2_S5_R03"),
  progress = TRUE
)

head(fake_result$data)
```

The default creates 1,000 fictional participants in every round:

```r
fake_result <- react_extract(react_synthetic(
  seed = 1L
))
```

This produces 25,000 independent fictional observations. A fixed seed gives
the same values every time, making examples and tests reproducible.

The approved synthetic profile preserves:

- exact round-specific raw field names;
- reviewed public response codes and missing-value types;
- approved questionnaire routing, including downstream questions; and
- the same cleaning and harmonisation code used for real extraction.

Version 0.5.6 uses the formally approved v5 profile. In addition to each
field's round-specific distribution, it includes a small reviewed set of
relationships centred on PCR positivity in REACT-1 and IgG antibody positivity
in REACT-2. The result manifest records
`synthetic_dependency_model = outcome_centred_v5` when these tables are used.

Synthetic identifiers are newly created and visibly fictional. Real participant
identifiers and respondent text are never included in the profile or generated
output.

## Extract real data

Create an Oracle connection using the enclave's normal credential process, then
pass that connection to `react_oracle()`:

```r
con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "Oracle in instantclient_23_0",
  DBQ = "se-enclaves-db02.sm.med.ic.ac.uk:1521/react",
  SVC = "REACT_V",
  UID = Sys.getenv("REACT_DB_USER"),
  PWD = Sys.getenv("REACT_DB_PASSWORD")
)

real_source <- react_oracle(con)

real_result <- react_extract(
  real_source,
  families = "health/acute-symptoms",
  rounds = c("REACT1_R07", "REACT2_S5_R03"),
  progress = TRUE
)

DBI::dbDisconnect(con)
```

The default requests every dictionary topic and all 25 rounds:

```r
real_result <- react_extract(react_oracle(con))
```

The extractor selects only required identifiers and fields, works through
rounds sequentially, and reports partial failures without joining independently
queried columns by row position.

## Move the same analysis into the enclave

Put the extraction request in one function:

```r
get_analysis_data <- function(source) {
  react_extract(
    source,
    families = "health/acute-symptoms",
    rounds = c("REACT1_R07", "REACT2_S5_R03"),
    progress = TRUE
  )
}
```

Develop outside the enclave:

```r
result <- get_analysis_data(
  react_synthetic(
    n_per_round = c(REACT1_R07 = 500L, REACT2_S5_R03 = 500L),
    seed = 2026L
  )
)
```

Then run the same function on real data inside the enclave:

```r
result <- get_analysis_data(react_oracle(con))
```

Code written against `result$data`, `result$raw_data` or the detailed result
tables can therefore be transferred without changing their structure.

## Extract from files

Round files are useful for local validation inside an approved environment:

```r
file_source <- react_files("D:/react_round_files")
result <- react_extract(file_source, families = "vaccination")
```

RDS, CSV and Parquet round files are supported. Files may use either the round
ID (`react1.r01.rds`) or survey ID (`REACT1_R01.rds`). Named in-memory data
frames are also accepted:

```r
file_source <- react_files(list(react1.r01 = round_1_data))
```

CSV import preserves the literal text `"NA"` rather than silently converting
it to a database missing value.

## Choose variables by topic or concept

Request a family:

```r
result <- react_extract(
  source,
  families = "vaccination",
  rounds = c("REACT1_R10", "REACT1_R19")
)
```

Or request a specific reviewed concept:

```r
result <- react_extract(
  source,
  concepts = "health.preexisting.overweight",
  rounds = "REACT1_R01"
)
```

Use `react_families()` to see available topics. The
[REACT metadata wiki](https://mathzero.github.io/react_wiki/) shows concepts,
raw field names, round coverage, response coding and approved distributions.

## Results

The default output is deliberately compact and centres on two researcher-ready
wide tables:

- `result$data`: identifiers followed by cleaned concept columns;
- `result$raw_data`: the same identifiers followed by exact source fields.

```r
result$data
result$raw_data
```

The default result also contains small supporting tables:

- `observations`: study, round, participant keys and visit order;
- `column_dictionary`: the fields and rounds contributing to each output column;
- `issues`: unavailable fields, unexpected codes and partial failures;
- `manifest`: package, dictionary, source, request, counts and stage timings.

Detailed typed values are available when they are genuinely needed, but are not
retained by default because an all-round extraction would otherwise contain
tens of millions of long rows:

```r
long_result <- react_extract(source, output = "long")
full_result <- react_extract(source, output = "both")
```

`output = "long"` omits the wide tables and returns `raw_values` and
`harmonised_values`. `output = "both"` returns all eight tables. Profiling an
extraction result requires one of these long-enabled modes; large enclave
profiles should use `react_profile_source()` directly.

Use the column dictionary whenever a concept has multiple component fields:

```r
result$column_dictionary
```

Single-field concepts use their full topic-prefixed concept ID. Genuine
multi-item outputs use `concept_id__field__EXACT_RAW_FIELD`; the marker can be
removed when grouping components:

```r
sub("__field__.*$", "", names(result$data))
```

## Cleaning and harmonisation

Exact raw values are always retained. Cleaned output applies only decisions
reviewed in the shared wiki dictionary:

- approved scientific transforms are applied as written;
- documented category labels and administrative missing codes are standardised;
- fields are merged only where the reviewed evidence supports equivalence; and
- uncertain concepts remain source-preserving rather than being guessed.

Inspect the relevant decisions offline:

```r
react_concept_coding("health.acute_symptoms.cough")
react_harmonisation_notes()
react_harmonisation_decisions()
```

The package does not automatically calculate analysis measures such as BMI,
household size or laboratory thresholds. Optional, transparent REACT-1 code
for established PCR positivity (`estbinres`), IMD quintile, fixed age groups,
household size and BMI is provided in the
[derived-variable recipes](inst/DERIVED_VARIABLES.md). Each recipe lists its
exact raw inputs, round-specific decisions and known limitations.

## Progress and validation

With `progress = TRUE`, extraction reports the current round, requested fields,
records received and elapsed time. It then reports combining, visit assignment,
harmonisation, table creation and validation. Stage timings are saved in the
result manifest.

Before a first real extraction, run the read-only enclave acceptance workflow:

```sh
Rscript enclave/run_acceptance.R connection.R reactextract-acceptance
```

See [the enclave installation guide](inst/ENCLAVE_INSTALL.md) for setup details.

## What fictional data are for

Synthetic REACT data are designed for:

- writing and debugging extraction requests;
- developing cleaning and analysis code;
- checking joins, reshaping and plotting;
- creating reproducible examples; and
- testing that code will accept the real result structure.

The default generator uses the formally approved, disclosure-controlled
`react-synthetic-profile-v5` round distributions. The package verifies the
profile archive, its table manifest, the exact rc14 dictionary checksum and the
approval record before generating any values. A public-domain-only fallback
remains available for package testing with
`react_synthetic_profile(development = TRUE)`.

The v5 profile separates exact zero from positive Ct/Cp measurements,
preserves occurrence-specific laboratory-result categories and the source's
exact single-space missing value, and supplies the reviewed outcome-centred
dependency tables. These relationships make the fictional data more useful for
developing analyses of the studies' main outcomes; they are not a general model
of the real joint data distribution.

PCR profiling is deliberately strict and occurrence-specific. Exact `Not
Detected` is negative in the common support; Round 11 also has the reviewed
lowercase value `negative`. Round 2 `FINALRESULT = "Rejected"` and Round 13
`RESULT = "ambiguous"` are missing/non-evaluable. These decisions apply only to
those exact occurrences. Every unfamiliar label remains missing, is reported,
and must be reviewed before it can enter the shared dictionary.

Neither form of synthetic data is suitable for prevalence estimation,
hypothesis testing, power calculations or substantive scientific conclusions.
The released aggregates are rounded and disclosure-controlled. Apart from the
reviewed outcome-centred dependencies and approved questionnaire routing,
general relationships between variables are not modelled.

## Licences

Package code is MIT licensed. Bundled dictionary metadata is CC BY 4.0 and
retains its attribution and checksums. The dictionary, concept links,
harmonisation rules and questionnaire routing are maintained in
[`react_wiki`](https://github.com/mathzero/react_wiki), then included here as a
pinned release rather than duplicated as separate scientific decisions.
