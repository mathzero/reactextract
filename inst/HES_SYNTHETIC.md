# Linked hospital records (HES)

`react_hes()` returns REACT's linked Hospital Episode Statistics for a set of
participants: admitted patient care (one row per episode) and outpatients (one
row per appointment). Records cover April 2015 to March 2023, the period of
REACT's linked HES. As with `react_extract()`, only the source changes between
synthetic and real data.

```r
source <- react_synthetic(n_per_round = 1000L, seed = 1L)
result <- react_extract(source, families = "people-households/demographics")
hes <- react_hes(source, result)

apc <- hes$NHSE_HES_ADMITTED_PATIENTS
op  <- hes$NHSE_HES_OUTPATIENTS
```

In the secure environment, use the same code with `source <- react_oracle(con)`.
`react_hes()` then queries the real tables for the participants'
`SUBJECT_ID`s, 1,000 at a time. Use `columns` to fetch only what you need:

```r
hes <- react_hes(source, result, tables = "apc",
                 columns = c("ADMIDATE", "DISDATE", "ADMIMETH", "DIAG_4_01", "DIAG_4_CONCAT"))
```

> **Synthetic data are for learning and writing code, not for findings.** The
> records are fictional, made from summary tables rather than anyone's records.

## How the tables match the real data

| Real table | Rows | Columns |
|---|---|---|
| `NHSE_HES_ADMITTED_PATIENTS` | Episodes (spells can have several) | 239 |
| `NHSE_HES_OUTPATIENTS` | Appointments | 147 |

Synthetic tables have the real column names, order and types:

- dates are `POSIXct` at UTC midnight;
- Oracle `NUMBER` columns are double;
- text columns are character.

This follows odbc's usual mapping of Oracle types (to confirm in the enclave).
`SUBJECT_ID` keeps the type of the participants' IDs.

Codes use HES formats:

- ICD-10 without the dot (`J181`, `I10X`), in `DIAG_4_01`–`DIAG_4_12`, with
  every code in `DIAG_4_CONCAT`;
- OPCS-4 procedures (`X292`), with `-` for no procedure;
- `R69X` for an outpatient appointment with no diagnosis recorded.

## What the synthetic records capture

- **Who uses hospitals, and how much.** Admission and appointment rates depend
  on age and sex. Some people use hospitals far more than others, and people
  admitted more often also have more outpatient appointments. Calibrated so
  that, for the synthetic REACT participants, these match REACT's linked HES:
  - records per person and the share of people with any record;
  - the age distribution;
  - the sex split.
- **When.** Yearly trends, the drop in admissions in spring 2020, and COVID-19
  admissions (U07.1/U07.2) following REACT's monthly counts from February 2020.
- **Hospital stays.**
  - Spells made of one or more episodes, day cases and length of stay.
  - Admission method and source, discharge method and destination.
  - In-hospital deaths; nothing happens to a person after they die in hospital.
- **Diagnoses and procedures.** They depend on age, sex and admission type,
  following national HES 2023-24 statistics. Older patients have more
  secondary diagnoses.
- **Outpatient courses.** A first attendance, then follow-ups in the same
  specialty and provider.

## What they do not capture

- **Other columns are independent of each other.** Provider, commissioner,
  HRG, geography and similar columns take plausible values chosen
  independently. Residence fields stay the same for a person. Provider and
  specialty stay the same within a spell or outpatient course.
- **No link to REACT test results.** A positive synthetic swab does not make
  a later COVID-19 admission more likely.
- **No deaths outside hospital**, and no mortality register.
- **No GP, A&E or critical care records** (planned).
- **Some differences from REACT's aggregates:**
  - emergency admissions are 37% of episodes, against REACT's 33%;
  - delivery episodes are 1.2%, against 3.2%;
  - outpatient appointments are 20.8 per person, against 22.0.

  See `scripts/hes/VALIDATION.md` in the source repository.

## Sources

The profile behind the synthetic records (`extdata/hes-profile.rds`) contains:

- Public sector information licensed under the
  [Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/),
  © Crown copyright:
  - NHS England artificial HES data (admitted patient care and outpatients,
    February 2023 v1);
  - NHS England, Hospital Admitted Patient Care Activity 2023-24 (diagnosis
    and procedure open data).
- Aggregates of REACT's linked HES tables that passed output checking in the
  REACT secure environment:
  - column layout;
  - value frequencies from counts of at least 10, rounded to 5;
  - completeness and 5th–95th percentiles;
  - monthly COVID-19 admission counts.

  No records, identifiers or small counts.

How the profile is built, and its assumptions, are in `scripts/hes/README.md`
in the source repository.
