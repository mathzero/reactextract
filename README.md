# reactextract

[![R package validation](https://github.com/mathzero/reactextract/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mathzero/reactextract/actions/workflows/R-CMD-check.yaml)

`reactextract` gives one R interface to the REACT-1 and REACT-2 study data, with
two interchangeable sources:

- **Synthetic data**, which anyone can generate on their own computer:
  fictional participants whose records have the same variables, codes,
  questionnaire routing and cleaning as the real data.
- **Real data**, which approved researchers extract inside the secure REACT data
  environment.

The idea is to write and test an analysis on synthetic data, then run exactly
the same code on the real data. Only the data source changes:

```r
# Anywhere: develop and test with synthetic data
source <- react_synthetic(n_per_round = 1000L, seed = 1L)

# Inside the secure environment: run the same analysis on the real data
source <- react_oracle(con)

result <- react_extract(source, families = "health/acute-symptoms")
```

It is designed for anyone new to REACT, including students learning to work
with the data and researchers preparing a data access request. You can explore
the variables and build a complete analysis pipeline before you have access to
the real data.

> **Synthetic data are for learning and writing code, not for findings.** They
> are generated from protected summary tables, not from anyone's records. Do not
> use them to estimate prevalence, test hypotheses, calculate power or draw any
> scientific conclusion. See [What the synthetic data capture](#what-the-synthetic-data-capture).

## The REACT studies

REACT (REal-time Assessment of Community Transmission) was a programme of large
surveys of random samples of the population of England during the COVID-19
pandemic, run by Imperial College London with Ipsos.

| Study | Dates | Rounds | What participants did |
|---|---|---|---|
| **REACT-1** | May 2020 – March 2022 | 19 | People aged 5 and over took a throat and nose swab for a PCR test and completed a questionnaire |
| **REACT-2** | June 2020 – May 2021 | 6 | Adults took a finger-prick antibody (IgG) lateral flow test at home and completed a questionnaire |

Each round recruited a new random sample, with roughly 95,000–210,000
participants tested per round. The questionnaires changed from round to round as
the pandemic, testing and vaccination changed.

The [REACT data wiki](https://mathzero.github.io/react_wiki/) describes every
variable, question, response option and round. Use it alongside this package.

<details>
<summary>Round dates and identifiers</summary>

Rounds are identified by survey IDs such as `REACT1_R08` and `REACT2_S5_R05`.

| Survey ID | Fieldwork | Survey ID | Fieldwork |
|---|---|---|---|
| `REACT1_R01` | 1 May – 1 Jun 2020 | `REACT1_R11` | 15 Apr – 3 May 2021 |
| `REACT1_R02` | 19 Jun – 7 Jul 2020 | `REACT1_R12` | 20 May – 7 Jun 2021 |
| `REACT1_R03` | 24 Jul – 11 Aug 2020 | `REACT1_R13` | 24 Jun – 12 Jul 2021 |
| `REACT1_R04` | 20 Aug – 8 Sep 2020 | `REACT1_R14` | 9 – 27 Sep 2021 |
| `REACT1_R05` | 18 Sep – 5 Oct 2020 | `REACT1_R15` | 19 Oct – 5 Nov 2021 |
| `REACT1_R06` | 16 Oct – 2 Nov 2020 | `REACT1_R16` | 23 Nov – 14 Dec 2021 |
| `REACT1_R07` | 13 Nov – 3 Dec 2020 | `REACT1_R17` | 5 – 20 Jan 2022 |
| `REACT1_R08` | 6 – 22 Jan 2021 | `REACT1_R18` | 8 Feb – 1 Mar 2022 |
| `REACT1_R09` | 4 – 23 Feb 2021 | `REACT1_R19` | 8 – 31 Mar 2022 |
| `REACT1_R10` | 11 – 30 Mar 2021 | | |
| `REACT2_S5_R01` | 20 Jun – 10 Jul 2020 | `REACT2_S5_R04` | 27 Oct – 10 Nov 2020 |
| `REACT2_S5_R02` | 30 Jul – 12 Aug 2020 | `REACT2_S5_R05` | 25 Jan – 8 Feb 2021 |
| `REACT2_S5_R03` | 15 – 28 Sep 2020 | `REACT2_S5_R06` | 12 – 25 May 2021 |

The wiki's [fieldwork page](https://mathzero.github.io/react_wiki/fieldwork.html)
also gives the number of tests in each round.

</details>

## Installation

You need R 4.1.0 or later. The package uses only base R, so it installs no other
packages. Install it from GitHub:

```r
install.packages("remotes")
remotes::install_github("mathzero/reactextract")
```

Check that it worked:

```r
library(reactextract)
packageVersion("reactextract")
react_dictionary_version()
```

## Quick start with synthetic data

This creates 2,000 fictional people in each of two REACT-1 rounds, January 2021
and January 2022, extracts their demographics and PCR results, and makes a
small analysis data set:

```r
library(reactextract)

# 1. Set up a synthetic source. The seed makes the result reproducible.
source <- react_synthetic(
  n_per_round = c(REACT1_R08 = 2000L, REACT1_R17 = 2000L),
  seed = 2026L
)

# 2. Extract two topics.
result <- react_extract(
  source,
  families = c("people-households/demographics", "infection-measurement/laboratory")
)
result

# 3. Build an analysis data set with short column names and a PCR outcome.
dat <- react_short_names(result)
dat$pcr_positive <- react_derive_estbinres(result)
dat$age_group <- react_derive_age_group(result)

table(dat$survey_id, dat$pcr_positive, useNA = "ifany")
prop.table(table(dat$age_group, dat$pcr_positive), margin = 2)

# 4. Save it.
saveRDS(dat, "react_synthetic_demo.rds")
```

The same seed, sample sizes and request always give exactly the same data.

## Choosing variables

### Fields, concepts and topics

REACT variables are organised at three levels:

- A **field** is an exact variable in one round's data set, such as `SMOKENOW`.
  The same question can have different field names in different rounds.
- A **concept** groups the fields that measure the same thing across rounds,
  such as `behaviours_mobility.behaviours.current_cigarette_smoking`.
- A **topic** (or family) groups related concepts, such as
  `behaviours-mobility/behaviours`.

`react_extract()` accepts topics through `families`, concepts through `concepts`,
and rounds through `rounds`. You always get every field belonging to what you
request.

### Find variables on the wiki

The [wiki](https://mathzero.github.io/react_wiki/) is the best place to browse.
Search for a word, open a variable to see its exact question, response options
and which rounds asked it, and add it to **Your shortlist**. The shortlist page
writes the `react_extract()` code for you:

```r
result <- reactextract::react_extract(
  source,
  concepts = c(
    "behaviours_mobility.behaviours.current_cigarette_smoking",
    "people_households.demographics.ethnic_group"
  )
)
```

Create `source` with `react_synthetic()` first, as in the quick start.

### Find variables in R

`react_search()` searches concept names, raw field names and question wording:

```r
react_search("smok")
react_search("U_AGE")                      # a raw field name
react_search("vaccin", study = "react2")   # only REACT-2
```

Each match shows the concept ID to use in `react_extract()`, its topic, its raw
fields, the rounds that asked it and a typical question. `react_families()`
lists all topics.

### Choose rounds and sample sizes

`n_per_round` sets how many fictional people to create in each round. A single
number applies to all 25 rounds; a named vector creates only the rounds you
name:

```r
react_synthetic(n_per_round = 1000L)                              # every round
react_synthetic(n_per_round = c(REACT1_R16 = 5000L, REACT1_R17 = 5000L))
```

`react_extract()` then returns every round in the source unless you set
`rounds`.

### Things to watch

- **`families` and `concepts` combine as a filter.** If you give both, you get
  only the concepts that are *inside* those families. To combine a topic with a
  few extra variables, list everything in `concepts` instead.
- **Larger requests take longer.** Each round is generated in full behind the
  scenes, so time depends mostly on the number of rounds and people:

  | Request | Time | Memory |
  |---|---|---|
  | A few topics, 2–3 rounds, up to 5,000 people per round | seconds | small |
  | All topics, all 25 rounds, 1,000 people per round | about 2 minutes | about 1.5 GB |
  | 6 topics, 19 rounds, 20,000 people per round (380,000 rows) | about 3–4 minutes | about 6 GB |

  These timings are from a laptop. A few thousand people per round is plenty
  for building a pipeline.

## Understanding the result

`react_extract()` returns a list. Printing it gives a short summary. The two
tables you will mostly use have one row per participant per round, in the same
order:

| Table | Contents |
|---|---|
| `result$data` | Cleaned data: response codes replaced by labels and missing codes set to `NA` |
| `result$raw_data` | Raw data: the exact fields and codes as they appear in the REACT database |

Both start with identifier columns: `observation_id`, `study_id`, `round_id`,
`survey_id`, the participant codes `U_PASSCODE` and `SUBJECT_ID`, and
`visit_number`. In synthetic data, every row is a different person.

### Column names

Cleaned columns are named by concept, for example
`people_households.demographics.gender`. When a concept has several fields, the
field name is added: `people_households.demographics.age__field__U_AGE`.
`react_short_names()` shortens these to `gender` and `age_U_AGE`.

### Missing values

The raw data use negative codes to say why an answer is missing. In
`result$data` they all become `NA`; in `result$raw_data` they are kept.

| Code | Meaning |
|---|---|
| `-77` | Survey non-response: the participant did not complete this part of the questionnaire |
| `-91` | Not applicable: the question was not asked because of an earlier answer |
| `-92` | Item non-response: the question was asked but not answered |
| `-99`, `-66`, `-555` | Rarer missing codes; check the variable's page on the wiki |
| `NA` | Missing in the database |

### Coding can change between rounds

Questions, response options and codes were revised over time, so check a
variable round by round before pooling rounds. For example, `AGE_GROUP` has
labelled bands such as `55 to 64` in most REACT-1 rounds, but the source
documentation for some rounds, including all of REACT-2, has no labels, so the
cleaned column keeps codes such as `5` there. `react_derive_age_group()` gives
consistent groups from exact age. To see how a concept's codes map to cleaned
labels in each round:

```r
react_concept_coding("behaviours_mobility.behaviours.current_cigarette_smoking")
```

### Supporting tables

- `result$column_dictionary`: which raw fields and rounds feed each cleaned column.
- `result$issues`: anything the extraction could not provide or had to adjust.
  A few warnings are normal; most say that an unexpected value was kept exactly
  as it appears in the source.
- `result$manifest`: the package and dictionary versions, the request, the
  synthetic seed and profile, and timings.

## Deriving analysis variables

Common REACT analysis variables are not added automatically, because each is an
analysis decision. These functions create them from the raw fields:

| Function | Creates | Applies to |
|---|---|---|
| `react_derive_estbinres()` | PCR positivity, the established REACT-1 definition (1/0) | REACT-1 |
| `react_derive_igg()` | IgG antibody positivity (1/0) | REACT-2 |
| `react_derive_age_group()` | Fixed age groups from `U_AGE` | REACT-1 |
| `react_derive_imd_quintile()` | Index of Multiple Deprivation quintile (1 = most deprived) | REACT-1 |
| `react_derive_household_size()` | Number of people in the household | REACT-1 |
| `react_derive_bmi()` | Body mass index | REACT-1 rounds 2–19 |
| `react_bmi_category()` | Adult BMI category | Any |

Each function takes the extraction result and returns one value per row, in the
same order as `result$data`:

```r
result <- react_extract(
  react_synthetic(n_per_round = c(REACT1_R17 = 5000L), seed = 1L),
  families = c(
    "infection-measurement/laboratory",
    "people-households/demographics",
    "people-households/housing"
  )
)

dat <- react_short_names(result)
dat$pcr_positive <- react_derive_estbinres(result)
dat$imd_quintile <- react_derive_imd_quintile(result)
```

If a function needs a raw field you did not extract, it stops and tells you
which topic to add. The [derived-variable guide](inst/DERIVED_VARIABLES.md)
explains each definition and its limitations.

## Saving your data

Save an analysis data set with `saveRDS()` and read it back with `readRDS()`:

```r
saveRDS(dat, "react_synthetic_analysis.rds")
dat <- readRDS("react_synthetic_analysis.rds")
```

To keep the whole result, including the raw data and supporting tables, use
`react_write(result, "react_result.rds")`.

## Moving your analysis to the real data

Synthetic and real extractions return exactly the same tables, columns and
coding, so code written against `result$data` or `result$raw_data` runs
unchanged on the real data. The simplest way to keep the switch to one line is
to put the extraction in a function that takes the source:

```r
get_analysis_data <- function(source) {
  result <- react_extract(
    source,
    families = c("people-households/demographics", "infection-measurement/laboratory"),
    rounds = c("REACT1_R08", "REACT1_R17")
  )
  dat <- react_short_names(result)
  dat$pcr_positive <- react_derive_estbinres(result)
  dat
}

# While developing
dat <- get_analysis_data(react_synthetic(
  n_per_round = c(REACT1_R08 = 2000L, REACT1_R17 = 2000L),
  seed = 2026L
))

# Inside the secure environment, with a database connection `con`
dat <- get_analysis_data(react_oracle(con))
```

The real data can only be used inside the secure REACT data environment, and
extracted records must never leave it. Access is by application: see
[REACT data access requests](https://www.imperial.ac.uk/medicine/research-and-impact/groups/react-study/data-access-requests/).
Connecting to the database, extracting from round files, output options and the
cleaning rules are described in the [advanced guide](inst/ADVANCED_GUIDE.md),
and installation inside the secure environment in the
[enclave installation guide](inst/ENCLAVE_INSTALL.md).

## Linked hospital records (HES)

`react_hes()` returns the Hospital Episode Statistics linked to REACT
participants: admitted patient care (one row per episode) and outpatients (one
row per appointment), April 2015 to March 2023. As with `react_extract()`, it
gives synthetic records from `react_synthetic()` and the real tables from
`react_oracle()`, with the same columns and types:

```r
hes <- react_hes(source, result)
apc <- hes$NHSE_HES_ADMITTED_PATIENTS
op  <- hes$NHSE_HES_OUTPATIENTS
```

Synthetic records follow REACT's linked HES for admissions per person, ages,
calendar trends and COVID-19 admissions. Diagnoses and procedures depend on age
and sex. A person's records depend only on their age and sex: their synthetic
test results, symptoms and reported conditions have no effect. See
[`inst/HES_SYNTHETIC.md`](inst/HES_SYNTHETIC.md) for what is and is not
captured.

## What the synthetic data capture

Synthetic records are generated round by round from the approved
`react-synthetic-profile-v5`. That profile is a set of summary tables made inside
the secure environment: counts below 10 are suppressed, other counts are rounded
to the nearest 5, and the tables passed formal disclosure review.

**Captured**

- Each round's distribution for every field, so values, missing codes and trends
  over time follow the real rounds.
- Questionnaire routing: participants are only asked questions that applied to
  them.
- The same cleaning and labelling as the real data.
- Relationships between the main outcomes and a set of key characteristics:

  | Outcome | Related characteristics |
  |---|---|
  | REACT-1 PCR positivity | Age, gender, ethnicity, region, deprivation, economic activity, previous COVID-19 and contact with a confirmed case (all rounds); household size (rounds 1 and 4–19); symptoms (rounds 2–19); key-worker or care role (rounds 2–3 and 5–19); vaccination (rounds 8–19) |
  | REACT-2 IgG positivity | Age, gender, ethnicity, region, deprivation, household size, economic activity and previous COVID-19 (all rounds); key-worker or care role (rounds 2–4); vaccination (rounds 5–6) |

- Age and sex patterns in pre-existing conditions (`health.preexisting.*`).
  Diabetes, high blood pressure and heart disease, for example, are much more
  common in older people. **The shape of each pattern comes from published
  statistics for England, such as the Health Survey for England, not from
  REACT**; each round keeps its approved overall prevalence of every condition.
  Conditions tend to occur together, "None of these" is ticked only by people
  who report no condition, and someone who did not answer the question has the
  same missing code in every box. Weakened immune system and organ transplant,
  which have no suitable public source, are equally common at every age. See
  [`inst/CONDITION_AGE_GRADIENTS.md`](inst/CONDITION_AGE_GRADIENTS.md) for the
  sources, figures and method.
- Linked hospital records whose amount and content depend on age and sex; see
  [Linked hospital records (HES)](#linked-hospital-records-hes).

**Not captured**

- **Other relationships between variables.** Apart from the relationships
  above, variables are generated independently. For example, age and smoking
  are unrelated. Pre-existing conditions do not affect PCR or antibody
  results, and nothing in the survey data affects a person's hospital records
  except their age and sex. So, once age and sex are allowed for, any other
  comparison shows only chance differences (see below).
- **Follow-up of the same people.** Every row is a different person, even
  across rounds.
- **Free text.** Write-in answers are replaced by the phrase `Synthetic response`.
- **Some withheld groups.** Where a protected table withheld a group, the
  synthetic data cannot recreate it. Almost nobody aged 85 or over appears in
  REACT-1 rounds. In some rounds, an outcome relationship is not generated for
  a characteristic whose table was partly withheld.
- **REACT-2 IMD quintile.** The `IMD_QUINTILE` field in synthetic REACT-2 data
  contains implausible values; use `IMD_DECILE` instead.
- **A few labels contain `?`** where the source questionnaire had an apostrophe
  or dash, for example `Can?t tell what the result is`.

**Differences that come only from age and sex**

PCR and antibody results, pre-existing conditions and hospital records all
depend on age, and some on sex. So crude comparisons between them can show
differences even though none affects another. In 50,000 synthetic people in
each of REACT-1 rounds 8 and 17, for example:

| People who reported diabetes, against those who did not | Crude | Adjusted for age group and sex |
|---|---|---|
| Had a hospital admission in 2021 | 28% against 22% (odds ratio 1.4) | odds ratio close to 1 |
| Had a diabetes code (E10–E14) in their HES records | 26% against 17% (odds ratio 1.8) | odds ratio close to 1 |

Crude comparisons with PCR or antibody results can differ in the same way, by
an amount that depends on each round's age pattern. This is deliberate: age is
a realistic confounder to practise adjusting for. These figures describe the
generator, not REACT participants.

## Getting help

- The [REACT data wiki](https://mathzero.github.io/react_wiki/) documents every
  variable and round.
- Every function has a help page, for example `?react_extract`, `?react_search`
  or `?react_derive_estbinres`.
- Report problems or suggestions on
  [GitHub Issues](https://github.com/mathzero/reactextract/issues).

## Licences

Package code is MIT licensed. The bundled REACT dictionary metadata is licensed
CC BY 4.0. The HES profile contains public sector information licensed under
the Open Government Licence v3.0 (NHS England artificial HES and HES 2023-24
statistics). The age and sex gradients for pre-existing conditions use
statistics published by NHS England and NHS Digital (the Health Survey for
England, the Adult Psychiatric Morbidity Survey, the Mental Health of Children
and Young People survey and NDRS cancer prevalence), licensed under the Open
Government Licence v3.0, except one Health Survey for England 2010 table used
with attribution; see
[`inst/CONDITION_AGE_GRADIENTS.md`](inst/CONDITION_AGE_GRADIENTS.md#sources).
To cite the REACT data dictionary, see
[How to cite](https://mathzero.github.io/react_wiki/cite.html) on the wiki.
