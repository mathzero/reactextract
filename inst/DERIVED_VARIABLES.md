# Derived-variable recipes

These functions add commonly used REACT analysis variables to a `reactextract`
result. They are not applied automatically because they involve analysis
decisions rather than straightforward cleaning or harmonisation.

The recipes:

- leave the extracted raw values unchanged;
- use exact source field names;
- apply to the rounds stated for each function and return `NA` elsewhere; and
- identify where a recipe deliberately differs from the historical round
  wrangling scripts.

They are intended to make an analysis reproducible, not to define new clinical
or diagnostic standards. Check that a recipe is appropriate for the analysis
before using it.

| Function | Variable | Studies and rounds | Raw inputs |
|---|---|---|---|
| `react_derive_estbinres()` | Established PCR positivity (1/0) | REACT-1 rounds 1–19 | `RESULT`, `FINALRESULT`, `LAB`, Ct fields |
| `react_derive_igg()` | IgG antibody positivity (1/0) | REACT-2 rounds 1–6 | `NEWRESULT` |
| `react_derive_age_group()` | Fixed age groups | REACT-1 rounds 1–19 | `U_AGE` |
| `react_derive_imd_quintile()` | IMD quintile (1 most deprived) | REACT-1 rounds 1–19 | `IMD_DECILE` |
| `react_derive_household_size()` | Household size | REACT-1 rounds 1–19 | `NADULTS`, `NADULTS1`, `NCHILD`, `NCHILD1` |
| `react_derive_bmi()` | Body mass index | REACT-1 rounds 2–19 | metric or imperial height and weight |
| `react_bmi_category()` | Adult BMI category | any, from age 18 | a BMI and an age |

The source code for every function is in
[`R/derived.R`](https://github.com/mathzero/reactextract/blob/main/R/derived.R)
and is written in plain base R so that each rule can be read and checked.

## Prepare an extraction

The examples use the normal wide result. The following topic selection contains
the inputs needed by every recipe on this page:

```r
result <- react_extract(
  source,
  families = c(
    "infection-measurement/laboratory",
    "infection-measurement/testing",
    "people-households/demographics",
    "people-households/household",
    "people-households/housing"
  )
)

raw <- result$raw_data
analysis_data <- result$data

stopifnot(identical(
  raw$observation_id,
  analysis_data$observation_id
))
```

Each function takes `result$raw_data` (or the whole `result`) and returns one
value per row, in the same order as `result$data`. When using only one recipe,
a narrower extraction is possible; the function stops and names any required
raw fields that were not requested.

## Established PCR positivity (`estbinres`)

This reproduces the main historical `estbinres` definition, including its
round-specific inputs.

| REACT-1 rounds | Exact raw inputs | Historical handling |
|---|---|---|
| 1 | `RESULT`, `LAB`, `CT_VALUE1`, `CT_VALUE2` | A detected result from a laboratory other than `Eurofin` is positive; otherwise use the Ct rule |
| 2–4 and 6–7 | `RESULT`, `CT_VALUE1`, `CT_VALUE2` | Use the Ct rule |
| 5 | `FINALRESULT` | Use the final result alone because the script records an interim-result data problem |
| 8–19 | `RESULT`, `NGENE_CTVALUE`, `EGENE_CTVALUE` | Use the Ct rule |

```r
analysis_data$estbinres <- react_derive_estbinres(raw)

table(
  analysis_data$survey_id,
  analysis_data$estbinres,
  useNA = "ifany"
)
```

Important limitations:

- Outside Round 5, the historical scripts use interim `RESULT`, not
  `FINALRESULT`.
- The two-gene branch is positive whenever both Ct values exceed zero. The Ct
  37 threshold applies only to the N-gene-only branch.
- The nested historical missing-value behaviour is retained. A missing Ct
  value, or a missing Round 1 laboratory value, can therefore produce a missing
  outcome rather than falling through to another branch.
- Literal comparisons such as `Detected` and `Eurofin` are case-sensitive.
- Historical single-participant corrections in Rounds 11 and 17 are
  deliberately omitted. Participant-specific corrections do not belong in
  public analysis code.
- This is a historical analysis definition, not a new diagnostic
  interpretation.

The sensitivity variables in the old scripts (`estbinres35`, `estbinres33`,
`estbinres30`, and `estbinres27`) are not reproduced here. Their named threshold
applied only to an N-gene-only branch, while two-gene detections remained
positive regardless of Ct. Round 5 also used a different source for its main
outcome and sensitivity variables.

## IMD quintile

The same decile-to-quintile conversion was used in all 19 rounds:

```r
analysis_data$imd_quintile <- react_derive_imd_quintile(raw)

analysis_data$imd_quintile_label <- factor(
  analysis_data$imd_quintile,
  levels = 1:5,
  labels = c(
    "1 - most deprived", "2", "3", "4", "5 - least deprived"
  )
)
```

Values outside integer deciles 1–10 remain missing.

## Fixed age groups

The historical scripts intended the following age groups but used each round's
observed maximum as the final boundary. Round 19 also mistakenly labelled every
age of 65 or over as `75+`. This recipe uses fixed boundaries and therefore
deliberately corrects those implementation problems.

```r
analysis_data$age_group <- react_derive_age_group(raw)
```

`U_AGE`, rather than `AGE`, is the field used by the historical scripts.

## Household size

The legacy scripts cleaned negative codes and summed `NADULTS`, `NADULTS1`,
`NCHILD`, and `NCHILD1`. The fields are routed alternatives: some versions
include the participant and others do not. The historical calculation assumes
that the questionnaire routing was applied correctly.

```r
analysis_data$hh_size <- react_derive_household_size(raw)

analysis_data$hh_size_group <- cut(
  analysis_data$hh_size,
  breaks = c(0, 2, 5, Inf),
  labels = c("1-2", "3-5", "6+"),
  right = TRUE
)
```

Check any warning from the function. The legacy row sum can double-count when
both routed alternatives are populated and can produce a partial total when
one component is missing. The scripts' detailed top category also changed from
`7+` in Rounds 1–14 to `6+` in Rounds 15–19; the broader `1-2`, `3-5`, `6+`
grouping was stable.

## Body mass index

The required raw fields are available through the current extraction dictionary
for REACT-1 Rounds 2–19:

- `WEIGHT_KG`, `WEIGHT_S`, and `WEIGHT_P`;
- `HEIGHT_CM`, `HEIGHT_FEET`, and `HEIGHT_INCHES`.

Although the historical Round 1 script contains BMI code, these height and
weight fields are not available for Round 1 through the current dictionary.

```r
analysis_data$bmi <- react_derive_bmi(raw)
analysis_data$adult_bmi_category <- react_bmi_category(
  analysis_data$bmi,
  raw$U_AGE
)
```

Differences and limitations:

- The historical conversions of 2.2 pounds per kilogram and 2.54 centimetres
  per inch are retained.
- This recipe permits imperial fallback when a metric value is missing or
  non-positive. The old scripts fell back only when the metric value was
  negative.
- Both imperial components must be present for a fallback.
- Adult BMI categories are assigned only from age 18. The old scripts applied
  adult thresholds to children as well.
- The recipe does not impose additional height, weight, or BMI plausibility
  limits. These should be defined in the analysis protocol if needed.

## REACT-2 IgG antibody positivity

REACT-2 participants reported the result of a home lateral flow antibody test.
The reviewed outcome definition uses the detailed `NEWRESULT` coding:

| `NEWRESULT` code | Meaning | `react_derive_igg()` |
|---|---|---|
| 0, 1 | Negative, or IgM-only positive | 0 |
| 2, 3 | IgG positive (with or without an IgM line) | 1 |
| 4–7 | Invalid, unreadable or not photographed | `NA` |
| any other value | Missing | `NA` |

```r
analysis_data$igg_positive <- react_derive_igg(raw)
```

IgG positivity measures past infection or vaccination response, not protective
immunity. From Round 5 onwards it also reflects vaccination, so analyses of past
infection need to account for vaccination status.

## Add the new columns to the cleaned table

The preparation example verifies that `result$data` and `result$raw_data` have
the same observation order. If an analysis has reordered either table, join by
`observation_id` instead of relying on row position:

```r
derived <- data.frame(
  observation_id = raw$observation_id,
  estbinres = react_derive_estbinres(raw),
  stringsAsFactors = FALSE
)

position <- match(result$data$observation_id, derived$observation_id)
stopifnot(!anyNA(position))

analysis_data <- result$data
analysis_data$estbinres <- derived$estbinres[position]
```

## Recipes not yet recommended

The historical scripts contain additional calculations that should not yet be
presented as general reusable recipes:

| Area | Reason for deferral |
|---|---|
| Combined specimen date | Source priorities and manually entered acceptable date windows change by round |
| Vaccination summaries | Definitions change repeatedly and some scripts contain non-row-wise calculations |
| Long COVID duration | Source structures change from symptom-specific to generic and back again |
| Employment and hospital summaries | Selecting the lowest checked option can discard valid multiple selections |
| School-age household flags | The historical “any school-aged” logic fails for some mixed-age households |
| Region and week number | Their external helper functions were not present with the scripts |
| Urban/rural classification | It depends on an unidentified postcode lookup and sensitive postcode data |
| Variant and repeat participation | They depend on separate lineage or participant-linkage products |

These can be added after their definitions, inputs, and missing-value behaviour
have been reviewed separately. Apart from `react_derive_igg()`, none of the
recipes on this page should be applied to REACT-2 without an independent review
of the corresponding source fields and scientific definitions.

## Provenance

The REACT-1 recipes were transcribed from the historical REACT-1 round
wrangling scripts `0_data_wrangling_round1.R` through
`0_data_wrangling_round19.R` and `00_react1_data_combine.R`. The source audit
distinguished active code from commented-out proposals and identified
round-specific inputs, external dependencies, and known implementation defects.
The REACT-2 IgG recipe applies the reviewed `react2_igg_positive_v1` outcome
definition in the bundled dictionary, which the synthetic generator also uses.

The [REACT metadata wiki](https://mathzero.github.io/react_wiki/) remains the
source for raw field wording, response coding, availability, and reviewed
cross-round harmonisation.
