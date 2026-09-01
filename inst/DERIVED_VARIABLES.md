# Derived-variable recipes

These optional recipes add commonly used REACT-1 analysis variables to a
`reactextract` result. They are not applied automatically because they involve
analysis decisions rather than straightforward cleaning or harmonisation.

The recipes:

- leave the extracted raw values unchanged;
- use exact source field names;
- apply only to REACT-1 rounds 1–19; and
- identify where a recipe deliberately differs from the historical round
  wrangling scripts.

They are intended to make an analysis reproducible, not to define new clinical
or diagnostic standards. Check that a recipe is appropriate for the analysis
before using it.

## Prepare an extraction

The examples use the normal wide result. The following topic selection contains
the inputs needed by every recipe on this page:

```r
result <- react_extract(
  source,
  families = c(
    "infection-measurement/laboratory",
    "people-households/demographics",
    "people-households/household",
    "people-households/housing"
  ),
  rounds = sprintf("REACT1_R%02d", 1:19)
)

raw <- result$raw_data
analysis_data <- result$data

stopifnot(identical(
  raw$observation_id,
  analysis_data$observation_id
))
```

The default all-family extraction also contains these fields. When using only
one recipe, a narrower extraction is possible; its function will report any raw
fields that were not requested.

The functions below use base R and can be copied together into an analysis
script.

```r
as_numeric_raw <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

require_columns_for <- function(data, rows, columns, recipe) {
  missing <- setdiff(columns, names(data))

  if (any(rows, na.rm = TRUE) && length(missing) > 0L) {
    stop(
      recipe, " needs raw column", if (length(missing) > 1L) "s " else " ",
      paste(missing, collapse = ", "),
      ". Re-run react_extract() with the relevant topic families.",
      call. = FALSE
    )
  }

  invisible(data)
}

react1_lab_result_state <- function(result, survey, field = "RESULT") {
  result <- as.character(result)
  survey <- as.character(survey)
  state <- rep(NA_character_, length(result))
  state[result == "Detected"] <- "positive"
  state[result == "Not Detected"] <- "negative"
  state[is.na(result) | result %in% c(
    "", " ", "Void", "-91", "-92", "-77", "-66", "-99", "-555"
  )] <- "missing"
  state[
    survey == "REACT1_R11" & field == "RESULT" & result == "negative"
  ] <- "negative"
  state[
    survey == "REACT1_R13" & field == "RESULT" & result == "ambiguous"
  ] <- "missing"
  state[
    survey == "REACT1_R02" & field == "FINALRESULT" & result == "Rejected"
  ] <- "missing"
  state
}

legacy_ct_positive <- function(result, ct1, ct2, survey, threshold = 37) {
  result_state <- react1_lab_result_state(result, survey, "RESULT")
  ct1 <- as_numeric_raw(ct1)
  ct2 <- as_numeric_raw(ct2)

  ifelse(
    is.na(result_state) | result_state == "missing", NA_integer_,
    ifelse(
      result_state == "negative", 0L,
      ifelse(
        result_state == "positive",
        ifelse(
          ct1 > 0 & ct2 > 0, 1L,
          ifelse(ct1 > 0 & ct1 < threshold, 1L, 0L)
        ),
        NA_integer_
      )
    )
  )
}

derive_react1_estbinres <- function(raw_data) {
  if (!is.data.frame(raw_data) || !("survey_id" %in% names(raw_data))) {
    stop(
      "`raw_data` must be result$raw_data and contain `survey_id`.",
      call. = FALSE
    )
  }

  survey <- as.character(raw_data$survey_id)
  react1 <- survey %in% sprintf("REACT1_R%02d", 1:19)
  output <- rep(NA_integer_, nrow(raw_data))

  if (!any(react1)) {
    return(output)
  }

  round_1 <- survey == "REACT1_R01"
  require_columns_for(
    raw_data,
    round_1,
    c("RESULT", "LAB", "CT_VALUE1", "CT_VALUE2"),
    "`estbinres` for REACT-1 Round 1"
  )

  if (any(round_1)) {
    result <- as.character(raw_data$RESULT[round_1])
    result_state <- react1_lab_result_state(
      result, survey[round_1], "RESULT"
    )
    lab <- as.character(raw_data$LAB[round_1])
    ct1 <- as_numeric_raw(raw_data$CT_VALUE1[round_1])
    ct2 <- as_numeric_raw(raw_data$CT_VALUE2[round_1])

    output[round_1] <- ifelse(
      is.na(result_state) | result_state == "missing", NA_integer_,
      ifelse(
        result_state == "negative", 0L,
        ifelse(
          result_state == "positive" & lab != "Eurofin", 1L,
          ifelse(
            ct1 > 0 & ct2 > 0, 1L,
            ifelse(ct1 > 0 & ct1 < 37, 1L, 0L)
          )
        )
      )
    )
  }

  round_5 <- survey == "REACT1_R05"
  require_columns_for(
    raw_data,
    round_5,
    "FINALRESULT",
    "`estbinres` for REACT-1 Round 5"
  )

  if (any(round_5)) {
    final_result <- as.character(raw_data$FINALRESULT[round_5])
    final_state <- react1_lab_result_state(
      final_result, survey[round_5], "FINALRESULT"
    )
    output[round_5] <- ifelse(
      is.na(final_state) | final_state == "missing", NA_integer_,
      ifelse(final_state == "positive", 1L,
        ifelse(final_state == "negative", 0L, NA_integer_))
    )
  }

  early_rounds <- survey %in% sprintf("REACT1_R%02d", c(2:4, 6:7))
  require_columns_for(
    raw_data,
    early_rounds,
    c("RESULT", "CT_VALUE1", "CT_VALUE2"),
    "`estbinres` for REACT-1 Rounds 2-4 and 6-7"
  )

  if (any(early_rounds)) {
    output[early_rounds] <- legacy_ct_positive(
      raw_data$RESULT[early_rounds],
      raw_data$CT_VALUE1[early_rounds],
      raw_data$CT_VALUE2[early_rounds],
      survey[early_rounds]
    )
  }

  later_rounds <- survey %in% sprintf("REACT1_R%02d", 8:19)
  require_columns_for(
    raw_data,
    later_rounds,
    c("RESULT", "NGENE_CTVALUE", "EGENE_CTVALUE"),
    "`estbinres` for REACT-1 Rounds 8-19"
  )

  if (any(later_rounds)) {
    output[later_rounds] <- legacy_ct_positive(
      raw_data$RESULT[later_rounds],
      raw_data$NGENE_CTVALUE[later_rounds],
      raw_data$EGENE_CTVALUE[later_rounds],
      survey[later_rounds]
    )
  }

  output
}

derive_react1_imd_quintile <- function(raw_data) {
  survey <- as.character(raw_data$survey_id)
  rows <- survey %in% sprintf("REACT1_R%02d", 1:19)
  require_columns_for(raw_data, rows, "IMD_DECILE", "REACT-1 IMD quintile")

  output <- rep(NA_integer_, nrow(raw_data))
  if (any(rows)) {
    decile <- as_numeric_raw(raw_data$IMD_DECILE[rows])
    valid <- !is.na(decile) & decile %in% 1:10 & decile == floor(decile)
    values <- rep(NA_integer_, sum(rows))
    values[valid] <- as.integer(ceiling(decile[valid] / 2))
    output[rows] <- values
  }

  output
}

derive_react1_age_group <- function(raw_data) {
  survey <- as.character(raw_data$survey_id)
  rows <- survey %in% sprintf("REACT1_R%02d", 1:19)
  require_columns_for(raw_data, rows, "U_AGE", "REACT-1 age groups")

  labels <- c(
    "5-12", "13-17", "18-24", "25-34",
    "35-44", "45-54", "55-64", "65+"
  )
  output <- factor(rep(NA_character_, nrow(raw_data)), levels = labels)

  if (any(rows)) {
    age <- as_numeric_raw(raw_data$U_AGE[rows])
    output[rows] <- cut(
      age,
      breaks = c(4, 12, 17, 24, 34, 44, 54, 64, Inf),
      labels = labels,
      right = TRUE
    )
  }

  output
}

derive_react1_household_size <- function(raw_data) {
  survey <- as.character(raw_data$survey_id)
  rows <- survey %in% sprintf("REACT1_R%02d", 1:19)
  columns <- c("NADULTS", "NADULTS1", "NCHILD", "NCHILD1")
  require_columns_for(raw_data, rows, columns, "REACT-1 household size")

  output <- rep(NA_integer_, nrow(raw_data))
  if (any(rows)) {
    counts <- lapply(raw_data[rows, columns, drop = FALSE], as_numeric_raw)
    counts <- as.data.frame(counts, check.names = FALSE)
    counts[] <- lapply(counts, function(value) {
      value[!is.finite(value) | value < 0] <- NA_real_
      value
    })

    usable <- rowSums(!is.na(counts))
    values <- rowSums(counts, na.rm = TRUE)
    values[usable == 0L | values <= 0 | values != floor(values)] <- NA_real_
    output[rows] <- as.integer(values)

    both_adult_routes <- !is.na(counts$NADULTS) & !is.na(counts$NADULTS1)
    both_child_routes <- !is.na(counts$NCHILD) & !is.na(counts$NCHILD1)
    if (any(both_adult_routes | both_child_routes)) {
      warning(
        "Some rows populate both routed household-count alternatives; ",
        "the historical recipe sums them. Check these rows before analysis.",
        call. = FALSE
      )
    }
  }

  output
}

derive_react1_bmi <- function(raw_data) {
  survey <- as.character(raw_data$survey_id)
  rows <- survey %in% sprintf("REACT1_R%02d", 2:19)
  columns <- c(
    "WEIGHT_KG", "WEIGHT_S", "WEIGHT_P",
    "HEIGHT_CM", "HEIGHT_FEET", "HEIGHT_INCHES"
  )
  require_columns_for(raw_data, rows, columns, "REACT-1 BMI")

  bmi <- rep(NA_real_, nrow(raw_data))
  if (any(rows)) {
    kg <- as_numeric_raw(raw_data$WEIGHT_KG[rows])
    stones <- as_numeric_raw(raw_data$WEIGHT_S[rows])
    pounds <- as_numeric_raw(raw_data$WEIGHT_P[rows])
    imperial_kg <- (stones * 14 + pounds) / 2.2
    use_imperial_weight <- is.na(kg) | kg <= 0
    kg[use_imperial_weight] <- imperial_kg[use_imperial_weight]
    kg[!is.finite(kg) | kg <= 0] <- NA_real_

    cm <- as_numeric_raw(raw_data$HEIGHT_CM[rows])
    feet <- as_numeric_raw(raw_data$HEIGHT_FEET[rows])
    inches <- as_numeric_raw(raw_data$HEIGHT_INCHES[rows])
    imperial_cm <- (feet * 12 + inches) * 2.54
    use_imperial_height <- is.na(cm) | cm <= 0
    cm[use_imperial_height] <- imperial_cm[use_imperial_height]
    cm[!is.finite(cm) | cm <= 0] <- NA_real_

    values <- round(kg / (cm / 100)^2, 2)
    values[!is.finite(values)] <- NA_real_
    bmi[rows] <- values
  }

  bmi
}

derive_adult_bmi_category <- function(bmi, age) {
  bmi <- as_numeric_raw(bmi)
  age <- as_numeric_raw(age)
  labels <- c("Underweight", "Normal weight", "Overweight", "Obese")
  output <- factor(rep(NA_character_, length(bmi)), levels = labels)
  adult <- !is.na(age) & age >= 18 & !is.na(bmi)

  output[adult] <- cut(
    bmi[adult],
    breaks = c(-Inf, 18.5, 25, 30, Inf),
    labels = labels,
    right = FALSE
  )

  output
}
```

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
analysis_data$estbinres <- derive_react1_estbinres(raw)

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
analysis_data$imd_quintile <- derive_react1_imd_quintile(raw)

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
analysis_data$age_group <- derive_react1_age_group(raw)
```

`U_AGE`, rather than `AGE`, is the field used by the historical scripts.

## Household size

The legacy scripts cleaned negative codes and summed `NADULTS`, `NADULTS1`,
`NCHILD`, and `NCHILD1`. The fields are routed alternatives: some versions
include the participant and others do not. The historical calculation assumes
that the questionnaire routing was applied correctly.

```r
analysis_data$hh_size <- derive_react1_household_size(raw)

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
analysis_data$bmi <- derive_react1_bmi(raw)
analysis_data$adult_bmi_category <- derive_adult_bmi_category(
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

## Add the new columns to the cleaned table

The preparation example verifies that `result$data` and `result$raw_data` have
the same observation order. If an analysis has reordered either table, join by
`observation_id` instead of relying on row position:

```r
derived <- data.frame(
  observation_id = raw$observation_id,
  estbinres = derive_react1_estbinres(raw),
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
have been reviewed separately. None of the REACT-1 recipes on this page should
be applied to REACT-2 without an independent review of the corresponding source
fields and scientific definitions.

## Provenance

These recipes were transcribed from the historical REACT-1 round wrangling
scripts `0_data_wrangling_round1.R` through
`0_data_wrangling_round19.R` and `00_react1_data_combine.R`. The source audit
distinguished active code from commented-out proposals and identified
round-specific inputs, external dependencies, and known implementation defects.

The [REACT metadata wiki](https://mathzero.github.io/react_wiki/) remains the
source for raw field wording, response coding, availability, and reviewed
cross-round harmonisation.
