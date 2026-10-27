#' Derive common REACT analysis variables
#'
#' These functions add widely used analysis variables to an extraction. They
#' are not applied automatically because each one encodes an analysis decision
#' rather than cleaning. Each function reads exact raw fields, so pass
#' `result$raw_data` (or the whole extraction result) and assign the returned
#' vector to `result$data`, which has the same row order.
#'
#' * `react_derive_estbinres()`: established REACT-1 PCR positivity (1 positive,
#'   0 negative, `NA` missing or non-evaluable), using the historical
#'   round-specific rules for rounds 1-19.
#' * `react_derive_igg()`: REACT-2 IgG antibody positivity from `NEWRESULT`
#'   (codes 2-3 positive, 0-1 negative, 4-7 non-evaluable and therefore `NA`).
#' * `react_derive_age_group()`: fixed REACT-1 age groups from NHS-derived
#'   `U_AGE` (5-12, 13-17, 18-24, 25-34, 35-44, 45-54, 55-64, 65+).
#' * `react_derive_imd_quintile()`: REACT-1 IMD quintile (1 most deprived) from
#'   `IMD_DECILE`.
#' * `react_derive_household_size()`: REACT-1 household size, the historical
#'   sum of `NADULTS`, `NADULTS1`, `NCHILD` and `NCHILD1`.
#' * `react_derive_bmi()`: REACT-1 BMI (rounds 2-19) from metric or imperial
#'   height and weight.
#' * `react_bmi_category()`: adult BMI category (from age 18).
#'
#' Rows from studies or rounds a recipe does not cover are returned as `NA`.
#' If covered rows are present but a required raw field was not extracted, the
#' function stops and names the missing fields. The full definitions,
#' historical provenance and limitations are in the package's
#' `DERIVED_VARIABLES.md` guide.
#'
#' @param x The `raw_data` table from [react_extract()], or the extraction
#'   result itself.
#' @param bmi Numeric BMI values, for example from `react_derive_bmi()`.
#' @param age Numeric ages in years, for example `raw_data$U_AGE`.
#' @return A vector with one value per row of `x` (or of `bmi`): integers for
#'   indicators, quintiles and household size, numeric BMI, and factors for age
#'   group and BMI category.
#' @examples
#' raw <- data.frame(
#'   survey_id = c("REACT1_R08", "REACT1_R08", "REACT2_S5_R05"),
#'   RESULT = c("Detected", "Not Detected", NA),
#'   NGENE_CTVALUE = c(25, 0, NA),
#'   EGENE_CTVALUE = c(27, 0, NA),
#'   NEWRESULT = c(NA, NA, 2),
#'   U_AGE = c(8, 47, 70)
#' )
#' react_derive_estbinres(raw)
#' react_derive_igg(raw)
#' react_derive_age_group(raw)
#' @name react_derived_variables
NULL

.react1_surveys <- function(rounds = 1:19) sprintf("REACT1_R%02d", rounds)

.as_numeric_raw <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

.derived_raw_data <- function(x) {
  if (inherits(x, "react_extract_result") ||
      (is.list(x) && !is.data.frame(x) && is.data.frame(x$raw_data))) {
    x <- x$raw_data
  }
  if (!is.data.frame(x) || !("survey_id" %in% names(x))) {
    stop(
      "`x` must be `result$raw_data` (or the extraction result) and contain `survey_id`.",
      call. = FALSE
    )
  }
  x
}

.require_columns_for <- function(data, rows, columns, recipe) {
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

.react1_lab_result_state <- function(result, survey, field = "RESULT") {
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

.legacy_ct_positive <- function(result, ct1, ct2, survey, threshold = 37) {
  result_state <- .react1_lab_result_state(result, survey, "RESULT")
  ct1 <- .as_numeric_raw(ct1)
  ct2 <- .as_numeric_raw(ct2)

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

#' @rdname react_derived_variables
#' @export
react_derive_estbinres <- function(x) {
  raw_data <- .derived_raw_data(x)
  survey <- as.character(raw_data$survey_id)
  output <- rep(NA_integer_, nrow(raw_data))
  if (!any(survey %in% .react1_surveys())) {
    return(output)
  }

  round_1 <- survey == "REACT1_R01"
  .require_columns_for(
    raw_data, round_1, c("RESULT", "LAB", "CT_VALUE1", "CT_VALUE2"),
    "`estbinres` for REACT-1 Round 1"
  )
  if (any(round_1)) {
    result <- as.character(raw_data$RESULT[round_1])
    result_state <- .react1_lab_result_state(result, survey[round_1], "RESULT")
    lab <- as.character(raw_data$LAB[round_1])
    ct1 <- .as_numeric_raw(raw_data$CT_VALUE1[round_1])
    ct2 <- .as_numeric_raw(raw_data$CT_VALUE2[round_1])
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
  .require_columns_for(
    raw_data, round_5, "FINALRESULT", "`estbinres` for REACT-1 Round 5"
  )
  if (any(round_5)) {
    final_state <- .react1_lab_result_state(
      raw_data$FINALRESULT[round_5], survey[round_5], "FINALRESULT"
    )
    output[round_5] <- ifelse(
      is.na(final_state) | final_state == "missing", NA_integer_,
      ifelse(final_state == "positive", 1L,
        ifelse(final_state == "negative", 0L, NA_integer_))
    )
  }

  early_rounds <- survey %in% .react1_surveys(c(2:4, 6:7))
  .require_columns_for(
    raw_data, early_rounds, c("RESULT", "CT_VALUE1", "CT_VALUE2"),
    "`estbinres` for REACT-1 Rounds 2-4 and 6-7"
  )
  if (any(early_rounds)) {
    output[early_rounds] <- .legacy_ct_positive(
      raw_data$RESULT[early_rounds],
      raw_data$CT_VALUE1[early_rounds],
      raw_data$CT_VALUE2[early_rounds],
      survey[early_rounds]
    )
  }

  later_rounds <- survey %in% .react1_surveys(8:19)
  .require_columns_for(
    raw_data, later_rounds, c("RESULT", "NGENE_CTVALUE", "EGENE_CTVALUE"),
    "`estbinres` for REACT-1 Rounds 8-19"
  )
  if (any(later_rounds)) {
    output[later_rounds] <- .legacy_ct_positive(
      raw_data$RESULT[later_rounds],
      raw_data$NGENE_CTVALUE[later_rounds],
      raw_data$EGENE_CTVALUE[later_rounds],
      survey[later_rounds]
    )
  }

  output
}

#' @rdname react_derived_variables
#' @export
react_derive_igg <- function(x) {
  raw_data <- .derived_raw_data(x)
  rows <- startsWith(as.character(raw_data$survey_id), "REACT2_")
  .require_columns_for(raw_data, rows, "NEWRESULT", "REACT-2 IgG positivity")

  output <- rep(NA_integer_, nrow(raw_data))
  if (any(rows)) {
    result <- .as_numeric_raw(raw_data$NEWRESULT[rows])
    output[rows] <- ifelse(result %in% c(2, 3), 1L,
      ifelse(result %in% c(0, 1), 0L, NA_integer_))
  }
  output
}

#' @rdname react_derived_variables
#' @export
react_derive_imd_quintile <- function(x) {
  raw_data <- .derived_raw_data(x)
  rows <- as.character(raw_data$survey_id) %in% .react1_surveys()
  .require_columns_for(raw_data, rows, "IMD_DECILE", "REACT-1 IMD quintile")

  output <- rep(NA_integer_, nrow(raw_data))
  if (any(rows)) {
    decile <- .as_numeric_raw(raw_data$IMD_DECILE[rows])
    valid <- !is.na(decile) & decile %in% 1:10 & decile == floor(decile)
    values <- rep(NA_integer_, sum(rows))
    values[valid] <- as.integer(ceiling(decile[valid] / 2))
    output[rows] <- values
  }
  output
}

#' @rdname react_derived_variables
#' @export
react_derive_age_group <- function(x) {
  raw_data <- .derived_raw_data(x)
  rows <- as.character(raw_data$survey_id) %in% .react1_surveys()
  .require_columns_for(raw_data, rows, "U_AGE", "REACT-1 age groups")

  labels <- c("5-12", "13-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  output <- factor(rep(NA_character_, nrow(raw_data)), levels = labels)
  if (any(rows)) {
    age <- .as_numeric_raw(raw_data$U_AGE[rows])
    output[rows] <- cut(
      age,
      breaks = c(4, 12, 17, 24, 34, 44, 54, 64, Inf),
      labels = labels,
      right = TRUE
    )
  }
  output
}

#' @rdname react_derived_variables
#' @export
react_derive_household_size <- function(x) {
  raw_data <- .derived_raw_data(x)
  rows <- as.character(raw_data$survey_id) %in% .react1_surveys()
  columns <- c("NADULTS", "NADULTS1", "NCHILD", "NCHILD1")
  .require_columns_for(raw_data, rows, columns, "REACT-1 household size")

  output <- rep(NA_integer_, nrow(raw_data))
  if (any(rows)) {
    counts <- lapply(raw_data[rows, columns, drop = FALSE], .as_numeric_raw)
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

#' @rdname react_derived_variables
#' @export
react_derive_bmi <- function(x) {
  raw_data <- .derived_raw_data(x)
  rows <- as.character(raw_data$survey_id) %in% .react1_surveys(2:19)
  columns <- c(
    "WEIGHT_KG", "WEIGHT_S", "WEIGHT_P",
    "HEIGHT_CM", "HEIGHT_FEET", "HEIGHT_INCHES"
  )
  .require_columns_for(raw_data, rows, columns, "REACT-1 BMI")

  bmi <- rep(NA_real_, nrow(raw_data))
  if (any(rows)) {
    kg <- .as_numeric_raw(raw_data$WEIGHT_KG[rows])
    stones <- .as_numeric_raw(raw_data$WEIGHT_S[rows])
    pounds <- .as_numeric_raw(raw_data$WEIGHT_P[rows])
    imperial_kg <- (stones * 14 + pounds) / 2.2
    use_imperial_weight <- is.na(kg) | kg <= 0
    kg[use_imperial_weight] <- imperial_kg[use_imperial_weight]
    kg[!is.finite(kg) | kg <= 0] <- NA_real_

    cm <- .as_numeric_raw(raw_data$HEIGHT_CM[rows])
    feet <- .as_numeric_raw(raw_data$HEIGHT_FEET[rows])
    inches <- .as_numeric_raw(raw_data$HEIGHT_INCHES[rows])
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

#' @rdname react_derived_variables
#' @export
react_bmi_category <- function(bmi, age) {
  bmi <- .as_numeric_raw(bmi)
  age <- .as_numeric_raw(age)
  if (length(bmi) != length(age)) {
    stop("`bmi` and `age` must have the same length.", call. = FALSE)
  }
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
