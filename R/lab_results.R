.lab_result_common_admin_codes <- function() {
  c("", "-91", "-92", "-77", "-66", "-99", "-555")
}

.lab_result_occurrence_support_ids <- function() {
  c(
    occ_3d8dfcbd9554c0afc41ef7e2 =
      "react1_pcr_lab_result_r02_final_v1",
    occ_654a0c1cc8cdb5d263af8bad =
      "react1_pcr_lab_result_r11_result_v1",
    occ_02fe3a74aee39c3150210a30 =
      "react1_pcr_lab_result_r13_result_v1"
  )
}

.lab_result_occurrence_id <- function(round_id, variable, dictionary) {
  rows <- dictionary$occurrences[
    dictionary$occurrences$round_id == round_id &
      dictionary$occurrences$variable == variable,
    "occurrence_id",
    drop = TRUE
  ]
  if (length(rows) != 1L) return(NA_character_)
  rows[[1L]]
}

.lab_result_support <- function(occurrence_id, dictionary = react_dictionary()) {
  overrides <- dictionary$synthetic_profile_overrides
  override <- overrides[
    overrides$occurrence_id == occurrence_id &
      overrides$review_state == "approved",
    ,
    drop = FALSE
  ]
  if (nrow(override) != 1L) {
    stop(
      "No unique approved laboratory-result support resolves occurrence `",
      occurrence_id, "`.",
      call. = FALSE
    )
  }
  reviewed <- .lab_result_occurrence_support_ids()
  if (occurrence_id %in% names(reviewed) &&
      !identical(override$support_id[[1L]], unname(reviewed[[occurrence_id]]))) {
    stop(
      "Occurrence `", occurrence_id,
      "` does not resolve its approved occurrence-specific laboratory support.",
      call. = FALSE
    )
  }
  supports <- dictionary$synthetic_public_supports
  rows <- supports[
    supports$support_id == override$support_id[[1L]] &
      supports$review_state == "approved",
    ,
    drop = FALSE
  ]
  rows <- rows[order(suppressWarnings(as.integer(rows$sort_order))), , drop = FALSE]
  required <- c("raw_value", "label", "outcome_state")
  allowed_states <- c("detected", "negative", "missing")
  if (!nrow(rows) || !all(required %in% names(rows)) ||
      anyNA(rows$raw_value) || anyDuplicated(rows$raw_value) ||
      anyNA(rows$outcome_state) || any(!(rows$outcome_state %in% allowed_states))) {
    stop(
      "Approved laboratory-result support `", override$support_id[[1L]],
      "` has an incomplete or invalid exact outcome-state mapping.",
      call. = FALSE
    )
  }
  rows
}

.lab_result_value_state <- function(value, occurrence_id,
                                    dictionary = react_dictionary()) {
  text <- as.character(value)
  support <- .lab_result_support(occurrence_id, dictionary)
  state <- unname(stats::setNames(
    support$outcome_state, support$raw_value
  )[text])
  state[is.na(value) | text %in% .lab_result_common_admin_codes()] <- "missing"
  state[is.na(state)] <- "unknown"
  state
}

.lab_result_field_state <- function(value, round_id, variable,
                                    dictionary = react_dictionary()) {
  occurrence_id <- .lab_result_occurrence_id(round_id, variable, dictionary)
  if (is.na(occurrence_id)) return(rep("unknown", length(value)))
  .lab_result_value_state(value, occurrence_id, dictionary)
}

.lab_result_missing <- function(value, round_id, variable,
                                dictionary = react_dictionary()) {
  .lab_result_field_state(value, round_id, variable, dictionary) == "missing"
}

.lab_result_unknown <- function(value, round_id, variable,
                                dictionary = react_dictionary()) {
  .lab_result_field_state(value, round_id, variable, dictionary) == "unknown"
}
