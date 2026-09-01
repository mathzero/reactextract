.test_rc14_lab_dictionary <- function() {
  dictionary <- react_dictionary()
  supports <- dictionary$synthetic_public_supports
  expected <- .lab_result_occurrence_support_ids()
  observed <- stats::setNames(
    dictionary$synthetic_profile_overrides$support_id[
      match(names(expected), dictionary$synthetic_profile_overrides$occurrence_id)
    ],
    names(expected)
  )
  if (identical(observed, expected) && "outcome_state" %in% names(supports)) {
    return(dictionary)
  }
  if (!"outcome_state" %in% names(supports)) supports$outcome_state <- ""
  generic <- supports$support_id == "react1_pcr_lab_result_v1"
  state <- c(
    Detected = "detected", `Not Detected` = "negative",
    Void = "missing", ` ` = "missing"
  )
  supports$outcome_state[generic] <- unname(state[supports$raw_value[generic]])

  special <- data.frame(
    occurrence_id = c(
      "occ_3d8dfcbd9554c0afc41ef7e2",
      "occ_654a0c1cc8cdb5d263af8bad",
      "occ_02fe3a74aee39c3150210a30"
    ),
    support_id = c(
      "react1_pcr_lab_result_r02_final_v1",
      "react1_pcr_lab_result_r11_result_v1",
      "react1_pcr_lab_result_r13_result_v1"
    ),
    raw_value = c("Rejected", "negative", "ambiguous"),
    label = c(
      "Rejected laboratory result (missing/non-evaluable)",
      "Negative laboratory result",
      "Ambiguous laboratory result (missing/non-evaluable)"
    ),
    outcome_state = c("missing", "negative", "missing"),
    stringsAsFactors = FALSE
  )
  added <- lapply(seq_len(nrow(special)), function(index) {
    base <- supports[generic, , drop = FALSE]
    base$support_id <- special$support_id[[index]]
    extra <- base[1L, , drop = FALSE]
    extra$raw_value <- special$raw_value[[index]]
    extra$label <- special$label[[index]]
    extra$sort_order <- as.character(max(as.integer(base$sort_order)) + 1L)
    extra$outcome_state <- special$outcome_state[[index]]
    note_column <- intersect(c("note", "notes"), names(extra))[[1L]]
    extra[[note_column]] <- "Test-only occurrence-specific exact support."
    rbind(base, extra)
  })
  dictionary$synthetic_public_supports <- do.call(rbind, c(list(supports), added))
  for (index in seq_len(nrow(special))) {
    selected <- dictionary$synthetic_profile_overrides$occurrence_id ==
      special$occurrence_id[[index]]
    dictionary$synthetic_profile_overrides$support_id[selected] <-
      special$support_id[[index]]
  }
  dictionary
}
