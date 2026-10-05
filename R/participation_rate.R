# D13: an explicit modelling assumption, never a replacement for protected counts.
.participation_rate_policy <- "below_10_shared_count_assume_5_over_approved_v5_rounded_round_total"

.participation_rate <- function(source, round) {
  p <- source$profile$participation
  rows <- p$counts[p$count_index[["individual_participation"]], , drop = FALSE]
  rows <- rows[rows$round_id == round, , drop = FALSE]
  unavailable <- function(reason) list(status = "unestimated", probability = NA_real_,
    lower = NA_real_, upper = NA_real_, assumed_count = NA_real_, denominator = NA_real_,
    denominator_source = "unavailable", reason = reason)
  states <- c("shared_nonresponse", "not_shared_nonresponse")
  if (nrow(rows) != 2L || !setequal(rows$state, states) || anyDuplicated(rows$state))
    return(unavailable("incomplete_participation_states"))
  shared <- match("shared_nonresponse", rows$state)
  if (all(!rows$suppressed) && all(is.finite(rows$count)) && sum(rows$count) > 0) {
    return(list(status = "estimated", probability = rows$count[shared] / sum(rows$count),
      lower = NA_real_, upper = NA_real_, assumed_count = NA_real_, denominator = sum(rows$count),
      denominator_source = "released_participation_counts", reason = ""))
  }
  # A complementary cell can be large. Only an explicit primary suppression
  # label on the SHARED state establishes the 0..9 bound used by this policy.
  if (!"protection" %in% names(rows) || is.na(rows$protection[shared]) ||
      !isTRUE(rows$suppressed[shared]) || !is.na(rows$count[shared]) ||
      rows$protection[shared] != "below_10" ||
      !identical(unname(p$metadata["minimum_cell"]), "10"))
    return(unavailable("shared_count_not_explicitly_below_10"))
  other <- rows[-shared, , drop = FALSE]
  if (!isTRUE((isTRUE(other$suppressed) && is.na(other$count) && identical(other$protection, "linked_complement")) ||
        (identical(other$suppressed, FALSE) && is.finite(other$count) && other$count >= 10)))
    return(unavailable("unsupported_complement_state"))
  # Both profilers enumerate the full round view, not just respondents. The
  # approved older total is a denominator proxy: a stable historical snapshot is
  # assumed, not proven by differencing releases or summing protected cells.
  m <- .synthetic_profile_metadata(source$profile)
  if (!identical(unname(m["approved_profile_manifest_sha256"]), .approved_profile_manifest_sha256) ||
      !identical(unname(m["dictionary_manifest_sha256"]), unname(p$metadata["dictionary_manifest_sha256"])) ||
      !identical(unname(m["count_rounding"]), "5"))
    return(unavailable("denominator_scope_not_approved"))
  total <- source$profile$round_denominators
  if (!is.data.frame(total) || !all(c("round_id", "count", "suppressed") %in% names(total)))
    return(unavailable("round_denominator_unavailable"))
  total <- total[!is.na(total$round_id) & total$round_id == round, , drop = FALSE]
  if (nrow(total) != 1L) return(unavailable("round_denominator_unavailable"))
  n <- suppressWarnings(as.numeric(total$count))
  if (!identical(as.character(total$suppressed), "FALSE") ||
      !is.finite(n) || n < 10 || n %% 5 != 0)
    return(unavailable("round_denominator_unavailable"))
  list(status = "assumed_below_10", probability = 5 / n,
    lower = 0, upper = min(1, 9 / (n - 2)), assumed_count = 5,
    denominator = n, denominator_source = "v5_rounded_total_same_scope_stable_snapshot_assumption", reason = "")
}

.participation_rate_description <- function(rate, round) {
  number <- function(x) if (is.na(x)) "unavailable" else format(x, digits = 10, scientific = FALSE, trim = TRUE)
  paste0(round, ":", rate$status, ":rate=", number(rate$probability),
    ":assumed_count=", number(rate$assumed_count), ":denominator=", number(rate$denominator),
    ":lower=", number(rate$lower), ":upper=", number(rate$upper),
    ":denominator_source=", rate$denominator_source, ":reason=", rate$reason)
}
