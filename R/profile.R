.analysis_values <- function(result) {
  harmonised <- result$harmonised_values
  raw <- result$raw_values
  # A reviewed grouped transform can combine several exact source occurrences.
  # Expand that provenance field before deciding which raw rows are already
  # represented in the cleaned profile.
  harmonised_occurrences <- unique(unlist(
    strsplit(harmonised$source_occurrence_id, "|", fixed = TRUE),
    use.names = FALSE
  ))
  raw <- raw[!(raw$occurrence_id %in% harmonised_occurrences), , drop = FALSE]

  harmonised_values <- if (nrow(harmonised) == 0L) {
    data.frame()
  } else {
    data.frame(
      observation_id = harmonised$observation_id,
      round_id = harmonised$round_id,
      feature_id = harmonised$output_column,
      concept_id = harmonised$concept_id,
      occurrence_id = harmonised$source_occurrence_id,
      profile_source = ifelse(
        harmonised$mapping_id == "",
        "source_preserved",
        "harmonised"
      ),
      missing_reason = harmonised$missing_reason,
      harmonised[c(
        "value_type", "value_logical", "value_integer", "value_double",
        "value_date", "value_character"
      )],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  raw_values <- if (nrow(raw) == 0L) {
    data.frame()
  } else {
    data.frame(
      observation_id = raw$observation_id,
      round_id = raw$round_id,
      feature_id = raw$occurrence_id,
      concept_id = raw$concept_id,
      occurrence_id = raw$occurrence_id,
      profile_source = "raw",
      missing_reason = ifelse(raw$source_is_missing, "input_missing", ""),
      raw[c(
        "value_type", "value_logical", "value_integer", "value_double",
        "value_date", "value_character"
      )],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  .bind_rows(list(harmonised_values, raw_values))
}

.feature_groups <- function(values) {
  if (nrow(values) == 0L) {
    return(list())
  }
  split(
    seq_len(nrow(values)),
    paste(values$feature_id, values$round_id, sep = "\r")
  )
}

.profile_identity <- function(data) {
  data.frame(
    feature_id = data$feature_id[[1]],
    concept_id = data$concept_id[[1]],
    occurrence_id = data$occurrence_id[[1]],
    round_id = data$round_id[[1]],
    profile_source = data$profile_source[[1]],
    value_type = data$value_type[[1]],
    stringsAsFactors = FALSE
  )
}

.observed_values <- function(data) {
  keep <- data$missing_reason == "" & .typed_nonmissing_count(data) == 1L
  list(data = data[keep, , drop = FALSE], text = .typed_to_character(data[keep, , drop = FALSE]))
}

.numeric_values <- function(data) {
  type <- data$value_type[[1]]
  if (type == "integer") {
    return(as.double(data$value_integer))
  }
  if (type == "double") {
    return(data$value_double)
  }
  if (type == "date") {
    return(as.double(data$value_date))
  }
  numeric()
}

.profile_histogram <- function(identity, values) {
  if (length(values) == 0L) {
    return(data.frame())
  }
  unique_values <- unique(values)
  if (length(unique_values) == 1L) {
    return(data.frame(
      identity,
      bin_lower = unique_values,
      bin_upper = unique_values,
      count = length(values),
      stringsAsFactors = FALSE
    ))
  }
  breaks <- unique(pretty(range(values), n = 10L))
  if (length(breaks) < 2L) {
    breaks <- range(values) + c(-0.5, 0.5)
  }
  histogram <- graphics::hist(values, breaks = breaks, plot = FALSE, include.lowest = TRUE)
  data.frame(
    identity[rep(1L, length(histogram$counts)), , drop = FALSE],
    bin_lower = histogram$breaks[-length(histogram$breaks)],
    bin_upper = histogram$breaks[-1L],
    count = as.integer(histogram$counts),
    stringsAsFactors = FALSE
  )
}

.profile_pairs_spec <- function(pairs) {
  if (is.null(pairs)) {
    return(data.frame(feature_x = character(), feature_y = character()))
  }
  if (is.matrix(pairs)) {
    pairs <- as.data.frame(pairs, stringsAsFactors = FALSE)
  }
  if (is.list(pairs) && !is.data.frame(pairs)) {
    pairs <- do.call(rbind, lapply(pairs, function(x) {
      if (length(x) != 2L) stop("Every requested pair must contain two feature IDs.", call. = FALSE)
      data.frame(feature_x = x[[1]], feature_y = x[[2]], stringsAsFactors = FALSE)
    }))
  }
  if (!is.data.frame(pairs) || ncol(pairs) < 2L) {
    stop("`pairs` must be NULL, a two-column data frame/matrix, or a list of pairs.", call. = FALSE)
  }
  out <- data.frame(
    feature_x = as.character(pairs[[1]]),
    feature_y = as.character(pairs[[2]]),
    stringsAsFactors = FALSE
  )
  if (anyNA(out) || any(!nzchar(out$feature_x)) || any(!nzchar(out$feature_y))) {
    stop("Requested pair feature IDs must be non-empty.", call. = FALSE)
  }
  unique(out)
}

.scalar_pair_values <- function(data) {
  observed <- .observed_values(data)$data
  if (anyDuplicated(observed$observation_id)) {
    stop(
      "Pairwise profiling requires one value per observation for feature `",
      data$feature_id[[1]],
      "` in `",
      data$round_id[[1]],
      "`.",
      call. = FALSE
    )
  }
  numeric_type <- observed$value_type %in% c("integer", "double", "date")
  if (nrow(observed) > 0L && all(numeric_type)) {
    values <- ifelse(
      observed$value_type == "integer",
      as.double(observed$value_integer),
      ifelse(
        observed$value_type == "double",
        observed$value_double,
        as.double(observed$value_date)
      )
    )
    kind <- "numeric"
  } else {
    values <- .typed_to_character(observed)
    kind <- "categorical"
  }
  data.frame(
    observation_id = observed$observation_id,
    value = values,
    kind = kind,
    stringsAsFactors = FALSE
  )
}

.profile_selected_pairs <- function(values, pairs) {
  correlations <- list()
  crosstabs <- list()
  grouped <- list()
  available <- unique(values$feature_id)
  unknown <- setdiff(unique(c(pairs$feature_x, pairs$feature_y)), available)
  if (length(unknown) > 0L) {
    stop("Unknown requested profile feature: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  used_correlation <- used_crosstab <- used_grouped <- 0L
  for (i in seq_len(nrow(pairs))) {
    rounds <- intersect(
      unique(values$round_id[values$feature_id == pairs$feature_x[[i]]]),
      unique(values$round_id[values$feature_id == pairs$feature_y[[i]]])
    )
    for (round_id in rounds) {
      x_data <- values[
        values$feature_id == pairs$feature_x[[i]] & values$round_id == round_id,
        , drop = FALSE
      ]
      y_data <- values[
        values$feature_id == pairs$feature_y[[i]] & values$round_id == round_id,
        , drop = FALSE
      ]
      x <- .scalar_pair_values(x_data)
      y <- .scalar_pair_values(y_data)
      joined <- merge(x, y, by = "observation_id", suffixes = c("_x", "_y"))
      if (nrow(joined) == 0L) next
      common <- data.frame(
        feature_x = pairs$feature_x[[i]],
        feature_y = pairs$feature_y[[i]],
        round_id = round_id,
        stringsAsFactors = FALSE
      )
      if (all(joined$kind_x == "numeric") && all(joined$kind_y == "numeric")) {
        used_correlation <- used_correlation + 1L
        correlations[[used_correlation]] <- data.frame(
          common,
          n = nrow(joined),
          correlation = if (nrow(joined) > 1L) stats::cor(joined$value_x, joined$value_y) else NA_real_,
          stringsAsFactors = FALSE
        )
      } else if (all(joined$kind_x == "categorical") && all(joined$kind_y == "categorical")) {
        counts <- as.data.frame(table(joined$value_x, joined$value_y), stringsAsFactors = FALSE)
        names(counts) <- c("value_x", "value_y", "count")
        counts <- counts[counts$count > 0L, , drop = FALSE]
        used_crosstab <- used_crosstab + 1L
        crosstabs[[used_crosstab]] <- data.frame(
          common[rep(1L, nrow(counts)), , drop = FALSE],
          counts,
          stringsAsFactors = FALSE
        )
      } else {
        numeric_is_x <- all(joined$kind_x == "numeric")
        category <- if (numeric_is_x) joined$value_y else joined$value_x
        numeric <- if (numeric_is_x) joined$value_x else joined$value_y
        groups <- split(numeric, category)
        used_grouped <- used_grouped + 1L
        grouped[[used_grouped]] <- .bind_rows(lapply(names(groups), function(level) {
          x_group <- groups[[level]]
          data.frame(
            common,
            numeric_feature = if (numeric_is_x) pairs$feature_x[[i]] else pairs$feature_y[[i]],
            category_feature = if (numeric_is_x) pairs$feature_y[[i]] else pairs$feature_x[[i]],
            category = level,
            n = length(x_group),
            mean = mean(x_group),
            sd = if (length(x_group) > 1L) stats::sd(x_group) else NA_real_,
            stringsAsFactors = FALSE
          )
        }))
      }
    }
  }
  list(
    correlations = .bind_rows(correlations),
    crosstabs = .bind_rows(crosstabs),
    grouped = .bind_rows(grouped)
  )
}

#' Build aggregate distribution profiles inside the enclave
#'
#' Approved transforms are used where available; otherwise source-preserving
#' cleaned values are profiled. Uncoded character text is represented only by
#' length bands and is never retained in the profile.
#'
#' @param result A result returned by [react_extract()].
#' @param pairs Optional explicit pairs of feature IDs for pairwise summaries.
#' @return Aggregate profile tables with no respondent identifiers.
#' @export
react_profile <- function(result, pairs = NULL) {
  if (!is.list(result) || !("manifest" %in% names(result))) {
    stop("`result` must be returned by `react_extract()`.", call. = FALSE)
  }
  if (!all(c("raw_values", "harmonised_values") %in% names(result))) {
    stop(
      "`react_profile()` requires detailed long values. Rerun `react_extract()` ",
      "with `output = \"long\"` or `output = \"both\"`. For enclave-scale ",
      "profiling, use `react_profile_source()`.",
      call. = FALSE
    )
  }
  values <- .analysis_values(result)
  dictionary <- react_dictionary()
  option_rows <- dictionary$response_options
  substantive_option <- !(
    option_rows$return_value == "NA" & option_rows$display_value == "NA"
  )
  coded_by_occurrence <- tapply(
    substantive_option,
    option_rows$occurrence_id,
    any
  )
  coded_occurrences <- names(coded_by_occurrence)[coded_by_occurrence]
  missingness <- categorical <- numeric <- histograms <- text_lengths <- list()
  used_missing <- used_categorical <- used_numeric <- used_hist <- used_text <- 0L

  for (indices in .feature_groups(values)) {
    data <- values[indices, , drop = FALSE]
    identity <- .profile_identity(data)
    statuses <- ifelse(data$missing_reason == "", "observed", data$missing_reason)
    status_counts <- as.data.frame(table(statuses), stringsAsFactors = FALSE)
    names(status_counts) <- c("status", "count")
    used_missing <- used_missing + 1L
    missingness[[used_missing]] <- data.frame(
      identity[rep(1L, nrow(status_counts)), , drop = FALSE],
      status_counts,
      stringsAsFactors = FALSE
    )

    observed <- .observed_values(data)
    if (nrow(observed$data) == 0L) next
    observed_type <- observed$data$value_type[[1]]
    is_uncoded_text <- identity$profile_source %in% c("raw", "source_preserved") &&
      observed_type == "character" &&
      !(identity$occurrence_id %in% coded_occurrences)
    if (is_uncoded_text) {
      lengths <- nchar(observed$text, type = "chars", allowNA = TRUE)
      bands <- cut(
        lengths,
        breaks = c(-1, 0, 10, 25, 50, 100, Inf),
        labels = c("0", "1-10", "11-25", "26-50", "51-100", "101+"),
        right = TRUE
      )
      counts <- as.data.frame(table(bands), stringsAsFactors = FALSE)
      names(counts) <- c("length_band", "count")
      counts <- counts[counts$count > 0L, , drop = FALSE]
      used_text <- used_text + 1L
      text_lengths[[used_text]] <- data.frame(
        identity[rep(1L, nrow(counts)), , drop = FALSE],
        counts,
        stringsAsFactors = FALSE
      )
      next
    }
    numeric_value <- .numeric_values(observed$data)
    treat_numeric <- length(numeric_value) > 0L && length(unique(numeric_value)) > 20L
    if (treat_numeric) {
      quantiles <- stats::quantile(
        numeric_value,
        probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
        names = FALSE,
        type = 7
      )
      used_numeric <- used_numeric + 1L
      numeric[[used_numeric]] <- data.frame(
        identity,
        n = length(numeric_value),
        mean = mean(numeric_value),
        sd = if (length(numeric_value) > 1L) stats::sd(numeric_value) else NA_real_,
        q05 = quantiles[[1]],
        q25 = quantiles[[2]],
        q50 = quantiles[[3]],
        q75 = quantiles[[4]],
        q95 = quantiles[[5]],
        stringsAsFactors = FALSE
      )
      used_hist <- used_hist + 1L
      histograms[[used_hist]] <- .profile_histogram(identity, numeric_value)
    } else {
      category <- observed$text
      counts <- as.data.frame(table(category), stringsAsFactors = FALSE)
      names(counts) <- c("value", "count")
      counts <- counts[counts$count > 0L, , drop = FALSE]
      used_categorical <- used_categorical + 1L
      categorical[[used_categorical]] <- data.frame(
        identity[rep(1L, nrow(counts)), , drop = FALSE],
        counts,
        stringsAsFactors = FALSE
      )
    }
  }

  pair_spec <- .profile_pairs_spec(pairs)
  pair_profiles <- .profile_selected_pairs(values, pair_spec)
  version <- react_dictionary_version()
  list(
    metadata = data.frame(
      key = c(
        "profile_schema_version", "package_version", "dictionary_release",
        "dictionary_manifest_sha256", "status", "requested_pair_count"
      ),
      value = c(
        "1", as.character(utils::packageVersion("reactextract")),
        version$dictionary_release, version$manifest_sha256,
        "enclave_internal_unsuppressed", as.character(nrow(pair_spec))
      ),
      stringsAsFactors = FALSE
    ),
    missingness = .bind_rows(missingness),
    categorical = .bind_rows(categorical),
    numeric = .bind_rows(numeric),
    histograms = .bind_rows(histograms),
    text_lengths = .bind_rows(text_lengths),
    pair_correlations = pair_profiles$correlations,
    pair_crosstabs = pair_profiles$crosstabs,
    pair_grouped = pair_profiles$grouped
  )
}

#' Define disclosure-preparation defaults
#'
#' @param min_count Primary suppression threshold.
#' @param count_rounding Base used to round released counts.
#' @param statistic_digits Significant digits retained for non-count summaries.
#' @return A disclosure-preparation policy list.
#' @export
react_sdc_policy <- function(min_count = 10L, count_rounding = 5L,
                             statistic_digits = 3L) {
  min_count <- as.integer(min_count)
  count_rounding <- as.integer(count_rounding)
  statistic_digits <- as.integer(statistic_digits)
  if (anyNA(c(min_count, count_rounding, statistic_digits)) ||
      min_count < 1L || count_rounding < 1L || statistic_digits < 1L) {
    stop("Disclosure policy values must be positive integers.", call. = FALSE)
  }
  list(
    min_count = min_count,
    count_rounding = count_rounding,
    statistic_digits = statistic_digits,
    complementary_suppression = TRUE,
    raw_text = FALSE,
    exact_extrema = FALSE
  )
}

.sdc_count_table <- function(data, group_columns, policy, count_column = "count") {
  if (!is.data.frame(data) || nrow(data) == 0L) return(data)
  groups <- split(
    seq_len(nrow(data)),
    interaction(data[group_columns], drop = TRUE, lex.order = TRUE)
  )
  data$suppressed <- FALSE
  for (indices in groups) {
    counts <- as.numeric(data[[count_column]][indices])
    suppressed <- counts < policy$min_count
    if (sum(suppressed) == 1L && length(indices) > 1L) {
      candidates <- which(!suppressed)
      suppressed[candidates[[which.min(counts[candidates])]]] <- TRUE
    }
    data$suppressed[indices] <- suppressed
  }
  counts <- as.numeric(data[[count_column]])
  counts <- round(counts / policy$count_rounding) * policy$count_rounding
  counts[data$suppressed] <- NA_real_
  data[[count_column]] <- counts
  data
}

.sdc_summary_table <- function(data, policy) {
  if (!is.data.frame(data) || nrow(data) == 0L) return(data)
  data$suppressed <- data$n < policy$min_count
  if ("missing_n" %in% names(data)) {
    sensitive_missing <- data$missing_n > 0L & data$missing_n < policy$min_count
    data$suppressed <- data$suppressed | sensitive_missing
  }
  statistic_columns <- intersect(
    c("mean", "sd", "q05", "q25", "q50", "q75", "q95", "correlation"),
    names(data)
  )
  for (column in statistic_columns) {
    data[[column]] <- .round_significant(data[[column]], policy$statistic_digits)
    data[[column]][data$suppressed] <- NA_real_
  }
  data$n <- round(data$n / policy$count_rounding) * policy$count_rounding
  data$n[data$suppressed] <- NA_real_
  if ("missing_n" %in% names(data)) {
    data$missing_n <- round(data$missing_n / policy$count_rounding) * policy$count_rounding
    data$missing_n[data$suppressed] <- NA_real_
  }
  data
}

.sdc_issue_table <- function(data, policy) {
  if (!is.data.frame(data) || nrow(data) == 0L ||
      !"affected_count" %in% names(data)) return(data)
  counts <- suppressWarnings(as.numeric(data$affected_count))
  present <- !is.na(counts)
  released <- present & counts >= policy$min_count
  protected <- rep("", length(counts))
  protected[released] <- as.character(
    round(counts[released] / policy$count_rounding) * policy$count_rounding
  )
  data$affected_count <- protected
  data
}

#' Prepare aggregate profiles for enclave disclosure review
#'
#' This applies mechanical suppression and rounding but does not constitute
#' permission to remove output from the enclave.
#'
#' @param profile A profile returned by [react_profile()].
#' @param policy A policy returned by [react_sdc_policy()].
#' @return Suppressed and rounded aggregate tables marked as requiring review.
#' @export
react_prepare_profile_export <- function(profile, policy = react_sdc_policy()) {
  if (is.list(profile) && all(.profile_v2_required %in% names(profile))) {
    out <- profile
    out$round_denominators <- .sdc_count_table(
      out$round_denominators, "round_id", policy
    )
    out$missingness <- .sdc_count_table(
      out$missingness, c("occurrence_id", "round_id"), policy
    )
    out$categorical_counts <- .sdc_count_table(
      out$categorical_counts, c("occurrence_id", "round_id"), policy
    )
    out$numeric_bin_counts <- .sdc_count_table(
      out$numeric_bin_counts, c("occurrence_id", "round_id"), policy
    )
    out$text_presence <- .sdc_count_table(
      out$text_presence, c("occurrence_id", "round_id"), policy
    )
    out$routing_validation <- .sdc_count_table(
      out$routing_validation, c("routing_rule_id", "round_id"), policy
    )
    out$issues <- .sdc_issue_table(out$issues, policy)
    if (is.data.frame(out$overall_missingness)) {
      out$overall_missingness <- .sdc_count_table(
        out$overall_missingness, "distribution_group_id", policy
      )
    }
    if (is.data.frame(out$overall_categorical_counts)) {
      out$overall_categorical_counts <- .sdc_count_table(
        out$overall_categorical_counts, "distribution_group_id", policy
      )
    }
    if (is.data.frame(out$overall_numeric_bin_counts)) {
      out$overall_numeric_bin_counts <- .sdc_count_table(
        out$overall_numeric_bin_counts, "distribution_group_id", policy
      )
    }
    if (is.data.frame(out$overall_text_presence)) {
      out$overall_text_presence <- .sdc_count_table(
        out$overall_text_presence, "distribution_group_id", policy
      )
    }
    out$metadata <- rbind(
      out$metadata[!out$metadata$key %in% c(
        "status", "minimum_cell_count", "count_rounding",
        "complementary_suppression", "raw_text_included",
        "exact_extrema_included", "disclosure_approval"
      ), , drop = FALSE],
      data.frame(
        key = c(
          "status", "minimum_cell_count", "count_rounding",
          "complementary_suppression", "raw_text_included",
          "exact_extrema_included", "disclosure_approval"
        ),
        value = c(
          "requires_enclave_disclosure_review",
          as.character(policy$min_count), as.character(policy$count_rounding),
          "true", "false", "false", "not_approved"
        ),
        stringsAsFactors = FALSE
      )
    )
    class(out) <- c("react_profile_v2", "list")
    return(out)
  }
  required <- c(
    "metadata", "missingness", "categorical", "numeric", "histograms",
    "text_lengths", "pair_correlations", "pair_crosstabs", "pair_grouped"
  )
  if (!is.list(profile) || !all(required %in% names(profile))) {
    stop("`profile` must be returned by `react_profile()`.", call. = FALSE)
  }
  out <- profile
  identity_groups <- c("feature_id", "round_id")
  out$missingness <- .sdc_count_table(out$missingness, identity_groups, policy)
  out$categorical <- .sdc_count_table(out$categorical, identity_groups, policy)
  out$histograms <- .sdc_count_table(out$histograms, identity_groups, policy)
  out$text_lengths <- .sdc_count_table(out$text_lengths, identity_groups, policy)
  out$numeric <- .sdc_summary_table(out$numeric, policy)
  out$pair_correlations <- .sdc_summary_table(out$pair_correlations, policy)
  out$pair_crosstabs <- .sdc_count_table(
    out$pair_crosstabs,
    c("feature_x", "feature_y", "round_id"),
    policy
  )
  out$pair_grouped <- .sdc_summary_table(out$pair_grouped, policy)
  if (nrow(out$histograms) > 0L) {
    out$histograms$bin_lower <- .round_significant(
      out$histograms$bin_lower, policy$statistic_digits
    )
    out$histograms$bin_upper <- .round_significant(
      out$histograms$bin_upper, policy$statistic_digits
    )
  }
  out$metadata <- rbind(
    out$metadata[out$metadata$key != "status", , drop = FALSE],
    data.frame(
      key = c(
        "status", "minimum_cell_count", "count_rounding",
        "complementary_suppression", "raw_text_included", "exact_extrema_included"
      ),
      value = c(
        "requires_enclave_disclosure_review",
        as.character(policy$min_count),
        as.character(policy$count_rounding),
        "true", "false", "false"
      ),
      stringsAsFactors = FALSE
    )
  )
  out
}
