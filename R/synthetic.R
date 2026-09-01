.stable_stream_seed <- function(seed, round_id, occurrence_id) {
  text <- paste(seed, round_id, occurrence_id, sep = "\r")
  bytes <- utf8ToInt(enc2utf8(text))
  value <- 0
  for (byte in bytes) value <- (value * 131 + byte) %% 2147483646
  as.integer(value + 1)
}

.with_stream_seed <- function(seed, round_id, occurrence_id, code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(.stable_stream_seed(seed, round_id, occurrence_id))
  force(code)
}

.synthetic_profile_metadata <- function(profile) {
  if (!is.list(profile) || !is.data.frame(profile$metadata) ||
      !all(c("key", "value") %in% names(profile$metadata))) {
    stop("`profile` is not a reactextract synthetic profile.", call. = FALSE)
  }
  stats::setNames(profile$metadata$value, profile$metadata$key)
}

.profile_object_sha256 <- function(profile) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(profile, path, version = 3, compress = FALSE)
  .sha256_file(path)
}

.approved_profile_manifest_sha256 <- "ac157927e065fe70ab951f4d8b3accece5f66c4a47b1a1d615354a4bc3c99a5a"
.approved_profile_archive_sha256 <- "3bbf94e346682e0afb244dc5aebb71a04b95b925e720d55b5d13c2b448caf51a"

.synthetic_profile_dictionary_compatibility <- function(metadata) {
  profile_hash <- unname(metadata[["dictionary_manifest_sha256"]])
  current_hash <- react_dictionary_version()$manifest_sha256[[1L]]
  if (identical(profile_hash, current_hash)) return("exact")
  "incompatible"
}

.approved_synthetic_profile <- function(refresh = FALSE) {
  if (!isTRUE(refresh) && !is.null(.reactextract_env$synthetic_profile)) {
    return(.reactextract_env$synthetic_profile)
  }
  archive <- system.file("extdata", "synthetic-profile.tar.gz", package = "reactextract")
  approval_path <- system.file(
    "extdata", "synthetic-profile-approval.csv", package = "reactextract"
  )
  if (!nzchar(archive) || !nzchar(approval_path)) {
    stop("The installed approved synthetic profile is incomplete.", call. = FALSE)
  }
  archive_hash <- .sha256_file(archive)
  if (!identical(archive_hash, .approved_profile_archive_sha256)) {
    stop(
      "Synthetic profile archive SHA-256 mismatch. Expected ",
      .approved_profile_archive_sha256, "; observed ", archive_hash, ".",
      call. = FALSE
    )
  }
  approval <- .read_literal_csv(approval_path)
  required <- c(
    "profile_release", "profile_manifest_sha256", "profile_archive_sha256",
    "approval_status", "approval_date", "approval_confirmed_by"
  )
  if (nrow(approval) != 1L || !all(required %in% names(approval)) ||
      approval$approval_status[[1L]] != "formally_approved" ||
      approval$profile_manifest_sha256[[1L]] != .approved_profile_manifest_sha256 ||
      approval$profile_archive_sha256[[1L]] != archive_hash) {
    stop("The bundled synthetic profile approval record is invalid.", call. = FALSE)
  }
  path <- file.path(tempdir(), paste0("reactextract-profile-", archive_hash))
  if (!file.exists(file.path(path, "manifest.csv"))) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    utils::untar(archive, exdir = path)
  }
  manifest_hash <- .sha256_file(file.path(path, "manifest.csv"))
  if (!identical(manifest_hash, .approved_profile_manifest_sha256)) {
    stop("The bundled synthetic profile manifest does not match its approval.", call. = FALSE)
  }
  profile <- react_read_profile(path)
  metadata <- .synthetic_profile_metadata(profile)
  dictionary_compatibility <- .synthetic_profile_dictionary_compatibility(metadata)
  if (identical(dictionary_compatibility, "incompatible")) {
    stop("Synthetic profile and dictionary hashes do not match.", call. = FALSE)
  }
  replace_keys <- c(
    "status", "disclosure_approval", "approval_date",
    "approval_confirmed_by", "approved_profile_manifest_sha256",
    "profile_release", "dictionary_compatibility"
  )
  profile$metadata <- profile$metadata[
    !(profile$metadata$key %in% replace_keys), , drop = FALSE
  ]
  profile$metadata <- rbind(
    profile$metadata,
    data.frame(
      key = replace_keys,
      value = c(
        "approved_for_release", "formally_approved",
        approval$approval_date[[1L]], approval$approval_confirmed_by[[1L]],
        approval$profile_manifest_sha256[[1L]], approval$profile_release[[1L]],
        dictionary_compatibility
      ),
      stringsAsFactors = FALSE
    )
  )
  class(profile) <- c("react_synthetic_profile", "react_profile_v2", "list")
  .reactextract_env$synthetic_profile <- profile
  profile
}

.development_synthetic_profile <- function() {
  dictionary <- react_dictionary()
  version <- react_dictionary_version()
  occurrence_ids <- dictionary$occurrences$occurrence_id
  approved_specs <- .approved_profile_specs(dictionary)
  approved_specs <- approved_specs[
    approved_specs$profile_kind %in% c("categorical", "ordered_categorical"),
    , drop = FALSE
  ]
  categorical_rows <- lapply(seq_len(nrow(approved_specs)), function(index) {
    occurrence <- dictionary$occurrences[
      dictionary$occurrences$occurrence_id ==
        approved_specs$occurrence_id[[index]],
      , drop = FALSE
    ]
    options <- .profile_response_options(
      occurrence, approved_specs[index, , drop = FALSE], dictionary
    )
    if (!nrow(options)) return(NULL)
    data.frame(
      occurrence_id = occurrence$occurrence_id[[1L]],
      round_id = occurrence$round_id[[1L]],
      value = options$return_value,
      display_value = options$display_value,
      stringsAsFactors = FALSE
    )
  })
  categorical_rows <- categorical_rows[
    !vapply(categorical_rows, is.null, logical(1))
  ]
  categorical <- .bind_rows(categorical_rows, data.frame())
  categorical$count <- 100
  categorical$suppressed <- FALSE

  dependency_specs <- .dependency_specs(dictionary, dictionary$rounds$round_id)
  outcome_rows <- lapply(unique(paste(dependency_specs$round_id,
                                      dependency_specs$outcome_id, sep = "\r")), function(key) {
    parts <- strsplit(key, "\r", fixed = TRUE)[[1L]]
    levels <- .dependency_outcome_levels(parts[[2L]])
    data.frame(
      outcome_id = parts[[2L]], round_id = parts[[1L]], outcome_level = levels,
      count = if (parts[[2L]] == "react1_pcr_positive") {
        c(850, 100, 50)
      } else c(650, 250, 50, 50),
      suppressed = FALSE, stringsAsFactors = FALSE
    )
  })
  outcome_counts <- .bind_rows(outcome_rows, data.frame())
  dependency_rows <- lapply(seq_len(nrow(dependency_specs)), function(index) {
    spec <- dependency_specs[index, , drop = FALSE]
    outcomes <- .dependency_outcome_levels(spec$outcome_id[[1L]])
    predictors <- .dependency_split_ids(spec$levels[[1L]])
    grid <- expand.grid(
      outcome_level = outcomes, predictor_level = predictors,
      stringsAsFactors = FALSE
    )
    grid$count <- 100
    if (length(predictors) > 1L) {
      grid$count[grid$outcome_level == "positive" &
                   grid$predictor_level == predictors[[2L]]] <- 300
    }
    data.frame(
      dependency_id = spec$dependency_id[[1L]], round_id = spec$round_id[[1L]],
      outcome_id = spec$outcome_id[[1L]], predictor_id = spec$predictor_id[[1L]],
      grid, suppressed = FALSE, stringsAsFactors = FALSE
    )
  })

  profile <- list(
    metadata = data.frame(
      key = c(
        "profile_schema_version", "profile_version", "status",
        "dictionary_release", "dictionary_manifest_sha256",
        "disclosure_approval", "safe_prior_fraction"
      ),
      value = c(
        "2", "public-domain-development-v1", "development_only",
        version$dictionary_release, version$manifest_sha256,
        "not_applicable_no_enclave_aggregates", "0.01"
      ),
      stringsAsFactors = FALSE
    ),
    round_denominators = data.frame(
      round_id = dictionary$rounds$round_id,
      count = 1000,
      suppressed = FALSE,
      stringsAsFactors = FALSE
    ),
    missingness = rbind(
      data.frame(
        occurrence_id = dictionary$occurrences$occurrence_id,
        round_id = dictionary$occurrences$round_id,
        status = "database_missing",
        count = 5,
        suppressed = FALSE,
        stringsAsFactors = FALSE
      ),
      data.frame(
        occurrence_id = dictionary$synthetic_profile_overrides$occurrence_id,
        round_id = dictionary$occurrences$round_id[match(
          dictionary$synthetic_profile_overrides$occurrence_id,
          dictionary$occurrences$occurrence_id
        )],
        status = "coded: ",
        count = 5,
        suppressed = FALSE,
        stringsAsFactors = FALSE
      )
    ),
    categorical_counts = categorical,
    numeric_bin_counts = data.frame(
      occurrence_id = character(), round_id = character(), bin_spec_id = character(),
      bin_id = character(), count = numeric(), suppressed = logical(),
      stringsAsFactors = FALSE
    ),
    text_presence = data.frame(
      occurrence_id = character(), round_id = character(), status = character(),
      count = numeric(), suppressed = logical(), stringsAsFactors = FALSE
    ),
    routing_validation = data.frame(
      routing_rule_id = character(), round_id = character(), status = character(),
      count = numeric(), suppressed = logical(), stringsAsFactors = FALSE
    ),
    outcome_counts = outcome_counts,
    dependency_counts = .bind_rows(dependency_rows, data.frame()),
    dependency_specs = dictionary$synthetic_dependencies,
    profile_specs = .approved_profile_specs(dictionary),
    safe_bins = dictionary$safe_bins,
    issues = .empty_issues()
  )
  profile$metadata <- rbind(
    profile$metadata,
    data.frame(
      key = "profile_sha256",
      value = .profile_object_sha256(profile),
      stringsAsFactors = FALSE
    )
  )
  class(profile) <- c("react_synthetic_profile", "react_profile_v2", "list")
  profile
}

#' Load the bundled synthetic profile
#'
#' The default is the immutable, disclosure-approved aggregate profile bundled
#' with the package. Set `development = TRUE` only to use the public-domain
#' fallback that contains no enclave-derived distributions. No profile is
#' downloaded at runtime.
#'
#' @param development Use the public-domain development fallback instead of the
#'   approved aggregate profile.
#' @param refresh Re-read and re-verify the approved bundled profile.
#' @return A schema-2 synthetic profile.
#' @export
react_synthetic_profile <- function(development = FALSE, refresh = FALSE) {
  if (!is.logical(development) || length(development) != 1L || is.na(development)) {
    stop("`development` must be one TRUE or FALSE value.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    stop("`refresh` must be one TRUE or FALSE value.", call. = FALSE)
  }
  if (!development) return(.approved_synthetic_profile(refresh = refresh))
  .development_synthetic_profile()
}

.normalise_synthetic_counts <- function(n_per_round, rounds) {
  if (length(n_per_round) == 1L && is.null(names(n_per_round))) {
    value <- as.integer(n_per_round)
    if (is.na(value) || value < 1L || value != n_per_round) {
      stop("`n_per_round` must contain positive whole numbers.", call. = FALSE)
    }
    return(stats::setNames(rep(value, nrow(rounds)), rounds$round_id))
  }
  if (is.null(names(n_per_round)) || any(!nzchar(names(n_per_round)))) {
    stop("A multi-value `n_per_round` must be named with round or survey IDs.", call. = FALSE)
  }
  resolved <- vapply(names(n_per_round), .resolve_round_name, character(1L), rounds = rounds)
  if (anyNA(resolved) || anyDuplicated(resolved)) {
    stop("`n_per_round` contains an unknown or duplicate round.", call. = FALSE)
  }
  values <- as.integer(n_per_round)
  if (anyNA(values) || any(values < 1L) || any(values != n_per_round)) {
    stop("`n_per_round` must contain positive whole numbers.", call. = FALSE)
  }
  stats::setNames(values, resolved)
}

#' Create a safe synthetic REACT source
#'
#' @param profile A schema-2 profile from [react_synthetic_profile()] or an
#'   approved profile read by [react_read_profile()].
#' @param n_per_round One positive integer or a named vector by round.
#' @param seed One deterministic whole-number seed.
#' @return A source object for [react_extract()].
#' @export
react_synthetic <- function(profile = react_synthetic_profile(),
                            n_per_round = 1000L, seed = 1L) {
  metadata <- .synthetic_profile_metadata(profile)
  if (!identical(unname(metadata[["profile_schema_version"]]), "2")) {
    stop("Synthetic generation requires profile schema version 2.", call. = FALSE)
  }
  allowed_status <- c("development_only", "approved_for_release")
  if (!(unname(metadata[["status"]]) %in% allowed_status)) {
    stop(
      "Synthetic generation accepts only the development profile or a profile marked `approved_for_release`.",
      call. = FALSE
    )
  }
  dictionary_version <- react_dictionary_version()
  if (identical(
    .synthetic_profile_dictionary_compatibility(metadata),
    "incompatible"
  )) {
    stop("Synthetic profile and dictionary hashes do not match.", call. = FALSE)
  }
  seed <- as.integer(seed)
  if (length(seed) != 1L || is.na(seed)) {
    stop("`seed` must be one whole number.", call. = FALSE)
  }
  rounds <- react_dictionary()$rounds
  counts <- .normalise_synthetic_counts(n_per_round, rounds)
  structure(
    list(
      kind = "synthetic",
      profile = profile,
      profile_metadata = metadata,
      profile_sha256 = .profile_object_sha256(profile),
      n_per_round = counts,
      seed = seed,
      safe_prior_fraction = as.numeric(metadata[["safe_prior_fraction"]]),
      dependency_fallbacks = .synthetic_dependency_fallbacks(profile)
    ),
    class = c("react_synthetic_source", "react_source")
  )
}

.smoothed_probabilities <- function(counts, prior_fraction) {
  counts <- as.numeric(counts)
  released <- !is.na(counts) & counts > 0
  k <- length(counts)
  if (k == 0L) return(numeric())
  if (any(released)) {
    empirical <- numeric(k)
    empirical[released] <- counts[released] / sum(counts[released])
    return((1 - prior_fraction) * empirical + prior_fraction / k)
  }
  rep(1 / k, k)
}

.synthetic_support <- function(source, occurrence) {
  profile <- source$profile
  rows <- profile$categorical_counts[
    profile$categorical_counts$occurrence_id == occurrence$occurrence_id[[1L]] &
      profile$categorical_counts$round_id == occurrence$round_id[[1L]],
    ,
    drop = FALSE
  ]
  coded_status <- source$profile$missingness$status[
    source$profile$missingness$occurrence_id == occurrence$occurrence_id[[1L]] &
      source$profile$missingness$round_id == occurrence$round_id[[1L]] &
      startsWith(source$profile$missingness$status, "coded:")
  ]
  coded_values <- sub("^coded:", "", coded_status)
  rows <- rows[!(rows$value %in% coded_values), , drop = FALSE]
  if (nrow(rows) == 0L) return(NULL)
  probability <- .smoothed_probabilities(rows$count, source$safe_prior_fraction)
  list(values = rows$value, probability = probability)
}

.synthetic_missing_states <- function(source, occurrence, n) {
  rows <- source$profile$missingness[
    source$profile$missingness$occurrence_id == occurrence$occurrence_id[[1L]] &
      source$profile$missingness$round_id == occurrence$round_id[[1L]] &
      (source$profile$missingness$status == "database_missing" |
         source$profile$missingness$status == "outside_safe_support" |
         startsWith(source$profile$missingness$status, "coded:")),
    , drop = FALSE
  ]
  if (nrow(rows) == 0L) return(rep("observed", n))
  counts <- suppressWarnings(as.numeric(rows$count))
  keep <- !is.na(counts) & counts > 0
  rows <- rows[keep, , drop = FALSE]
  counts <- counts[keep]
  if (nrow(rows) == 0L) return(rep("observed", n))
  combined <- stats::aggregate(
    counts, by = list(status = rows$status), FUN = sum
  )
  denominator_values <- source$profile$round_denominators$count[
    source$profile$round_denominators$round_id == occurrence$round_id[[1L]]
  ]
  if (length(denominator_values) == 0L) return(rep("observed", n))
  denominator <- suppressWarnings(as.numeric(denominator_values[[1L]]))
  if (is.na(denominator) || denominator <= 0) {
    return(rep("observed", n))
  }
  probability <- pmax(0, combined$x / denominator)
  if (sum(probability) >= 1) probability <- probability / sum(probability) * 0.999
  states <- c(combined$status, "observed")
  probability <- c(probability, 1 - sum(probability))
  .with_stream_seed(
    source$seed, occurrence$round_id[[1L]],
    paste0(occurrence$occurrence_id[[1L]], "::missing_state"),
    sample(states, n, replace = TRUE, prob = probability)
  )
}

.apply_synthetic_missing_states <- function(value, states, occurrence) {
  database_missing <- states == "database_missing"
  unsupported <- states == "outside_safe_support"
  value[database_missing | unsupported] <- NA
  coded <- startsWith(states, "coded:")
  if (any(coded)) {
    codes <- sub("^coded:", "", states[coded])
    data_type <- occurrence$data_type[[1L]]
    if (data_type == "NUMBER") {
      value[coded] <- suppressWarnings(as.numeric(codes))
    } else if (data_type == "DATE" || startsWith(data_type, "TIMESTAMP")) {
      value[coded] <- NA
    } else {
      value[coded] <- codes
    }
  }
  value
}

.synthetic_binned_value <- function(source, occurrence, spec, n) {
  if (nrow(spec) == 0L || !nzchar(spec$bin_spec_id[[1L]])) return(NULL)
  bins <- source$profile$safe_bins[
    source$profile$safe_bins$bin_spec_id == spec$bin_spec_id[[1L]],
    , drop = FALSE
  ]
  if (nrow(bins) == 0L) return(NULL)
  rows <- source$profile$numeric_bin_counts[
    source$profile$numeric_bin_counts$occurrence_id == occurrence$occurrence_id[[1L]] &
      source$profile$numeric_bin_counts$round_id == occurrence$round_id[[1L]],
    , drop = FALSE
  ]
  counts <- if (nrow(rows)) {
    as.numeric(rows$count[match(bins$bin_id, rows$bin_id)])
  } else {
    rep(NA_real_, nrow(bins))
  }
  probability <- .smoothed_probabilities(counts, source$safe_prior_fraction)
  selected <- sample(seq_len(nrow(bins)), n, replace = TRUE, prob = probability)
  value <- numeric(n)
  is_date <- spec$profile_kind[[1L]] == "date"
  for (bin_index in seq_len(nrow(bins))) {
    positions <- which(selected == bin_index)
    if (length(positions) == 0L) next
    lower <- bins$lower[[bin_index]]
    upper <- bins$upper[[bin_index]]
    if (is_date) {
      lower_value <- if (nzchar(lower)) as.numeric(as.Date(lower)) else as.numeric(as.Date("1900-01-01"))
      upper_value <- if (nzchar(upper)) as.numeric(as.Date(upper)) else lower_value + 365
    } else {
      lower_value <- if (nzchar(lower)) as.numeric(lower) else as.numeric(upper) - 10
      upper_value <- if (nzchar(upper)) as.numeric(upper) else lower_value + 100
    }
    boundary_rule <- .safe_bin_boundary_rule(bins, bin_index)
    lower_exclusive <- boundary_rule %in% c(
      "lower_exclusive_upper_inclusive", "exclusive"
    )
    upper_exclusive <- boundary_rule %in% c(
      "lower_inclusive_upper_exclusive", "exclusive"
    )
    if (spec$profile_kind[[1L]] == "continuous") {
      generated <- stats::runif(length(positions), lower_value, upper_value)
      if (lower_exclusive && lower_value == upper_value) {
        stop("An exclusive continuous bin cannot have identical bounds.", call. = FALSE)
      }
      if (lower_exclusive) {
        generated[generated <= lower_value] <-
          lower_value + (upper_value - lower_value) / 2
      }
      if (!upper_exclusive && lower_value == upper_value) {
        generated[] <- lower_value
      }
      value[positions] <- generated
    } else {
      integer_lower <- if (lower_exclusive) {
        floor(lower_value) + 1L
      } else {
        ceiling(lower_value)
      }
      integer_upper <- if (upper_exclusive) {
        ceiling(upper_value) - 1L
      } else {
        floor(upper_value)
      }
      if (integer_lower > integer_upper) {
        stop("A safe integer/date bin contains no permitted value.", call. = FALSE)
      }
      value[positions] <- sample(
        seq.int(integer_lower, integer_upper),
        length(positions), replace = TRUE
      )
    }
  }
  if (is_date) as.Date(value, origin = "1970-01-01") else value
}

.synthetic_text_value <- function(source, occurrence, n) {
  rows <- source$profile$text_presence[
    source$profile$text_presence$occurrence_id == occurrence$occurrence_id[[1L]] &
      source$profile$text_presence$round_id == occurrence$round_id[[1L]],
    , drop = FALSE
  ]
  if (nrow(rows) == 0L) return(rep("Synthetic response", n))
  probability <- .smoothed_probabilities(rows$count, source$safe_prior_fraction)
  status <- sample(rows$status, n, replace = TRUE, prob = probability)
  ifelse(status == "present", "Synthetic response", NA_character_)
}

.coerce_synthetic_value <- function(value, data_type) {
  if (data_type == "NUMBER") {
    return(suppressWarnings(as.numeric(value)))
  }
  if (data_type == "DATE") return(as.Date(value))
  if (startsWith(data_type, "TIMESTAMP")) return(as.POSIXct(value, tz = "UTC"))
  as.character(value)
}

.synthetic_identifier_value <- function(source, occurrence, n) {
  variable <- occurrence$variable[[1L]]
  round_id <- occurrence$round_id[[1L]]
  if (occurrence$data_type[[1L]] == "NUMBER") {
    offset <- .stable_stream_seed(source$seed, round_id, occurrence$occurrence_id[[1L]]) %% 800000000
    return(1000000000 + offset + seq_len(n))
  }
  round_tag <- toupper(gsub("[^A-Za-z0-9]+", "-", round_id))
  variable_tag <- toupper(gsub("[^A-Za-z0-9]+", "-", variable))
  sprintf("SYN-%s-%s-%07d", round_tag, variable_tag, seq_len(n))
}

.coerce_like <- function(value, template) {
  if (inherits(template, "Date")) return(as.Date(value))
  if (is.integer(template)) return(as.integer(value))
  if (is.numeric(template)) return(as.numeric(value))
  if (is.logical(template)) return(as.logical(value))
  as.character(value)
}

.generate_synthetic_value <- function(source, occurrence, n) {
  spec <- source$profile$profile_specs
  spec_row <- if (is.data.frame(spec)) {
    spec[spec$occurrence_id == occurrence$occurrence_id[[1L]], , drop = FALSE]
  } else {
    data.frame()
  }
  profile_kind <- if (nrow(spec_row)) spec_row$profile_kind[[1L]] else ""
  action <- if (nrow(spec_row)) spec_row$generation_action[[1L]] else ""
  support <- if (profile_kind %in% c("categorical", "ordered_categorical")) {
    .synthetic_support(source, occurrence)
  } else {
    NULL
  }
  issue <- .empty_issues()

  value <- .with_stream_seed(source$seed, occurrence$round_id[[1L]],
                             occurrence$occurrence_id[[1L]], {
    if (action == "excluded") {
      rep(NA_character_, n)
    } else if (action == "synthetic_identifier" || profile_kind == "identifier") {
      .synthetic_identifier_value(source, occurrence, n)
    } else if (action == "fixed_placeholder" || profile_kind == "free_text") {
      .synthetic_text_value(source, occurrence, n)
    } else if (!is.null(support)) {
      sample(support$values, n, replace = TRUE, prob = support$probability)
    } else if (profile_kind == "date" || occurrence$data_type[[1L]] == "DATE") {
      binned <- .synthetic_binned_value(source, occurrence, spec_row, n)
      if (is.null(binned)) {
        as.Date("2021-01-01") + sample.int(365L, n, replace = TRUE) - 1L
      } else binned
    } else if (profile_kind %in% c("integer", "continuous") ||
               occurrence$data_type[[1L]] == "NUMBER") {
      binned <- .synthetic_binned_value(source, occurrence, spec_row, n)
      if (is.null(binned)) sample.int(11L, n, replace = TRUE) - 1L else binned
    } else {
      rep(NA_character_, n)
    }
  })

  if (is.null(support) && !(action %in% c(
      "excluded", "fixed_placeholder", "synthetic_identifier"
    )) &&
      !(profile_kind %in% c("date", "integer", "continuous"))) {
    issue <- .issue(
      "warning", "synthetic", "synthetic_profile_support_unavailable",
      "No safe public support was available; missing values were generated.",
      occurrence$round_id[[1L]], "synthetic", occurrence$variable[[1L]], n
    )
  }
  value <- .coerce_synthetic_value(value, occurrence$data_type[[1L]])

  if (!(action %in% c("excluded", "synthetic_identifier")) &&
      profile_kind != "free_text") {
    missing_states <- .synthetic_missing_states(source, occurrence, n)
    value <- .apply_synthetic_missing_states(value, missing_states, occurrence)
  }
  list(value = value, issues = issue)
}

.parse_comparison_values <- function(comparison_values_json) {
  text <- gsub("[[:space:]]", "", comparison_values_json)
  if (!grepl("^\\[(-?[0-9]+([.][0-9]+)?(,-?[0-9]+([.][0-9]+)?)*)?\\]$", text)) {
    stop("Approved routing comparison values are not a numeric JSON array.", call. = FALSE)
  }
  content <- substring(text, 2L, nchar(text) - 1L)
  if (!nzchar(content)) return(numeric())
  as.numeric(strsplit(content, ",", fixed = TRUE)[[1L]])
}

.condition_true <- function(value, operator, comparison_values_json) {
  comparison <- .parse_comparison_values(comparison_values_json)
  if (operator == "is_missing") return(is.na(value))
  if (operator == "not_missing") return(!is.na(value))
  if (operator == "selected_any") {
    selected <- tolower(trimws(as.character(value))) %in%
      c("1", "true", "yes", "selected")
    return(!is.na(value) & selected)
  }
  numeric_value <- suppressWarnings(as.numeric(value))
  numeric_comparison <- suppressWarnings(as.numeric(comparison))
  if (operator == "equals") {
    if (length(comparison) == 0L) return(rep(FALSE, length(value)))
    return(!is.na(value) & value == comparison[[1L]])
  }
  if (operator == "in") return(!is.na(value) & value %in% comparison)
  if (operator == "not_in") return(!is.na(value) & !(value %in% comparison))
  if (operator == "gt") return(!is.na(numeric_value) & numeric_value > numeric_comparison[[1L]])
  if (operator == "gte") return(!is.na(numeric_value) & numeric_value >= numeric_comparison[[1L]])
  if (operator == "lt") return(!is.na(numeric_value) & numeric_value < numeric_comparison[[1L]])
  if (operator == "lte") return(!is.na(numeric_value) & numeric_value <= numeric_comparison[[1L]])
  stop("Unsupported approved routing operator: ", operator, ".", call. = FALSE)
}

.gate_rule_eligibility <- function(data, rule_id, conditions, occurrence_variable) {
  condition_rows <- conditions[conditions$routing_rule_id == rule_id, , drop = FALSE]
  eligible <- rep(FALSE, nrow(data))
  for (clause_id in unique(condition_rows$clause_id)) {
    clause <- condition_rows[condition_rows$clause_id == clause_id, , drop = FALSE]
    clause_result <- rep(TRUE, nrow(data))
    for (index in seq_len(nrow(clause))) {
      parent_variable <- occurrence_variable[[clause$parent_occurrence_id[[index]]]]
      clause_result <- clause_result & .condition_true(
        data[[parent_variable]], clause$operator[[index]],
        clause$comparison_values_json[[index]]
      )
    }
    eligible <- eligible | clause_result
  }
  eligible
}

.apply_synthetic_gate_rules <- function(data, rules, conditions, targets,
                                        occurrence_variable) {
  gate_ids <- rules$routing_rule_id[rules$rule_type %in% c("gate", "terminate", "other_text")]
  if (length(gate_ids) == 0L) return(data)
  gate_conditions <- conditions[conditions$routing_rule_id %in% gate_ids, , drop = FALSE]
  gate_targets <- targets[targets$routing_rule_id %in% gate_ids, , drop = FALSE]
  present_ids <- names(occurrence_variable)[occurrence_variable %in% names(data)]
  pending_targets <- unique(gate_targets$target_occurrence_id[
    gate_targets$target_occurrence_id %in% present_ids
  ])

  while (length(pending_targets)) {
    progressed <- FALSE
    for (target_id in pending_targets) {
      governing_rules <- unique(gate_targets$routing_rule_id[
        gate_targets$target_occurrence_id == target_id
      ])
      parent_ids <- gate_conditions$parent_occurrence_id[
        gate_conditions$routing_rule_id %in% governing_rules
      ]
      if (any(parent_ids %in% pending_targets)) next
      parent_variables <- unname(occurrence_variable[parent_ids])
      if (anyNA(parent_variables) || !all(parent_variables %in% names(data))) next

      eligible <- rep(FALSE, nrow(data))
      for (rule_id in governing_rules) {
        eligible <- eligible | .gate_rule_eligibility(
          data, rule_id, gate_conditions, occurrence_variable
        )
      }
      target_variable <- occurrence_variable[[target_id]]
      target_rules <- rules[rules$routing_rule_id %in% governing_rules, , drop = FALSE]
      exact_codes <- unique(target_rules$false_value[
        target_rules$false_value_kind == "exact_code" & nzchar(target_rules$false_value)
      ])
      if (length(exact_codes) > 1L) {
        stop("Approved routes specify conflicting skip codes for `", target_variable, "`.", call. = FALSE)
      }
      if (length(exact_codes) == 1L) {
        data[[target_variable]][!eligible] <- .coerce_like(exact_codes, data[[target_variable]])
      } else {
        data[[target_variable]][!eligible] <- NA
      }
      pending_targets <- setdiff(pending_targets, target_id)
      progressed <- TRUE
    }
    if (!progressed) {
      stop("Approved routing rules could not be evaluated in acyclic order.", call. = FALSE)
    }
  }
  data
}

.apply_synthetic_routing <- function(data, occurrences, dictionary) {
  rules <- dictionary$routing_rules
  conditions <- dictionary$routing_conditions
  targets <- dictionary$routing_targets
  if (is.null(rules) || nrow(rules) == 0L) return(data)
  present_ids <- occurrences$occurrence_id
  relevant_rule_ids <- unique(targets$routing_rule_id[
    targets$target_occurrence_id %in% present_ids
  ])
  rules <- rules[
    rules$review_state == "approved" & rules$routing_rule_id %in% relevant_rule_ids,
    , drop = FALSE
  ]
  if (nrow(rules) == 0L) return(data)
  conditions <- conditions[conditions$routing_rule_id %in% rules$routing_rule_id, , drop = FALSE]
  targets <- targets[targets$routing_rule_id %in% rules$routing_rule_id, , drop = FALSE]
  occurrence_variable <- stats::setNames(occurrences$variable, occurrences$occurrence_id)
  data <- .apply_synthetic_gate_rules(data, rules, conditions, targets, occurrence_variable)

  constraint_rules <- rules[!rules$rule_type %in% c("gate", "terminate", "other_text"), , drop = FALSE]
  pending <- constraint_rules$routing_rule_id
  while (length(pending)) {
    progressed <- FALSE
    for (rule_id in pending) {
      condition_rows <- conditions[conditions$routing_rule_id == rule_id, , drop = FALSE]
      target_rows <- targets[targets$routing_rule_id == rule_id, , drop = FALSE]
      upstream_pending_targets <- targets$target_occurrence_id[
        targets$routing_rule_id %in% setdiff(pending, rule_id)
      ]
      if (any(condition_rows$parent_occurrence_id %in% upstream_pending_targets)) next
      parent_variables <- unname(occurrence_variable[condition_rows$parent_occurrence_id])
      target_variables <- unname(occurrence_variable[target_rows$target_occurrence_id])
      if (anyNA(c(parent_variables, target_variables)) ||
          !all(parent_variables %in% names(data))) next
      eligible <- .gate_rule_eligibility(data, rule_id, conditions, occurrence_variable)
      rule <- rules[rules$routing_rule_id == rule_id, , drop = FALSE]
      available_targets <- target_variables[target_variables %in% names(data)]
      rule_type <- rule$rule_type[[1L]]
      if (rule_type == "repeat_count") {
        parent_value <- suppressWarnings(as.integer(data[[parent_variables[[1L]]]]))
        target_order <- suppressWarnings(as.integer(target_rows$target_order))
        for (target_index in seq_along(available_targets)) {
          clear <- is.na(parent_value) | parent_value < target_order[[target_index]]
          data[[available_targets[[target_index]]]][clear] <- NA
        }
      } else if (rule_type == "exclusive_option") {
        selected <- lapply(available_targets, function(variable) {
          value <- data[[variable]]
          !is.na(value) & !(as.character(value) %in% c("0", "FALSE", "No", ""))
        })
        selected <- do.call(cbind, selected)
        if (!is.null(selected) && ncol(selected) > 1L) {
          conflicted <- rowSums(selected) > 1L
          if (any(conflicted)) {
            keep <- max.col(selected[conflicted, , drop = FALSE], ties.method = "first")
            conflict_rows <- which(conflicted)
            for (column_index in seq_along(available_targets)) {
              clear_rows <- conflict_rows[keep != column_index]
              template <- data[[available_targets[[column_index]]]]
              data[[available_targets[[column_index]]]][clear_rows] <-
                if (is.logical(template)) FALSE else if (is.numeric(template)) 0 else NA
            }
          }
        }
      } else if (rule_type == "ordered_values") {
        if (length(available_targets) > 1L) {
          for (row_index in seq_len(nrow(data))) {
            values <- lapply(available_targets, function(variable) data[[variable]][row_index])
            present <- !vapply(values, function(value) length(value) == 0L || is.na(value), logical(1L))
            sorted <- sort(do.call(c, values[present]))
            for (value_index in seq_along(sorted)) {
              data[[available_targets[present][[value_index]]]][row_index] <- sorted[[value_index]]
            }
          }
        }
      } else {
        for (target_variable in available_targets) {
          if (rule$false_value_kind[[1L]] == "exact_code" && nzchar(rule$false_value[[1L]])) {
            data[[target_variable]][!eligible] <- .coerce_like(
              rule$false_value[[1L]], data[[target_variable]]
            )
          } else {
            data[[target_variable]][!eligible] <- NA
          }
        }
      }
      pending <- setdiff(pending, rule_id)
      progressed <- TRUE
    }
    if (!progressed) {
      stop("Approved routing rules could not be evaluated in acyclic order.", call. = FALSE)
    }
  }
  data
}

.read_synthetic_round <- function(source, registry_row, occurrences, dictionary) {
  round_id <- registry_row$round_id[[1L]]
  n <- source$n_per_round[[round_id]]
  if (is.null(n)) {
    return(list(
      data = NULL, source_object = "synthetic",
      issues = .issue("warning", "synthetic", "round_unavailable",
                      "No synthetic sample size was supplied for this round.", round_id)
    ))
  }
  key <- registry_row$observation_key[[1L]]
  round_tag <- toupper(gsub("[.]", "-", round_id))
  data <- data.frame(
    placeholder = seq_len(n),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(data) <- key
  data[[key]] <- sprintf("SYN-%s-%07d", round_tag, seq_len(n))
  data$SUBJECT_ID <- sprintf("SYN-SUBJECT-%s-%07d", round_tag, seq_len(n))
  issues <- list()
  for (index in seq_len(nrow(occurrences))) {
    occurrence <- occurrences[index, , drop = FALSE]
    variable <- occurrence$variable[[1L]]
    if (variable %in% c(key, "SUBJECT_ID")) next
    generated <- .generate_synthetic_value(source, occurrence, n)
    data[[variable]] <- generated$value
    issues[[length(issues) + 1L]] <- generated$issues
  }
  data <- .apply_synthetic_dependencies(data, occurrences, source, dictionary)
  data <- .apply_synthetic_routing(data, occurrences, dictionary)
  data <- .reapply_synthetic_outcomes(data, occurrences, source, dictionary)
  data <- .synchronise_synthetic_age_group(data, occurrences, dictionary)
  list(
    data = data,
    source_object = paste0("synthetic:", round_id),
    issues = .bind_rows(issues, .empty_issues())
  )
}

.synthetic_generation_occurrences <- function(dictionary, selected, requested_rounds,
                                              source = NULL) {
  dependency_occurrences <- if (!is.null(source)) {
    .synthetic_dependency_occurrences(dictionary, source, requested_rounds)
  } else {
    dictionary$occurrences[0, , drop = FALSE]
  }
  rules <- dictionary$routing_rules
  conditions <- dictionary$routing_conditions
  targets <- dictionary$routing_targets
  if (is.null(rules) || is.null(conditions) || is.null(targets) || nrow(rules) == 0L) {
    return(unique(rbind(selected, dependency_occurrences)))
  }
  approved <- rules$routing_rule_id[rules$review_state == "approved"]
  conditions <- conditions[conditions$routing_rule_id %in% approved, , drop = FALSE]
  targets <- targets[targets$routing_rule_id %in% approved, , drop = FALSE]
  needed <- unique(c(selected$occurrence_id, dependency_occurrences$occurrence_id))
  repeat {
    governing_rules <- unique(targets$routing_rule_id[targets$target_occurrence_id %in% needed])
    expanded <- unique(c(
      needed,
      conditions$parent_occurrence_id[conditions$routing_rule_id %in% governing_rules]
    ))
    if (setequal(expanded, needed)) break
    needed <- expanded
  }
  dictionary$occurrences[
    dictionary$occurrences$round_id %in% requested_rounds &
      dictionary$occurrences$occurrence_id %in% needed,
    ,
    drop = FALSE
  ]
}
