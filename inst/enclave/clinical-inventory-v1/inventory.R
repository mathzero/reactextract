# Clinical cohort inventory and profile (clinical-inventory-v1). Base R only.
# ci_run() reads one source at a time and saves aggregates (never rows) to an
# INTERNAL folder that stays in the enclave. Those aggregates are not yet
# disclosure-controlled: export.R applies the rules and writes the candidate.

ci_version <- "clinical-inventory-v1"

## settings ---------------------------------------------------------------------

ci_missing_codes <- c(-77, -91, -92, -99, -66, -555, -999)
ci_probs <- c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
ci_anchor_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
ci_max_levels <- 50L           # more distinct values than this: no labels kept
ci_max_anchor_levels <- 15L    # categorical columns cross-tabulated by anchors
ci_categorical_anchors <- c("sex", "age_group", "lc_group", "main_cohort")
ci_max_cor_columns <- 150L
ci_min_cell <- 10L
ci_round_base <- 5L
ci_min_numeric_n <- 30L
ci_min_cor_n <- 50L
ci_min_profile_n <- 50L        # smaller tables: structure only
ci_omics_max_features <- 2000L # features entering the low-rank summary

# Names that mark identifiers, values never kept (direct identifiers or
# quasi-identifiers), and free text. Matched on lower-case names without "#".
ci_id_names <- c("code", "id", "iid", "fid", "uid", "user", "user_code", "barcode", "passcode",
  "u_passcode", "u_password", "password", "project_identifier", "react_id", "lpid", "patientid",
  "sample", "well")
ci_id_suffix <- "[._ ](id|iid|barcode|passcode|password|identifier)$"
ci_id_contains <- "(sample[._ ]?name|biosample|sample[._ ]?well|nhs[._ ]?number|subject[._ ]?id)"
ci_withheld_pattern <- paste0("(dob|birth|postcode|post_code|address|email|phone|mobile|surname|",
  "forename|first_name|last_name|full_name|lsoa|msoa|gp_practice|ip_address|latitude|longitude)")
ci_text_pattern <- "(text|comment|specify|describe|free|other)"

## small helpers -----------------------------------------------------------------

ci_frame <- function(...) data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)

# rbind data frames whose columns differ, filling missing columns with NA
ci_bind <- function(frames) {
  frames <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, frames)
  if (!length(frames)) return(NULL)
  cols <- unique(unlist(lapply(frames, names)))
  frames <- lapply(frames, function(x) {
    for (col in setdiff(cols, names(x))) x[[col]] <- rep(NA, nrow(x))
    x[cols]
  })
  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  out
}

# results accumulate per kind in an environment, then bind once per source
ci_new_result <- function() new.env(parent = emptyenv())
ci_put <- function(res, kind, frame) {
  if (is.data.frame(frame) && nrow(frame)) res[[kind]] <- c(res[[kind]], list(frame))
  invisible(NULL)
}
ci_finish <- function(res) {
  kinds <- sort(ls(res))
  stats::setNames(lapply(kinds, function(k) ci_bind(res[[k]])), kinds)
}

ci_name_key <- function(name) tolower(sub("^#", "", trimws(name)))
ci_is_id_name <- function(name) {
  k <- ci_name_key(name)
  k %in% ci_id_names | grepl(ci_id_suffix, k) | grepl(ci_id_contains, k)
}
ci_subject_col <- function(df) which(tolower(names(df)) == "subject_id")[1]

# Oracle NUMBER can arrive as integer64; it needs bit64 to become usable doubles
ci_as_double <- function(x) {
  if (!inherits(x, "integer64")) return(x)
  if (!requireNamespace("bit64", quietly = TRUE)) stop("integer64 column without bit64 installed")
  as.double(x)
}

# join keys as character; "%.0f" avoids "1e+06" for large numeric IDs
ci_key <- function(x) {
  x <- ci_as_double(x)
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) {
    out <- rep(NA_character_, length(x))
    out[!is.na(x)] <- sprintf("%.0f", x[!is.na(x)])
    return(out)
  }
  out <- trimws(as.character(x))
  out[!is.na(out) & !nzchar(out)] <- NA_character_
  out
}

ci_as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.character(x) || is.factor(x)) {
    x <- substr(as.character(x), 1L, 10L)
    d <- as.Date(x, format = "%Y-%m-%d")
    if (all(is.na(d))) d <- as.Date(x, format = "%d/%m/%Y")
    return(d)
  }
  as.Date(rep(NA_character_, length(x)))
}

ci_qnames <- function(p) sprintf("p%02d", round(p * 100))

ci_decimals <- function(v) {
  v <- utils::head(v, 5000L)
  if (!length(v)) return(NA_integer_)
  for (d in 0:6) {
    s <- v * 10^d
    if (mean(abs(s - round(s)) <= 1e-6 * pmax(1, abs(s))) >= 0.99) return(d)
  }
  7L
}

# skewness is used only to choose an omics transform; it is never exported
ci_skew <- function(v) {
  if (length(v) < 3L) return(NA_real_)
  s <- stats::sd(v)
  if (!is.finite(s) || s == 0) return(NA_real_)
  mean((v - mean(v))^3) / s^3
}

# make error text safe to keep: no long digit runs or quoted values
ci_sanitise <- function(x) {
  x <- gsub("'[^']*'|\"[^\"]*\"", "'...'", x)
  x <- gsub("[0-9]{4,}", "#", x)
  substr(gsub("[\r\n]+", " ", x), 1L, 200L)
}

ci_mask_name <- function(x) gsub("[0-9]{5,}", "#####", x)

# split values such as "<LOD" or "n.d." from numbers in character columns
ci_numeric_tokens <- function(x) {
  x <- ci_as_double(x)
  if (is.numeric(x)) return(list(value = as.numeric(x), tokens = NULL))
  text <- trimws(as.character(x))
  value <- suppressWarnings(as.numeric(text))
  token <- text[!is.na(text) & nzchar(text) & is.na(value)]
  list(value = value, tokens = if (length(token)) table(token) else NULL)
}

## column roles --------------------------------------------------------------------

# empty, withheld, identifier, date, categorical, coded_numeric, continuous,
# free_text, high_cardinality or other. Roles decide what may be kept.
ci_role <- function(x, name) {
  k <- ci_name_key(name)
  ok <- !is.na(x)
  if (!any(ok)) return("empty")
  if (grepl(ci_withheld_pattern, k)) return("withheld")
  if (ci_is_id_name(name)) return("identifier")
  if (inherits(x, c("Date", "POSIXt"))) return("date")
  if (is.list(x) || is.complex(x) || is.raw(x)) return("other")
  if (is.logical(x)) return("categorical")
  if (is.factor(x) || is.character(x)) {
    n_distinct <- length(unique(as.character(x[ok])))
    if (is.character(x) && grepl(ci_text_pattern, k) && n_distinct > 10L) return("free_text")
    return(if (n_distinct <= ci_max_levels) "categorical" else "high_cardinality")
  }
  if (is.numeric(x) || inherits(x, "difftime")) {
    v <- as.numeric(x[ok])
    v <- v[is.finite(v) & !v %in% ci_missing_codes]
    if (!length(v) || (length(unique(v)) <= 12L && all(v == round(v)))) return("coded_numeric")
    return("continuous")
  }
  "other"
}

# shapes (digits to 9, letters to A) only for code-like values without spaces
ci_shapes <- function(x) {
  v <- as.character(x[!is.na(x)])
  if (!length(v) || mean(!grepl("\\s", v) & nchar(v) <= 30L) < 0.95) return(NULL)
  shape <- gsub("[A-Za-z]", "A", gsub("[0-9]", "9", v))
  tab <- sort(table(shape), decreasing = TRUE)
  utils::head(ci_frame(level = names(tab), n_rows = as.numeric(tab)), 10L)
}

## summaries ---------------------------------------------------------------------

ci_numeric_row <- function(x, probs = ci_probs) {
  x <- as.numeric(x)
  codes <- vapply(ci_missing_codes, function(k) sum(x == k, na.rm = TRUE), numeric(1))
  v <- x[!is.na(x) & is.finite(x) & !x %in% ci_missing_codes]
  q <- if (length(v)) stats::quantile(v, probs, names = FALSE, type = 7) else rep(NA_real_, length(probs))
  # no mean, SD or skewness: one extreme value can dominate them and be recovered.
  # The 10% trimmed mean ignores the 10% most extreme values at each end.
  out <- ci_frame(n = length(v), mean_trimmed = if (length(v)) mean(v, trim = 0.1) else NA_real_,
    n_zero = sum(v == 0), n_negative = sum(v < 0),
    share_integer = if (length(v)) mean(v == round(v)) else NA_real_, decimals = ci_decimals(v))
  qf <- as.data.frame(as.list(q))
  names(qf) <- ci_qnames(probs)
  cf <- as.data.frame(as.list(codes))
  names(cf) <- paste0("n_code_", sub("-", "m", ci_missing_codes))
  cbind(out, qf, cf)
}

# level counts in rows and in distinct people (people decide label exposure)
ci_counts <- function(x, persons = NULL, lev = NULL) {
  if (is.null(lev)) {
    lev <- if (is.factor(x)) levels(x) else if (is.numeric(x)) as.character(sort(unique(x[!is.na(x)]))) else
      sort(unique(as.character(x[!is.na(x)])))
  }
  key <- factor(as.character(x), levels = lev)
  n_rows <- as.vector(table(key))
  n_persons <- rep(NA_real_, length(lev))
  if (!is.null(persons) && length(lev)) {
    keep <- !is.na(key) & !is.na(persons)
    n_persons <- as.vector(tapply(persons[keep], key[keep], function(p) length(unique(p))))
    n_persons[is.na(n_persons)] <- 0
  }
  ci_frame(level = lev, level_order = seq_along(lev), n_rows = n_rows, n_persons = n_persons)
}

ci_date_summary <- function(d) {
  v <- d[!is.na(d)]
  q <- if (length(v)) as.Date(stats::quantile(as.numeric(v), ci_probs, names = FALSE, type = 1),
    origin = "1970-01-01") else as.Date(rep(NA_character_, length(ci_probs)))
  out <- ci_frame(n = length(v), n_before_2019_12 = sum(v < as.Date("2019-12-01")),
    n_after_run_date = sum(v > Sys.Date()))
  qf <- as.data.frame(as.list(q))
  names(qf) <- ci_qnames(ci_probs)
  cbind(out, qf)
}

# people per anchor group, for each row of a table (NA anchors become "(missing)")
ci_anchor_values <- function(persons, ctx) {
  if (is.null(ctx$anchors) || is.null(persons)) return(NULL)
  i <- match(persons, ctx$anchors$subject)
  out <- lapply(setdiff(names(ctx$anchors), "subject"), function(a) {
    v <- ctx$anchors[[a]][i]
    v[is.na(v)] <- "(missing)"
    v
  })
  names(out) <- setdiff(names(ctx$anchors), "subject")
  if (!is.null(ctx$cohort)) out$main_cohort <- ifelse(persons %in% ctx$cohort, "Yes", "No")
  as.data.frame(out, stringsAsFactors = FALSE)
}

## table profile -----------------------------------------------------------------

ci_visit_from_name <- function(table) {
  if (grepl("_t0($|_)", table)) return("t0")
  if (grepl("_t1($|_)", table)) return("t1")
  NA_character_
}

ci_rows_per_person <- function(persons) {
  if (is.null(persons) || all(is.na(persons))) return(ci_frame(rpp_1 = NA, rpp_2 = NA, rpp_3_5 = NA,
    rpp_6_10 = NA, rpp_11_plus = NA))
  k <- as.vector(table(persons[!is.na(persons)]))
  ci_frame(rpp_1 = sum(k == 1), rpp_2 = sum(k == 2), rpp_3_5 = sum(k >= 3 & k <= 5),
    rpp_6_10 = sum(k >= 6 & k <= 10), rpp_11_plus = sum(k >= 11))
}

# Profile one data frame. persons: subject keys per row (found from a
# subject_id column when NULL). Distributions are skipped for small tables.
ci_profile_table <- function(res, df, source, table, ctx, persons = NULL, anchored = TRUE,
                             n_rows_source = NA, skip = character()) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, optional = TRUE)
  if (is.null(persons)) {
    j <- ci_subject_col(df)
    if (!is.na(j)) persons <- ci_key(df[[j]])
  }
  has_persons <- !is.null(persons)
  p <- if (has_persons) persons[!is.na(persons)] else character()
  person_level <- has_persons && length(p) > 0L && !anyDuplicated(p)
  n_persons <- if (has_persons) length(unique(p)) else NA_real_
  profile_n <- if (has_persons) n_persons else nrow(df)
  level <- if (profile_n >= ci_min_profile_n) "full" else "structure_only"
  ci_put(res, "tables", cbind(ci_frame(source = source, table = table, visit = ci_visit_from_name(table),
    n_rows = nrow(df), n_rows_source = n_rows_source, n_cols = ncol(df), has_persons = has_persons,
    person_level = person_level, n_persons = n_persons,
    n_persons_main = if (has_persons && !is.null(ctx$cohort)) length(intersect(unique(p), ctx$cohort)) else NA_real_,
    n_rows_no_person = if (has_persons) sum(is.na(persons)) else NA_real_, profile_level = level),
    ci_rows_per_person(persons)))
  if (level == "full" && has_persons && anchored) {
    anchors <- ci_anchor_values(persons, ctx)
  } else {
    anchors <- NULL
  }
  if (!person_level && !is.null(anchors)) {
    # many rows per person: profile how many, by anchor group
    keys <- unique(p)
    counts <- as.vector(table(factor(persons, levels = keys)))
    a1 <- ci_anchor_values(keys, ctx)
    rows <- list(cbind(ci_frame(source = source, table = table, column = ".rows_per_person",
      stratum_var = "all", stratum = "all"), ci_numeric_row(counts, ci_anchor_probs)))
    for (a in names(a1)) for (g in sort(unique(a1[[a]]))) {
      rows[[length(rows) + 1L]] <- cbind(ci_frame(source = source, table = table, column = ".rows_per_person",
        stratum_var = a, stratum = g), ci_numeric_row(counts[a1[[a]] == g], ci_anchor_probs))
    }
    ci_put(res, "numeric", ci_bind(rows))
    anchors <- NULL
  }
  numeric_cols <- list()
  for (pos in seq_along(df)) {
    name <- names(df)[pos]
    if (name %in% skip) next
    x <- tryCatch(ci_as_double(df[[pos]]), error = function(e) NULL)
    role <- if (is.null(x)) "other" else tryCatch(ci_role(x, name), error = function(e) "other")
    n_ok <- if (is.null(x) || is.list(x)) NA_real_ else sum(!is.na(x))
    ci_put(res, "columns", ci_frame(source = source, table = table, position = pos, column = name,
      class = class(df[[pos]])[1], role = role, n_nonmissing = n_ok, n_missing = nrow(df) - n_ok,
      n_distinct = if (is.null(x) || is.list(x)) NA_real_ else length(unique(x[!is.na(x)]))))
    if (level != "full") next
    stub <- function(sv = "all", st = "all") ci_frame(source = source, table = table, column = name,
      stratum_var = sv, stratum = st)
    if (role %in% c("categorical", "coded_numeric")) {
      ci_put(res, "categorical", cbind(stub(), kind = "level", ci_counts(x, persons)))
    }
    if (role %in% c("continuous", "coded_numeric")) {
      ci_put(res, "numeric", cbind(stub(), ci_numeric_row(x)))
      numeric_cols[[name]] <- as.numeric(x)
    }
    if (role == "date") {
      d <- ci_as_date(x)
      ci_put(res, "date_summary", cbind(ci_frame(source = source, table = table, column = name), ci_date_summary(d)))
      month <- format(d, "%Y-%m")
      ci_put(res, "categorical", cbind(stub(), kind = "month", ci_counts(month, persons)))
    }
    if (role %in% c("identifier", "high_cardinality")) {
      sh <- ci_shapes(x)
      if (!is.null(sh)) ci_put(res, "categorical", cbind(stub(), kind = "shape",
        ci_frame(level = sh$level, level_order = seq_len(nrow(sh)), n_rows = sh$n_rows, n_persons = NA_real_)))
    }
    if (!is.null(anchors)) {
      for (a in names(anchors)) {
        g <- anchors[[a]]
        if (role == "continuous") {
          parts <- split(as.numeric(x), g)
          ci_put(res, "numeric", ci_bind(lapply(names(parts), function(lv)
            cbind(stub(a, lv), ci_numeric_row(parts[[lv]], ci_anchor_probs)))))
        }
        if (role %in% c("categorical", "coded_numeric") && a %in% ci_categorical_anchors &&
            length(unique(x[!is.na(x)])) <= ci_max_anchor_levels) {
          lev <- ci_counts(x)$level
          parts <- split(seq_along(g), g)
          ci_put(res, "categorical", ci_bind(lapply(names(parts), function(lv) {
            i <- parts[[lv]]
            cbind(stub(a, lv), kind = "level", ci_counts(x[i], persons[i], lev))
          })))
        }
      }
    }
  }
  if (level == "full" && person_level && length(numeric_cols) >= 2L) {
    ci_put(res, "correlations", cbind(ci_frame(source = source, scope = table), ci_correlations(numeric_cols)))
  }
  invisible(list(person_level = person_level, persons = persons))
}

## correlations, visits and timing --------------------------------------------------

# Rank correlations with column-wise ranks and pairwise-complete pairs.
ci_correlations <- function(cols) {
  cols <- lapply(cols, function(v) {
    v <- as.numeric(v)
    v[!is.finite(v) | v %in% ci_missing_codes] <- NA
    v
  })
  nn <- vapply(cols, function(v) sum(!is.na(v)), numeric(1))
  sdv <- vapply(cols, function(v) if (sum(!is.na(v)) > 1L) stats::sd(v, na.rm = TRUE) else 0, numeric(1))
  keep <- names(cols)[nn >= ci_min_cor_n & sdv > 0]
  if (length(keep) < 2L) return(NULL)
  keep <- keep[order(-nn[keep])][seq_len(min(length(keep), ci_max_cor_columns))]
  m <- do.call(cbind, lapply(cols[keep], rank, na.last = "keep", ties.method = "average"))
  rho <- suppressWarnings(stats::cor(m, use = "pairwise.complete.obs"))
  n <- crossprod(!is.na(m))
  ut <- which(upper.tri(rho), arr.ind = TRUE)
  out <- ci_frame(var1 = keep[ut[, 1]], var2 = keep[ut[, 2]], rho = rho[ut], n = n[ut])
  out[!is.na(out$rho) & out$n >= ci_min_cor_n, , drop = FALSE]
}

ci_person_table <- function(df) {
  j <- ci_subject_col(df)
  if (is.na(j)) return(NULL)
  k <- ci_key(df[[j]])
  if (anyDuplicated(k[!is.na(k)])) return(NULL)
  df <- as.data.frame(df, stringsAsFactors = FALSE, optional = TRUE)[!is.na(k), , drop = FALSE]
  attr(df, "ci_keys") <- k[!is.na(k)]
  df
}

ci_panel_correlations <- function(res, source, panel, tables) {
  cols <- list()
  keys <- NULL
  parts <- lapply(tables, ci_person_table)
  if (any(vapply(parts, is.null, logical(1)))) return(invisible(NULL))
  keys <- Reduce(union, lapply(parts, attr, "ci_keys"))
  for (t in names(parts)) {
    df <- parts[[t]]
    i <- match(keys, attr(df, "ci_keys"))
    for (name in names(df)) {
      x <- df[[name]]
      role <- tryCatch(ci_role(x, name), error = function(e) "other")
      if (role %in% c("continuous", "coded_numeric")) cols[[paste0(basename(t), ":", name)]] <- as.numeric(x)[i]
    }
  }
  if (length(cols) >= 2L) ci_put(res, "correlations", cbind(ci_frame(source = source, scope = paste0("panel:", panel)),
    ci_correlations(cols)))
  invisible(NULL)
}

ci_visit_pairs <- function(res, source, family, t0, t1) {
  a <- ci_person_table(t0)
  b <- ci_person_table(t1)
  if (is.null(a) || is.null(b)) return(invisible(NULL))
  both <- intersect(attr(a, "ci_keys"), attr(b, "ci_keys"))
  if (length(both) < ci_min_numeric_n) return(invisible(NULL))
  ia <- match(both, attr(a, "ci_keys"))
  ib <- match(both, attr(b, "ci_keys"))
  rows <- list()
  for (name in setdiff(intersect(names(a), names(b)), names(a)[ci_subject_col(a)])) {
    role <- tryCatch(ci_role(a[[name]], name), error = function(e) "other")
    if (!role %in% c("continuous", "coded_numeric")) next
    v0 <- suppressWarnings(as.numeric(a[[name]][ia]))
    v1 <- suppressWarnings(as.numeric(b[[name]][ib]))
    v0[v0 %in% ci_missing_codes] <- NA
    v1[v1 %in% ci_missing_codes] <- NA
    ok <- !is.na(v0) & !is.na(v1) & is.finite(v0) & is.finite(v1)
    if (sum(ok) < ci_min_numeric_n) next
    rho <- if (stats::sd(v0[ok]) > 0 && stats::sd(v1[ok]) > 0) stats::cor(rank(v0[ok]), rank(v1[ok])) else NA_real_
    # change from baseline to follow-up, summarised by quantiles only
    diff <- if (role == "continuous") ci_numeric_row(v1[ok] - v0[ok], ci_anchor_probs)[c("mean_trimmed", ci_qnames(ci_anchor_probs))] else
      ci_frame(mean_trimmed = NA_real_)
    rows[[length(rows) + 1L]] <- cbind(ci_frame(source = source, family = family, column = name, role = role,
      n = sum(ok), rho = rho, share_same = mean(v0[ok] == v1[ok])), diff)
  }
  ci_put(res, "visit_pairs", ci_bind(rows))
  invisible(NULL)
}

ci_timing <- function(res, source, tables) {
  rows <- list()
  for (i in seq_len(nrow(ci_timing_spec))) {
    s <- ci_timing_spec[i, ]
    a <- if (is.null(tables[[s$from_table]])) NULL else ci_person_table(tables[[s$from_table]])
    b <- if (is.null(tables[[s$to_table]])) NULL else ci_person_table(tables[[s$to_table]])
    if (is.null(a) || is.null(b) || !s$from_col %in% names(a) || !s$to_col %in% names(b)) next
    both <- intersect(attr(a, "ci_keys"), attr(b, "ci_keys"))
    d0 <- ci_as_date(a[[s$from_col]])[match(both, attr(a, "ci_keys"))]
    d1 <- ci_as_date(b[[s$to_col]])[match(both, attr(b, "ci_keys"))]
    days <- as.numeric(d1 - d0)
    rows[[length(rows) + 1L]] <- cbind(ci_frame(source = source, label = s$label,
      from = paste0(s$from_table, ":", s$from_col), to = paste0(s$to_table, ":", s$to_col)),
      ci_numeric_row(days))
  }
  ci_put(res, "timing", ci_bind(rows))
  invisible(NULL)
}

## omics ---------------------------------------------------------------------------

ci_visit <- function(meta, sample_key = NULL) {
  out <- rep("unknown", nrow(meta))
  k <- tolower(names(meta))
  j <- which(k %in% c("timepoint", "visit", "time_point", "visit_name", "project_stage"))[1]
  if (!is.na(j)) {
    v <- tolower(as.character(meta[[j]]))
    out[grepl("first|baseline|t0|^0$|^1$", v)] <- "t0"
    out[grepl("second|follow|recall|t1|lcr|^2$", v)] <- "t1"
  }
  if (!is.null(sample_key)) {
    lcr <- grepl("^LCR", sample_key)
    out[out == "unknown" & !is.na(sample_key) & lcr] <- "t1"
    out[out == "unknown" & !is.na(sample_key) & !lcr] <- "t0"
  }
  out
}

ci_map <- function(keys, via, ctx) {
  lk <- if (identical(via, "lookup")) ctx$sample_lookup else if (identical(via, "identifiers")) ctx$identifiers else NULL
  if (is.null(lk)) return(rep(NA_character_, length(keys)))
  unname(lk[keys])
}

# subject keys for the rows of a table: subject_id column, else a mapped ID column
ci_table_persons <- function(df, src, ctx) {
  j <- ci_subject_col(df)
  if (!is.na(j)) return(ci_key(df[[j]]))
  if (!is.null(src$id_col) && src$id_col %in% names(df) && !is.null(src$map_via)) {
    return(ci_map(ci_key(df[[src$id_col]]), src$map_via, ctx))
  }
  NULL
}

ci_choose_transform <- function(m, transform) {
  if (transform != "auto") return(transform)
  v <- m[!is.na(m)]
  if (!length(v) || min(v) < 0) return("none")
  med <- stats::median(apply(m, 2, stats::median, na.rm = TRUE), na.rm = TRUE)
  skews <- apply(m, 2, function(x) ci_skew(x[!is.na(x)]))
  if (stats::median(skews, na.rm = TRUE) < 1) return("none")
  if (is.finite(med) && med > 10) "log2p1" else "log2eps"
}

ci_apply_transform <- function(m, tr) {
  if (tr == "log2p1") return(log2(m + 1))
  if (tr == "log2eps") {
    pos <- m[!is.na(m) & m > 0]
    eps <- if (length(pos)) min(pos) / 2 else 1e-4
    return(log2(m + eps))
  }
  m
}

# Rank-based normal scores per feature: no single sample can dominate a
# component (so loadings cannot mirror one person's profile), and this is the
# Gaussian-copula scale a generator would use.
ci_normal_scores <- function(x) {
  r <- rank(x, na.last = "keep", ties.method = "average")
  stats::qnorm((r - 0.5) / sum(!is.na(x)))
}

ci_lowrank <- function(mt, max_features = ci_omics_max_features, k_load = 10L) {
  v <- apply(mt, 2, stats::var, na.rm = TRUE)
  keep <- which(colMeans(is.na(mt)) <= 0.5 & is.finite(v) & v > 0)
  if (length(keep) > max_features) keep <- keep[order(-v[keep])][seq_len(max_features)]
  if (length(keep) < 3L || nrow(mt) < ci_min_cor_n) return(NULL)
  z <- apply(mt[, keep, drop = FALSE], 2, ci_normal_scores)
  z[is.na(z)] <- 0
  z <- scale(z)
  z[is.na(z)] <- 0
  e <- eigen(crossprod(z) / (nrow(z) - 1), symmetric = TRUE)
  k <- min(k_load, length(e$values))
  list(features = colnames(mt)[keep], values = e$values, vectors = e$vectors[, seq_len(k), drop = FALSE],
    scores = z %*% e$vectors[, seq_len(min(5L, k)), drop = FALSE], n = nrow(z))
}

# share of each principal component's variance explained by a covariate
ci_r2 <- function(scores, covariates) {
  rows <- list()
  for (name in names(covariates)) {
    x <- covariates[[name]]
    for (pc in seq_len(ncol(scores))) {
      y <- scores[, pc]
      if (is.numeric(x)) {
        ok <- !is.na(x) & !is.na(y) & is.finite(x)
        if (sum(ok) < ci_min_cor_n || stats::sd(x[ok]) == 0) next
        r2 <- stats::cor(x[ok], y[ok])^2
        n <- sum(ok)
      } else {
        g <- as.character(x)
        small <- names(which(table(g) < ci_min_cell))
        g[g %in% small] <- NA
        ok <- !is.na(g) & !is.na(y)
        if (sum(ok) < ci_min_cor_n || length(unique(g[ok])) < 2L) next
        fit <- stats::ave(y[ok], g[ok])
        r2 <- sum((fit - mean(y[ok]))^2) / sum((y[ok] - mean(y[ok]))^2)
        n <- sum(ok)
      }
      rows[[length(rows) + 1L]] <- ci_frame(covariate = name, pc = pc, r2 = r2, n = n)
    }
  }
  ci_bind(rows)
}

ci_visit_stability <- function(mt, meta) {
  s <- meta$.subject
  i0 <- which(meta$.visit == "t0" & !is.na(s))
  i1 <- which(meta$.visit == "t1" & !is.na(s))
  i0 <- i0[!duplicated(s[i0])]
  i1 <- i1[!duplicated(s[i1])]
  both <- intersect(s[i0], s[i1])
  if (length(both) < ci_min_numeric_n) return(NULL)
  a <- mt[i0[match(both, s[i0])], , drop = FALSE]
  b <- mt[i1[match(both, s[i1])], , drop = FALSE]
  out <- t(vapply(seq_len(ncol(mt)), function(j) {
    ok <- !is.na(a[, j]) & !is.na(b[, j])
    if (sum(ok) < ci_min_numeric_n || stats::sd(a[ok, j]) == 0 || stats::sd(b[ok, j]) == 0) return(c(sum(ok), NA))
    c(sum(ok), stats::cor(rank(a[ok, j]), rank(b[ok, j])))
  }, numeric(2)))
  ci_frame(feature = colnames(mt), n = out[, 1], rho = out[, 2])
}

# mat: samples x features. meta: one row per sample with .subject and .visit
# plus any sample metadata (plate, batch, run order). Features are profiled on
# samples mapped to cohort participants when at least 50 are mapped.
ci_profile_omics <- function(res, source, platform, mat, meta, ctx, transform = "auto",
                             max_features = ci_omics_max_features, min_share_nonzero = 0,
                             extra = list(), n_samples_source = nrow(mat)) {
  if (!is.double(mat)) storage.mode(mat) <- "double"
  if (is.null(colnames(mat))) colnames(mat) <- paste0("feature_", seq_len(ncol(mat)))
  mapped <- !is.na(meta$.subject)
  use <- if (sum(mapped) >= ci_min_profile_n) mapped else rep(TRUE, nrow(mat))
  m <- if (all(use)) mat else mat[use, , drop = FALSE]
  md <- meta[use, , drop = FALSE]
  tr <- ci_choose_transform(m, transform)
  mt <- ci_apply_transform(m, tr)
  subj <- unique(md$.subject[!is.na(md$.subject)])
  feat <- t(vapply(seq_len(ncol(m)), function(j) {
    v <- m[, j]
    ok <- !is.na(v)
    vv <- v[ok]
    q <- if (length(vv)) stats::quantile(vv, ci_probs, names = FALSE, type = 7) else rep(NA_real_, length(ci_probs))
    c(length(vv), 1 - mean(ok), if (length(vv)) mean(vv == 0) else NA, if (length(vv)) mean(vv == min(vv)) else NA, q)
  }, numeric(4L + length(ci_probs))))
  colnames(feat) <- c("n", "share_missing", "share_zero", "share_at_min", ci_qnames(ci_probs))
  features <- cbind(ci_frame(source = source, platform = platform, feature = colnames(m), n_total = nrow(m)),
    as.data.frame(feat))
  kept <- if (min_share_nonzero > 0) features$share_zero <= 1 - min_share_nonzero else rep(TRUE, nrow(features))
  ci_put(res, "omics_features", features[kept, , drop = FALSE])
  ci_put(res, "omics_platform", ci_frame(source = source, platform = platform,
    n_samples_source = n_samples_source, n_samples = nrow(mat), n_samples_mapped = sum(mapped),
    n_samples_profiled = nrow(m), n_persons = length(subj),
    n_persons_main = if (is.null(ctx$cohort)) NA_real_ else length(intersect(subj, ctx$cohort)),
    n_samples_t0 = sum(md$.visit == "t0"), n_samples_t1 = sum(md$.visit == "t1"),
    n_samples_visit_unknown = sum(md$.visit == "unknown"), n_features = ncol(m),
    n_features_profiled = sum(kept), share_missing = mean(is.na(m)), share_nonpositive = mean(m <= 0, na.rm = TRUE),
    transform = tr, profiled_on = if (all(use)) "all_samples" else "mapped_samples"))
  # per-sample summaries (each sample is one person-visit; quantiles only)
  sample_stats <- c(list(share_missing = rowMeans(is.na(m)), median_t = apply(mt, 1, stats::median, na.rm = TRUE)),
    lapply(extra, function(x) x[use]))
  ci_put(res, "omics_samples", ci_bind(lapply(names(sample_stats), function(st)
    cbind(ci_frame(source = source, platform = platform, stat = st), ci_numeric_row(sample_stats[[st]], ci_probs)))))
  lr <- ci_lowrank(mt[, kept, drop = FALSE], max_features)
  if (!is.null(lr)) {
    share <- lr$values / sum(lr$values)
    k <- seq_len(min(20L, length(share)))
    ci_put(res, "omics_eigen", ci_frame(source = source, platform = platform, pc = k, eigenvalue = lr$values[k],
      share = share[k], cumulative = cumsum(share)[k], n_samples = lr$n, n_features = length(lr$features)))
    load <- lr$vectors
    colnames(load) <- paste0("pc", seq_len(ncol(load)))
    ci_put(res, "omics_loadings", cbind(ci_frame(source = source, platform = platform, feature = lr$features),
      as.data.frame(load)))
    covs <- list(visit = md$.visit)
    an <- ci_anchor_values(md$.subject, ctx)
    if (!is.null(an)) covs <- c(covs, as.list(an))
    for (name in setdiff(names(md), c(".subject", ".visit", ".sample"))) {
      x <- md[[name]]
      role <- tryCatch(ci_role(x, name), error = function(e) "other")
      if (role %in% c("categorical", "coded_numeric") && length(unique(x[!is.na(x)])) >= 2L) covs[[paste0("meta:", name)]] <- as.character(x)
      if (role == "continuous") covs[[paste0("meta:", name)]] <- as.numeric(x)
    }
    ci_put(res, "omics_r2", cbind(ci_frame(source = source, platform = platform), ci_r2(lr$scores, covs)))
  }
  stab <- ci_visit_stability(mt[, kept, drop = FALSE], md)
  if (!is.null(stab)) ci_put(res, "omics_visit", cbind(ci_frame(source = source, platform = platform), stab))
  others <- setdiff(names(md), c(".subject", ".visit", ".sample"))
  if (length(others)) {
    ci_profile_table(res, md[others], source, paste0(platform, "/samples"), ctx, persons = md$.subject, anchored = FALSE)
  }
  invisible(NULL)
}

ci_metadata_pattern <- paste0("^(subject_id|timepoint|visit|age|sex|gender|batch|plate|run|order|well|position|",
  "date|time|site|panel|qc|lod|dilution|sample|type|status|group|project|study|class|injection|acquired|ethnic)")

# Long tables (one row per sample and feature, as Olink NPX) are pivoted;
# wide tables keep one row per sample with features in columns.
ci_omics_from_table <- function(res, source, platform, df, src, ctx) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, optional = TRUE)
  nm <- names(df)
  k <- tolower(nm)
  value_col <- nm[k %in% c("npx", "quantified_value", "value", "intensity", "abundance", "concentration", "conc")][1]
  feature_col <- nm[k %in% c("assay", "olinkid", "olink_id", "uniprot", "protein", "protein_name", "feature",
    "target", "analyte", "gene", "metabolite", "lipid")][1]
  sample_col <- if (!is.null(src$sample_col) && src$sample_col %in% nm) src$sample_col else
    nm[gsub("[._ ]", "", k) %in% c("sampleid", "barcode", "samplename", "sample")][1]
  subj_j <- ci_subject_col(df)
  if (!is.na(value_col) && !is.na(feature_col)) {
    # long layout: sample key from the sample column, else subject and visit
    key <- if (!is.na(sample_col)) ci_key(df[[sample_col]]) else
      paste(ci_key(df[[subj_j]]), ci_visit(df), sep = "|")
    feat <- as.character(df[[feature_col]])
    val <- ci_numeric_tokens(df[[value_col]])
    samples <- unique(key[!is.na(key)])
    features <- unique(feat[!is.na(feat)])
    ok <- !is.na(key) & !is.na(feat) & !is.na(val$value)
    cell <- (match(feat[ok], features) - 1) * length(samples) + match(key[ok], samples)
    sums <- rowsum(val$value[ok], cell)
    cnt <- rowsum(rep(1, sum(ok)), cell)
    mat <- matrix(NA_real_, length(samples), length(features), dimnames = list(NULL, features))
    mat[as.numeric(rownames(sums))] <- sums / cnt
    first <- match(samples, key)
    # sample metadata: columns constant within sample (checked on a subset)
    check <- which(key %in% utils::head(samples, 200L))
    const <- vapply(setdiff(nm, c(value_col, feature_col)), function(col) {
      v <- df[[col]][check]
      all(tapply(v, key[check], function(z) length(unique(z)) <= 1L))
    }, logical(1))
    meta <- df[first, names(const)[const], drop = FALSE]
    ci_profile_table(res, df[setdiff(nm, c(value_col, names(const)[const]))], source,
      paste0(platform, "/long_rows"), ctx, persons = if (is.na(subj_j)) NULL else ci_key(df[[subj_j]]), anchored = FALSE)
    ci_put(res, "omics_platform_notes", ci_frame(source = source, platform = platform, layout = "long",
      value_column = value_col, feature_column = feature_col, sample_column = if (is.na(sample_col)) "subject_id|visit" else sample_col,
      n_duplicate_cells = sum(cnt > 1)))
    if (!is.null(val$tokens)) ci_put(res, "omics_tokens", ci_frame(source = source, platform = platform,
      feature = value_col, token = names(val$tokens), n = as.numeric(val$tokens)))
    sample_key <- if (!is.na(sample_col)) samples else NULL
  } else {
    from <- src$feature_from
    if (!is.null(from) && !is.na(from) && ncol(df) >= from) {
      fcols <- nm[from:ncol(df)]
    } else {
      fcols <- nm[!vapply(nm, ci_is_id_name, logical(1)) & !grepl(ci_metadata_pattern, k) &
        !grepl(ci_withheld_pattern, k)]
    }
    parsed <- lapply(df[fcols], ci_numeric_tokens)
    numeric_share <- vapply(parsed, function(p) {
      nn <- sum(!is.na(p$value))
      nt <- if (is.null(p$tokens)) 0 else sum(p$tokens)
      if (nn + nt == 0) 0 else nn / (nn + nt)
    }, numeric(1))
    fcols <- fcols[numeric_share >= 0.5]
    if (length(fcols) < 5L) stop("no feature columns recognised")
    mat <- do.call(cbind, lapply(parsed[fcols], `[[`, "value"))
    colnames(mat) <- fcols
    for (f in fcols) if (!is.null(parsed[[f]]$tokens)) ci_put(res, "omics_tokens", ci_frame(source = source,
      platform = platform, feature = f, token = names(parsed[[f]]$tokens), n = as.numeric(parsed[[f]]$tokens)))
    meta <- df[setdiff(nm, fcols)]
    key <- if (!is.na(sample_col)) ci_key(df[[sample_col]]) else NULL
    sample_key <- key
    ci_put(res, "omics_platform_notes", ci_frame(source = source, platform = platform, layout = "wide",
      sample_column = if (is.na(sample_col)) NA_character_ else sample_col, first_feature = fcols[1],
      n_duplicate_cells = NA_real_))
  }
  subject <- if ("subject_id" %in% tolower(names(meta))) ci_key(meta[[ci_subject_col(meta)]]) else
    if (!is.null(sample_key) && !is.null(src$map_via)) ci_map(sample_key, src$map_via, ctx) else rep(NA_character_, nrow(meta))
  if (!is.null(sample_key) && !is.null(src$map_via)) {
    fill <- is.na(subject)
    subject[fill] <- ci_map(sample_key[fill], src$map_via, ctx)
  }
  meta$.subject <- subject
  meta$.visit <- ci_visit(meta, sample_key)
  ci_profile_omics(res, source, platform, mat, meta, ctx)
}

## readers -------------------------------------------------------------------------

# walk plain lists; anything else is a leaf
ci_leaves <- function(x, path = character()) {
  if (is.list(x) && identical(class(x), "list")) {
    nms <- names(x)
    if (is.null(nms)) nms <- paste0("[", seq_along(x), "]")
    nms[!nzchar(nms)] <- paste0("[", which(!nzchar(nms)), "]")
    return(do.call(c, lapply(seq_along(x), function(i) ci_leaves(x[[i]], c(path, nms[i])))))
  }
  list(list(path = paste(path, collapse = "/"), value = x))
}

ci_structure_row <- function(source, path, x) {
  d <- dim(x)
  ci_frame(source = source, path = path, class = paste(class(x), collapse = "/"),
    n_rows = if (is.null(d)) length(x) else d[1], n_cols = if (is.null(d)) NA_real_ else d[2])
}

ci_restrict <- function(df, persons, ctx) {
  if (is.null(persons) || is.null(ctx$cohort)) return(list(df = df, persons = persons, n = NA_real_))
  keep <- !is.na(persons) & persons %in% ctx$cohort
  list(df = df[keep, , drop = FALSE], persons = persons[keep], n = nrow(df))
}

ci_profile_frame <- function(res, df, src, table, ctx) {
  persons <- ci_table_persons(df, src, ctx)
  n_source <- NA_real_
  if (isTRUE(src$cohort_only)) {
    r <- ci_restrict(df, persons, ctx)
    df <- r$df
    persons <- r$persons
    n_source <- r$n
  }
  ci_profile_table(res, df, src$id, table, ctx, persons = persons, n_rows_source = n_source)
}

ci_read_rds_tree <- function(res, src, ctx) {
  x <- readRDS(src$path)
  leaves <- ci_leaves(x, character())
  tables <- list()
  for (leaf in leaves) {
    path <- if (nzchar(leaf$path)) leaf$path else src$id
    ci_put(res, "structure", ci_structure_row(src$id, path, leaf$value))
    if (is.data.frame(leaf$value)) {
      ci_profile_frame(res, leaf$value, src, path, ctx)
      tables[[path]] <- leaf$value
    }
  }
  if (isTRUE(src$wrangled)) {
    if (!is.null(ctx$master_dates)) tables$master <- ctx$master_dates
    for (panel in names(ci_panels)) {
      parts <- tables[ci_panels[[panel]]]
      if (!any(vapply(parts, is.null, logical(1)))) ci_panel_correlations(res, src$id, panel, parts)
    }
    for (family in names(ci_visit_families)) {
      f <- ci_visit_families[[family]]
      if (!is.null(tables[[f[1]]]) && !is.null(tables[[f[2]]])) ci_visit_pairs(res, src$id, family, tables[[f[1]]], tables[[f[2]]])
    }
    ci_timing(res, src$id, tables)
  }
  invisible(NULL)
}

ci_read_master <- function(res, src, ctx) {
  df <- readRDS(src$path)
  ci_put(res, "structure", ci_structure_row(src$id, src$id, df))
  ci_profile_frame(res, df, src, "combined_data_set", ctx)
  # data availability: flags as categorical counts among main-cohort members
  persons <- ci_key(df[[ci_subject_col(df)]])
  in_pop <- if (is.null(ctx$cohort)) rep(TRUE, nrow(df)) else persons %in% ctx$cohort
  flags <- grep("^(has_|attended_|completed_)", names(df), value = TRUE)
  if (!length(flags)) return(invisible(NULL))
  yes <- as.data.frame(lapply(df[in_pop, flags, drop = FALSE], function(v)
    !is.na(v) & tolower(as.character(v)) %in% c("yes", "1", "true")), check.names = FALSE)
  pop <- if (is.null(ctx$cohort)) "all_master_rows" else "main_cohort"
  anchors <- ci_anchor_values(persons[in_pop], ctx)
  for (f in flags) {
    lab <- ifelse(yes[[f]], "Yes", "No")
    ci_put(res, "categorical", cbind(ci_frame(source = src$id, table = paste0("availability:", pop), column = f,
      stratum_var = "all", stratum = "all", kind = "availability"), ci_counts(factor(lab, c("Yes", "No")), persons[in_pop])))
    if (!is.null(anchors)) for (a in intersect(c("sex", "age_group", "lc_group", "route"), names(anchors))) {
      parts <- split(seq_along(lab), anchors[[a]])
      ci_put(res, "categorical", ci_bind(lapply(names(parts), function(lv) cbind(ci_frame(source = src$id,
        table = paste0("availability:", pop), column = f, stratum_var = a, stratum = lv, kind = "availability"),
        ci_counts(factor(lab[parts[[lv]]], c("Yes", "No")), persons[in_pop][parts[[lv]]])))))
    }
  }
  m <- as.matrix(yes) * 1
  both <- crossprod(m)
  ut <- which(upper.tri(both), arr.ind = TRUE)
  ci_put(res, "availability_pairs", ci_frame(source = src$id, population = pop, flag1 = flags[ut[, 1]],
    flag2 = flags[ut[, 2]], n_both = both[ut]))
  groups <- list(baseline = grep("_t0$|genome", flags, value = TRUE), followup = grep("_t1$", flags, value = TRUE),
    linkage = grep("linked", flags, value = TRUE), later_studies = grep("cog|2022|2025", flags, value = TRUE))
  for (g in names(groups)) {
    fs <- groups[[g]]
    if (length(fs) < 2L) next
    pattern <- apply(yes[fs], 1, function(r) if (any(r)) paste(fs[r], collapse = "+") else "(none)")
    tab <- sort(table(pattern), decreasing = TRUE)
    ci_put(res, "categorical", ci_frame(source = src$id, table = paste0("availability_patterns:", pop), column = g,
      stratum_var = "all", stratum = "all", kind = "pattern", level = names(tab), level_order = seq_along(tab),
      n_rows = as.numeric(tab), n_persons = as.numeric(tab)))
  }
  invisible(NULL)
}

ci_flatten_sets <- function(x, path = character()) {
  if (is.data.frame(x)) {
    j <- ci_subject_col(x)
    return(if (is.na(j)) list() else stats::setNames(list(unique(ci_key(x[[j]]))), paste(path, collapse = "/")))
  }
  if (is.list(x)) {
    nms <- names(x)
    if (is.null(nms)) nms <- paste0("[", seq_along(x), "]")
    return(do.call(c, lapply(seq_along(x), function(i) ci_flatten_sets(x[[i]], c(path, nms[i])))))
  }
  k <- unique(ci_key(x))
  stats::setNames(list(k[!is.na(k)]), paste(path, collapse = "/"))
}

ci_read_id_sets <- function(res, src, ctx) {
  sets <- ci_flatten_sets(readRDS(src$path))
  ci_put(res, "id_sets", ci_frame(source = src$id, set = names(sets),
    n = vapply(sets, length, numeric(1)),
    n_main = vapply(sets, function(s) if (is.null(ctx$cohort)) NA_real_ else length(intersect(s, ctx$cohort)), numeric(1))))
  parent <- dirname(names(sets))
  rows <- list()
  for (p in unique(parent)) {
    nm <- names(sets)[parent == p]
    if (length(nm) < 2L) next
    for (i in seq_along(nm)[-length(nm)]) for (j in (i + 1L):length(nm)) {
      rows[[length(rows) + 1L]] <- ci_frame(source = src$id, set1 = nm[i], set2 = nm[j],
        n_both = length(intersect(sets[[nm[i]]], sets[[nm[j]]])))
    }
  }
  ci_put(res, "id_overlaps", ci_bind(rows))
  invisible(NULL)
}

ci_read_provenance <- function(res, src, ctx) {
  for (what in names(src$path)) {
    if (!file.exists(src$path[[what]])) next
    x <- readRDS(src$path[[what]])
    for (name in names(x)) {
      v <- x[[name]]
      if (inherits(v, "sessionInfo")) {
        pk <- c(v$otherPkgs, v$loadedOnly)
        ci_put(res, "provenance", ci_frame(file = what, key = c("R", paste0("package:", names(pk))),
          value = c(v$R.version$version.string, vapply(pk, function(p) p$Version, character(1)))))
      } else if (is.atomic(v) && length(v) == 1L) {
        ci_put(res, "provenance", ci_frame(file = what, key = name, value = format(v)))
      } else if (is.atomic(v) && !is.null(names(v))) {
        ci_put(res, "provenance", ci_frame(file = what, key = paste0(name, ":", basename(names(v))), value = format(v)))
      }
    }
  }
  invisible(NULL)
}

ci_read_omics_list <- function(res, src, ctx) {
  x <- readRDS(src$path)
  for (name in names(x)) {
    ci_put(res, "structure", ci_structure_row(src$id, name, x[[name]]))
    if (is.data.frame(x[[name]])) ci_omics_from_table(res, src$id, sub("^df_", "", name), x[[name]], src, ctx)
  }
  invisible(NULL)
}

ci_read_omics_csv <- function(res, src, ctx) {
  df <- utils::read.csv(src$path, check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
  ci_put(res, "structure", ci_structure_row(src$id, basename(src$path), df))
  ci_omics_from_table(res, src$id, src$id, df, src, ctx)
}

ci_read_omics_rds <- function(res, src, ctx) {
  df <- readRDS(src$path)
  ci_put(res, "structure", ci_structure_row(src$id, basename(src$path), df))
  ci_omics_from_table(res, src$id, src$id, df, src, ctx)
}

# Expression matrices are large (genes x samples): count summaries and the
# expressed-gene filter run in chunks of genes so no full-size logical copy is made.
ci_read_tximport <- function(res, src, ctx, chunk = 5000L) {
  x <- readRDS(src$path)
  for (name in names(x)) ci_put(res, "structure", ci_structure_row(src$id, name, x[[name]]))
  x$length <- NULL   # transcript lengths are not profiled; free them first
  ab <- if (!is.null(x$abundance)) x$abundance else x$counts
  if (is.null(ab) || is.null(colnames(ab))) stop("no abundance or counts matrix with sample names")
  key <- ci_key(colnames(ab))
  subject <- ci_map(key, src$map_via, ctx)
  meta <- data.frame(.subject = subject, .visit = ci_visit(data.frame(row.names = seq_along(key)), key),
    stringsAsFactors = FALSE)
  use <- if (sum(!is.na(subject)) >= ci_min_profile_n) which(!is.na(subject)) else seq_along(key)
  chunks <- split(seq_len(nrow(ab)), ceiling(seq_len(nrow(ab)) / chunk))
  extra <- list()
  if (!is.null(x$counts)) {
    lib <- det <- numeric(length(use))
    for (g in chunks) {
      cm <- x$counts[g, use, drop = FALSE]
      lib <- lib + colSums(cm, na.rm = TRUE)
      det <- det + colSums(cm > 0, na.rm = TRUE)
    }
    extra <- list(library_size = lib, detected_features = det)
  }
  rm(x)
  invisible(gc())
  # genes expressed (non-zero) in at least 10% of the profiled samples
  keep <- unlist(lapply(chunks, function(g) rowMeans(ab[g, use, drop = FALSE] > 0, na.rm = TRUE) >= 0.1))
  n_source <- ncol(ab)
  n_genes <- nrow(ab)
  mat <- t(ab[keep, use, drop = FALSE])
  rm(ab)
  invisible(gc())
  ci_put(res, "omics_platform_notes", ci_frame(source = src$id, platform = "rna_seq", layout = "tximport",
    n_features_source = n_genes, feature_filter = "non-zero in at least 10% of profiled samples"))
  ci_profile_omics(res, src$id, "rna_seq", mat, meta[use, , drop = FALSE], ctx, transform = "log2p1",
    extra = extra, n_samples_source = n_source)
}

ci_read_xlsx <- function(path, sheet) {
  if (requireNamespace("openxlsx", quietly = TRUE)) return(openxlsx::read.xlsx(path, sheet = sheet, check.names = FALSE))
  if (requireNamespace("readxl", quietly = TRUE)) return(as.data.frame(readxl::read_excel(path, sheet = sheet)))
  stop("reading xlsx needs openxlsx or readxl")
}

ci_clean_names <- function(x) {
  x <- gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(x)))
  make.unique(ifelse(nzchar(x), x, "x"), sep = "_")
}

ci_read_xlsx_dir <- function(res, src, ctx) {
  files <- list.files(src$path, pattern = "[.]xlsx$", full.names = TRUE)
  files <- files[!startsWith(basename(files), "~$")]
  if (!length(files)) stop("no xlsx files")
  sheets <- lapply(files, function(f) {
    d <- ci_read_xlsx(f, 1L)
    names(d) <- make.names(names(d))   # as openxlsx check.names = TRUE in stage 02
    d[] <- lapply(d, function(v) if (is.numeric(v) || is.character(v)) v else as.character(v))
    d
  })
  # bind sheets whose column types differ by falling back to character
  cols <- unique(unlist(lapply(sheets, names)))
  for (col in cols) {
    classes <- unique(unlist(lapply(sheets, function(d) if (col %in% names(d)) class(d[[col]])[1])))
    if (length(classes) > 1L) sheets <- lapply(sheets, function(d) { if (col %in% names(d)) d[[col]] <- as.character(d[[col]]); d })
  }
  df <- ci_bind(sheets)
  ci_put(res, "structure", ci_frame(source = src$id, path = "manifests", class = "xlsx", n_rows = nrow(df),
    n_cols = ncol(df)))
  ci_put(res, "structure", ci_frame(source = src$id, path = "manifest_files", class = "count", n_rows = length(files),
    n_cols = NA_real_))
  ci_profile_frame(res, df, src, "manifests", ctx)
}

ci_read_autoantibodies <- function(res, src, ctx) {
  for (part in names(src$sheets)) {
    df <- ci_read_xlsx(src$path, src$sheets[[part]])
    # the controls sheet carries its first six names in its first data row (stage 02)
    if (part %in% src$header_in_first_row && nrow(df) > 1L && ncol(df) >= 6L) {
      head <- as.character(unlist(df[1, 1:6]))
      if (!anyNA(head)) {
        names(df)[1:6] <- head
        df <- df[-1, , drop = FALSE]
      }
    }
    names(df) <- ci_clean_names(names(df))
    df[] <- lapply(df, function(v) { if (is.character(v)) v[v %in% c("INSUF", "No FU sample")] <- NA; v })
    ci_put(res, "structure", ci_structure_row(src$id, part, df))
    ci_profile_frame(res, df, src, part, ctx)
  }
  invisible(NULL)
}

ci_read_psam_dir <- function(res, src, ctx) {
  files <- list.files(src$path, pattern = "[.]psam$", full.names = TRUE)
  if (!length(files)) stop("no .psam files")
  df <- ci_bind(lapply(files, function(f) utils::read.delim(f, check.names = FALSE, stringsAsFactors = FALSE,
    colClasses = "character", comment.char = "", encoding = "UTF-8")))
  iid_col <- names(df)[ci_name_key(names(df)) == "iid"][1]
  persons <- NULL
  if (!is.na(iid_col) && file.exists(src$lookup)) {
    lk <- utils::read.delim(src$lookup, check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character",
      encoding = "UTF-8")
    ci_put(res, "structure", ci_structure_row(src$id, "sample_lpid_lookup", lk))
    ci_profile_table(res, lk, src$id, "sample_lpid_lookup", ctx,
      persons = if ("PatientId" %in% names(lk)) ci_map(ci_key(lk$PatientId), "lookup", ctx) else NULL, anchored = FALSE)
    if (all(c("Sample Well", "PatientId") %in% names(lk))) {
      barcode <- lk$PatientId[match(df[[iid_col]], lk[["Sample Well"]])]
      persons <- ci_map(ci_key(barcode), "lookup", ctx)
    }
  }
  ci_put(res, "structure", ci_frame(source = src$id, path = "psam_files", class = "count", n_rows = length(files),
    n_cols = NA_real_))
  ci_put(res, "structure", ci_structure_row(src$id, "psam", df))
  ci_profile_table(res, df, src$id, "psam", ctx, persons = persons)
}

ci_read_csv <- function(res, src, ctx) {
  df <- utils::read.csv(src$path, check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
  ci_put(res, "structure", ci_structure_row(src$id, basename(src$path), df))
  ci_profile_frame(res, df, src, sub("[.]csv$", "", basename(src$path)), ctx)
}

ci_readers <- list(rds_tree = ci_read_rds_tree, master = ci_read_master, id_sets = ci_read_id_sets,
  provenance = ci_read_provenance, omics_list = ci_read_omics_list, omics_csv = ci_read_omics_csv,
  omics_rds = ci_read_omics_rds, tximport = ci_read_tximport, xlsx_dir = ci_read_xlsx_dir,
  autoantibodies = ci_read_autoantibodies, psam_dir = ci_read_psam_dir, csv = ci_read_csv)

## context, discovery and environment ---------------------------------------------------

# Small lookups every source needs: cohort IDs, anchors, sample and ID maps.
# Each is optional; a missing or unreadable file is recorded in the notes.
ci_context <- function(s) {
  meta <- file.path(s$data_root, "metadata")
  ctx <- list(settings = s)
  notes <- list()
  attempt <- function(label, expr) tryCatch(expr, error = function(e) {
    notes[[label]] <<- paste("error:", ci_sanitise(conditionMessage(e)))
    NULL
  })
  ids_path <- if (nzchar(s$project_root)) file.path(s$project_root, "subject_ids_main_cohort.rds") else ""
  sets_path <- file.path(meta, "cohort_availability_sets.rds")
  if (nzchar(ids_path) && file.exists(ids_path)) {
    ctx$cohort <- attempt("cohort_ids", unique(ci_key(readRDS(ids_path))))
    notes$cohort_ids <- "subject_ids_main_cohort.rds"
  } else if (file.exists(sets_path)) {
    # each main-cohort Venn set is intersected with the cohort; their union is the cohort
    ctx$cohort <- attempt("cohort_ids", unique(unlist(lapply(readRDS(sets_path)$main, ci_key))))
    notes$cohort_ids <- "union of cohort_availability_sets.rds$main"
  } else {
    notes$cohort_ids <- "not found: no cohort restriction or main-cohort counts"
  }
  if (!is.null(ctx$cohort)) ctx$cohort <- ctx$cohort[!is.na(ctx$cohort)]
  master_path <- file.path(meta, "combined_data_set.rds")
  if (file.exists(master_path)) attempt("anchors", {
    master <- as.data.frame(readRDS(master_path), stringsAsFactors = FALSE, optional = TRUE)
    j <- ci_subject_col(master)
    if (is.na(j)) stop("combined_data_set.rds has no subject_id")
    subject <- ci_key(master[[j]])
    anchors <- data.frame(subject = subject, stringsAsFactors = FALSE)
    for (a in names(ci_anchor_spec)) {
      col <- intersect(ci_anchor_spec[[a]], names(master))[1]
      notes[[paste0("anchor:", a)]] <- if (is.na(col)) "not found" else col
      if (!is.na(col)) anchors[[a]] <- as.character(master[[col]])
    }
    ctx$anchors <- anchors[!is.na(subject) & !duplicated(subject), , drop = FALSE]
    dates <- vapply(master, function(v) inherits(v, c("Date", "POSIXt")), logical(1)) |
      grepl("date", tolower(names(master)))
    ctx$master_dates <- master[unique(c(j, which(dates)))]
  }) else {
    notes$anchors <- "combined_data_set.rds not found: no anchored profiles"
  }
  lookup_path <- file.path(meta, "ge_lc_sample_lookup.rds")
  if (file.exists(lookup_path)) attempt("sample_lookup", {
    lk <- readRDS(lookup_path)
    ctx$sample_lookup <- stats::setNames(ci_key(lk$subject_id), ci_key(lk$barcode))
  }) else notes$sample_lookup <- "not found: omics samples cannot be mapped by barcode"
  id_path <- file.path(s$saved_root, "subject_identifiers_gelc.rds")
  if (file.exists(id_path)) attempt("identifiers", {
    ids <- readRDS(id_path)
    names(ids) <- tolower(names(ids))
    key <- ci_key(ids$project_identifier)
    subject <- ci_key(ids$subject_id)
    ok <- !is.na(key) & !is.na(subject)
    pairs <- unique(data.frame(key = key[ok], subject = subject[ok], stringsAsFactors = FALSE))
    # a project identifier pointing to two people is not used for mapping
    ambiguous <- unique(pairs$key[duplicated(pairs$key)])
    pairs <- pairs[!pairs$key %in% ambiguous, ]
    ctx$identifiers <- stats::setNames(pairs$subject, pairs$key)
    notes$identifiers_ambiguous <- as.character(length(ambiguous))
  }) else notes$identifiers <- "not found: project identifiers cannot be mapped"
  ctx$notes <- ci_frame(key = names(notes), value = unlist(notes, use.names = FALSE))
  ctx
}

# Folder listings: names (long digit runs masked), file counts and sizes.
# Folders with many entries are summarised by extension and not descended.
ci_discover <- function(roots, max_depth = 3L, max_entries = 25L) {
  rows <- list()
  walk <- function(root_name, dir, rel, depth) {
    entries <- list.files(dir, full.names = TRUE, no.. = TRUE, all.files = FALSE)
    if (!length(entries)) return(invisible(NULL))
    info <- file.info(entries)
    is_dir <- info$isdir %in% TRUE
    files <- entries[!is_dir]
    dirs <- entries[is_dir]
    if (length(files) > max_entries) {
      ext <- tolower(tools::file_ext(files))
      for (e in unique(ext)) rows[[length(rows) + 1L]] <<- ci_frame(root = root_name, folder = ci_mask_name(rel),
        entry = paste0("<", sum(ext == e), " .", e, " files>"), kind = "files", n_files = sum(ext == e),
        size_mb = round(sum(info[files[ext == e], "size"], na.rm = TRUE) / 2^20, 1),
        modified = format(max(info[files[ext == e], "mtime"], na.rm = TRUE), "%Y-%m"))
    } else for (f in files) {
      rows[[length(rows) + 1L]] <<- ci_frame(root = root_name, folder = ci_mask_name(rel),
        entry = ci_mask_name(basename(f)), kind = "file", n_files = 1, size_mb = round(info[f, "size"] / 2^20, 1),
        modified = format(info[f, "mtime"], "%Y-%m"))
    }
    if (length(dirs) > max_entries || depth >= max_depth) {
      if (length(dirs)) rows[[length(rows) + 1L]] <<- ci_frame(root = root_name, folder = ci_mask_name(rel),
        entry = paste0("<", length(dirs), " folders not listed>"), kind = "folders", n_files = NA_real_,
        size_mb = NA_real_, modified = NA_character_)
      return(invisible(NULL))
    }
    for (d in dirs) {
      rows[[length(rows) + 1L]] <<- ci_frame(root = root_name, folder = ci_mask_name(rel),
        entry = ci_mask_name(basename(d)), kind = "folder", n_files = NA_real_, size_mb = NA_real_,
        modified = format(info[d, "mtime"], "%Y-%m"))
      walk(root_name, d, file.path(rel, basename(d)), depth + 1L)
    }
  }
  for (name in names(roots$deep)) if (dir.exists(roots$deep[[name]])) walk(name, roots$deep[[name]], ".", 1L)
  for (name in names(roots$shallow)) if (dir.exists(roots$shallow[[name]])) walk(name, roots$shallow[[name]], ".", max_depth)
  ci_bind(rows)
}

ci_environment <- function() {
  pk <- utils::installed.packages(fields = NULL)[, c("Package", "Version"), drop = FALSE]
  ci_frame(key = c("R", "platform", "os", "locale_ctype", paste0("package:", pk[, "Package"])),
    value = c(R.version.string, R.version$platform, utils::sessionInfo()$running, Sys.getlocale("LC_CTYPE"),
      pk[, "Version"]))
}

## run -------------------------------------------------------------------------------

#' Run the inventory. Each source is checkpointed in `internal`; with
#' resume = TRUE a source is reused when its input files have not changed.
ci_run <- function(saved_root = Sys.getenv("REACT_SAVED_ROOT", "D:/saved_objects"),
                   data_root = Sys.getenv("REACT_DATA_ROOT", "//se-sandisk-vm01/react_data03$"),
                   project_root = Sys.getenv("REACT_GELC_PROJECT", ""),
                   internal = "clinical-inventory-INTERNAL", sources = "all", resume = TRUE,
                   discover = TRUE, progress = TRUE) {
  s <- list(saved_root = saved_root, data_root = data_root, project_root = project_root)
  dir.create(internal, recursive = TRUE, showWarnings = FALSE)
  writeLines(c("ENCLAVE INTERNAL: unsuppressed aggregates. Never transfer this folder.",
    "Run 02_prepare_export.R and request output checking for the candidate folder only."),
    file.path(internal, "DO_NOT_TRANSFER.txt"))
  registry <- ci_sources(s)
  if (!identical(sources, "all")) registry <- Filter(function(x) x$id %in% sources, registry)
  ctx <- ci_context(s)
  log <- list()
  for (src in registry) {
    out_file <- file.path(internal, paste0("source_", src$id, ".rds"))
    paths <- unname(src$path)
    info <- file.info(paths)
    identity <- paste(paths, info$size, format(info$mtime), collapse = ";")
    if (resume && file.exists(out_file) && identical(readRDS(out_file)$identity, identity)) {
      if (progress) message("Reusing ", src$id)
      next
    }
    if (progress) message("Profiling ", src$id, " ...")
    started <- Sys.time()
    res <- ci_new_result()
    found <- any(file.exists(paths))
    status <- if (!found) "missing" else tryCatch({
      ci_readers[[src$reader]](res, src, ctx)
      "ok"
    }, error = function(e) structure("error", message = conditionMessage(e)))
    message_text <- if (identical(as.character(status), "error")) ci_sanitise(attr(status, "message")) else ""
    record <- ci_frame(source = src$id, group = src$group, reader = src$reader, status = as.character(status),
      message = message_text, size_mb = round(sum(info$size, na.rm = TRUE) / 2^20, 1),
      modified = if (found) format(max(info$mtime, na.rm = TRUE), "%Y-%m-%d") else NA_character_,
      seconds = round(as.numeric(difftime(Sys.time(), started, units = "secs"))))
    saveRDS(list(identity = identity, record = record, frames = ci_finish(res)), out_file)
    if (progress) message("  ", record$status, if (nzchar(message_text)) paste0(": ", message_text) else "",
      " (", record$seconds, " s)")
    rm(res)
    invisible(gc())
  }
  extras <- list(context = ctx$notes, environment = ci_environment(),
    discovery = if (discover) ci_discover(ci_discovery_roots(s)) else NULL)
  files <- file.path(internal, paste0("source_", vapply(registry, `[[`, "", "id"), ".rds"))
  parts <- lapply(files[file.exists(files)], readRDS)
  kinds <- unique(unlist(lapply(parts, function(p) names(p$frames))))
  frames <- stats::setNames(lapply(kinds, function(k) ci_bind(lapply(parts, function(p) p$frames[[k]]))), kinds)
  frames$sources <- ci_bind(lapply(parts, `[[`, "record"))
  out <- list(version = ci_version, created = format(Sys.time(), "%Y-%m-%d %H:%M"), frames = c(frames, extras))
  saveRDS(out, file.path(internal, "clinical-inventory-INTERNAL.rds"))
  if (progress) message("Inventory saved in ", internal, ". Keep it in the enclave; next run 02_prepare_export.R.")
  invisible(out)
}
