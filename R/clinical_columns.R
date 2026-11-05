# Synthetic Clinical Study columns ------------------------------------------------

# Draw categorical levels for rows with anchor labels `strata`, at
# probabilities `u`. Level probabilities multiply the overall ones by each
# anchor's within-level ratio (a log-linear model with no interactions).
# Levels with no released overall count are never drawn. Within an anchor
# level, a blank (suppressed) cell isn't evidence of a rare level: the kit's
# complementary suppression can blank a common one. It keeps the level's
# overall share, and the released cells share the rest in their proportions;
# nothing is reconstructed.
.clinical_draw_levels <- function(rows, strata, u) {
  overall <- rows[rows$stratum_var == "all", , drop = FALSE]
  overall <- overall[!is.na(overall$level), , drop = FALSE]
  overall <- overall[order(overall$level_order), , drop = FALSE]
  counts <- overall$n_rows
  counts[is.na(counts)] <- 0
  keep <- counts > 0
  if (!any(keep)) return(NULL)
  levels <- overall$level[keep]
  if (length(u) == 0L) return(levels[0L])
  base <- counts[keep] / sum(counts[keep])
  by <- intersect(setdiff(unique(rows$stratum_var), "all"), names(strata))
  if (length(by) == 0L) {
    return(levels[pmin(findInterval(u, cumsum(base)) + 1L, length(levels))])
  }
  logp <- matrix(log(base), length(u), length(levels), byrow = TRUE)
  for (stratum_var in by) {
    part <- rows[rows$stratum_var == stratum_var & rows$level %in% levels, , drop = FALSE]
    if (nrow(part) == 0L) next
    part$n_rows[is.na(part$n_rows)] <- 0
    within <- tapply(part$n_rows, list(part$stratum, factor(part$level, levels = levels)), sum)
    within[is.na(within)] <- 0
    released <- within > 0
    blank_share <- as.vector((!released) %*% base)
    base_matrix <- matrix(base, nrow(within), length(base), byrow = TRUE)
    within_share <- within / pmax(rowSums(within), 1)
    probability <- ifelse(released, within_share * (1 - blank_share), base_matrix)
    probability[rowSums(released) == 0 | blank_share >= 1, ] <- base_matrix[rowSums(released) == 0 | blank_share >= 1, ]
    shift <- log(probability) - log(base_matrix)
    dimnames(shift) <- dimnames(within)
    label <- strata[[stratum_var]]
    matched <- label %in% rownames(shift)
    logp[matched, ] <- logp[matched, ] + shift[label[matched], , drop = FALSE]
  }
  p <- exp(logp - apply(logp, 1L, max))
  cumulative <- p / rowSums(p)
  for (j in seq_len(ncol(cumulative))[-1L]) cumulative[, j] <- cumulative[, j - 1L] + cumulative[, j]
  index <- pmin(rowSums(cumulative < u) + 1L, length(levels))
  levels[index]
}

# Quantile function through the released quantiles, with linear tails a
# quarter of the nearest step beyond the outermost ones. NULL when fewer than
# two quantiles were released.
.clinical_quantile_fun <- function(values, probs = .clinical_quantile_probs) {
  keep <- !is.na(values)
  values <- values[keep]
  probs <- probs[keep]
  if (length(values) < 2L) return(NULL)
  values <- cummax(values)
  k <- length(values)
  low <- values[1L] - (values[2L] - values[1L]) / 4
  high <- values[k] + (values[k] - values[k - 1L]) / 4
  if (values[1L] >= 0) low <- max(low, 0)
  x <- c(0, probs, 1)
  y <- c(low, values, high)
  function(u) stats::approx(x, y, xout = u, ties = "ordered")$y
}

# Numeric values at probabilities `u`: the overall quantile function plus
# the anchors' shifts from it at the same probability, mapped back to the
# overall quantiles by rank, then rounded and given missing codes as released
.clinical_draw_numeric <- function(rows, strata, u) {
  overall <- rows[rows$stratum_var == "all", , drop = FALSE]
  if (nrow(overall) == 0L) return(NULL)
  overall <- overall[1L, , drop = FALSE]
  q_all <- unlist(overall[.clinical_quantile_columns], use.names = FALSE)
  quantile <- .clinical_quantile_fun(q_all)
  if (is.null(quantile)) return(NULL)
  x <- quantile(u)
  shift <- numeric(length(u))
  shifted <- FALSE
  for (stratum_var in intersect(setdiff(unique(rows$stratum_var), "all"), names(strata))) {
    part <- rows[rows$stratum_var == stratum_var, , drop = FALSE]
    label <- strata[[stratum_var]]
    for (i in seq_len(nrow(part))) {
      matched <- which(label == part$stratum[[i]])
      if (length(matched) == 0L) next
      q <- unlist(part[i, .clinical_quantile_columns], use.names = FALSE)
      keep <- !is.na(q)
      if (sum(keep) < 2L) next
      probs <- .clinical_quantile_probs[keep]
      shift[matched] <- shift[matched] + stats::approx(probs, q[keep] - quantile(probs), xout = u[matched], rule = 2)$y
      shifted <- TRUE
    }
  }
  if (shifted) {
    # the anchors overlap (route, study and age group, for example), so their
    # summed shifts overshoot; mapping the values back to the overall
    # quantiles by rank keeps the overall distribution while the anchors set
    # who is high or low (checked against the profile's anchored quantiles)
    x <- quantile((rank(x + shift, ties.method = "first") - 0.5) / length(x))
  }
  x <- pmin(pmax(x, quantile(0)), quantile(1))

  decimals <- overall$decimals
  if (is.na(decimals)) decimals <- 2
  x <- round(x, decimals)
  share_integer <- overall$share_integer
  if (!is.na(share_integer) && decimals > 0 && share_integer > 0) {
    whole <- stats::runif(length(x)) < share_integer
    x[whole] <- round(x[whole])
  }

  code_columns <- paste0("n_code_", sub("-", "m", .clinical_missing_codes))
  present <- code_columns %in% names(overall)
  codes <- unlist(overall[code_columns[present]], use.names = FALSE)
  codes[is.na(codes)] <- 0
  if (sum(codes) > 0) {
    n <- if (is.na(overall$n)) 0 else overall$n
    p <- c(n, codes) / (n + sum(codes))
    pick <- sample.int(length(p), length(x), replace = TRUE, prob = p)
    coded <- pick > 1L
    x[coded] <- .clinical_missing_codes[present][pick[coded] - 1L]
  }
  x
}

# Dates from released month counts (or month quantiles), with a uniform day
.clinical_draw_dates <- function(months, summary, n) {
  overall <- months[months$stratum_var == "all" & !is.na(months$level), , drop = FALSE]
  counts <- overall$n_rows
  counts[is.na(counts)] <- 0
  first <- NULL
  if (sum(counts) > 0) {
    keep <- counts > 0
    starts <- as.Date(paste0(overall$level[keep], "-01"), optional = TRUE)
    ok <- !is.na(starts)
    if (any(ok)) first <- starts[ok][sample.int(sum(ok), n, replace = TRUE, prob = counts[keep][ok])]
  }
  if (is.null(first) && nrow(summary) > 0L) {
    q <- unlist(summary[1L, .clinical_quantile_columns], use.names = FALSE)
    q <- as.numeric(as.Date(ifelse(is.na(q), NA_character_, paste0(q, "-01")), optional = TRUE))
    quantile <- .clinical_quantile_fun(q)
    if (!is.null(quantile)) {
      first <- as.Date(quantile(stats::runif(n)), origin = "1970-01-01")
      first <- as.Date(format(first, "%Y-%m-01"))
    }
  }
  if (is.null(first)) return(NULL)
  next_month <- as.Date(format(first + 31, "%Y-%m-01"))
  days <- as.numeric(next_month - first)
  value <- first + floor(stats::runif(n) * days)
  # placeholder months keep the placeholder date, not a random day
  early <- .clinical_placeholder_month(format(first, "%Y-%m"))
  value[early] <- .clinical_placeholder_date(format(first[early], "%Y-%m"))
  if (!any(early)) value <- .clinical_counted_placeholders(value, summary)
  value
}

# Character values from shapes (letters to "A", digits to "9")
.clinical_draw_shapes <- function(rows, n, unique = FALSE) {
  overall <- rows[rows$stratum_var == "all" & !is.na(rows$level), , drop = FALSE]
  counts <- overall$n_rows
  counts[is.na(counts)] <- 0
  if (sum(counts) == 0 || n == 0L) return(NULL)
  shapes <- overall$level[counts > 0]
  shape <- shapes[sample.int(length(shapes), n, replace = TRUE, prob = counts[counts > 0])]
  fill <- function(shape) {
    out <- character(length(shape))
    for (s in unique(shape)) {
      chars <- strsplit(s, "", fixed = TRUE)[[1L]]
      at <- which(shape == s)
      parts <- matrix(rep(chars, each = length(at)), length(at))
      for (j in seq_along(chars)) {
        if (chars[[j]] == "A") parts[, j] <- sample(LETTERS, length(at), replace = TRUE)
        if (chars[[j]] == "9") parts[, j] <- sample(0:9, length(at), replace = TRUE)
      }
      out[at] <- apply(parts, 1L, paste, collapse = "")
    }
    out
  }
  value <- fill(shape)
  if (unique) {
    for (attempt in seq_len(20L)) {
      again <- duplicated(value)
      if (!any(again)) break
      value[again] <- fill(shape[again])
    }
  }
  value
}

.clinical_as_class <- function(x, class) {
  switch(class,
    numeric = , double = as.numeric(x),
    integer = as.integer(round(as.numeric(x))),
    logical = as.logical(x),
    Date = as.Date(x),
    POSIXct = as.POSIXct(x, tz = "UTC"),
    factor = factor(x),
    as.character(x)
  )
}

.clinical_na <- function(class, n) .clinical_as_class(rep(NA, n), class)

# Gaussian copula for a table's columns with rank correlations: Spearman's rho
# becomes a Pearson correlation and the matrix is made positive definite. A
# matrix of probabilities, one column per correlated column, or NULL.
.clinical_copula <- function(correlations, columns, n) {
  correlations <- correlations[correlations$var1 %in% columns & correlations$var2 %in% columns &
                                 !is.na(correlations$rho) & correlations$var1 != correlations$var2, ,
                               drop = FALSE]
  vars <- unique(c(correlations$var1, correlations$var2))
  if (length(vars) < 2L || n == 0L) return(NULL)
  r <- diag(length(vars))
  dimnames(r) <- list(vars, vars)
  pearson <- 2 * sin(pi * correlations$rho / 6)
  r[cbind(correlations$var1, correlations$var2)] <- pearson
  r[cbind(correlations$var2, correlations$var1)] <- pearson
  e <- eigen(r, symmetric = TRUE)
  root <- e$vectors %*% diag(sqrt(pmax(e$values, 1e-6)), length(vars))
  root <- root / sqrt(rowSums(root^2))
  z <- matrix(stats::rnorm(n * length(vars)), n) %*% t(root)
  colnames(z) <- vars
  stats::pnorm(z)
}

# One generated table. `rows` gives the person (index into `people`) of each
# row, or NULL for a table without people, which keeps its row count (`n`
# overrides it). Distributions come from `table$profile_table` in `source`.
# `dates` holds person-level dates keyed "table:column" (from the timing
# intervals). `baseline` links a follow-up table to its baseline: `u` holds
# the percentile of each person's baseline value per column and `rho` the
# columns' rank correlations between visits. Percentiles of the columns named
# in `keep` are returned in the "latent" attribute, for a follow-up table.
# `only` limits the table to some of its columns.
.clinical_table <- function(profile, table, people, rows, ids = list(), dates = list(), baseline = NULL,
                            keep = character(), source = .clinical_release_source, n = NULL, only = NULL) {
  columns <- .clinical_rows(profile$columns, table$table, source = source)
  columns <- columns[order(columns$position), , drop = FALSE]
  if (!is.null(only)) columns <- columns[columns$column %in% only, , drop = FALSE]
  from <- if (is.null(table$profile_table) || is.na(table$profile_table)) table$table else table$profile_table
  if (from != table$table) {
    # a borrowed profile also gives completeness where the table's own is suppressed
    own <- .clinical_rows(profile$columns, from, source = source)
    unknown <- is.na(columns$n_nonmissing) & is.na(columns$n_missing)
    at <- match(columns$column[unknown], own$column)
    columns$n_nonmissing[unknown] <- own$n_nonmissing[at]
    columns$n_missing[unknown] <- own$n_missing[at]
  }
  if (is.null(n)) n <- if (is.null(rows)) as.integer(table$n_rows) else length(rows)
  if (is.na(n)) n <- 0L
  anchor_names <- intersect(c("sex", "age_group", "lc_group", "main_cohort", "route", "study", "ethnicity"),
                            names(people))
  strata <- if (is.null(rows)) list() else as.list(people[rows, anchor_names, drop = FALSE])
  copula <- .clinical_copula(
    profile$correlations[profile$correlations$source == source & profile$correlations$scope == from, ,
                         drop = FALSE],
    columns$column, n
  )
  numeric_rows <- .clinical_rows(profile$numeric, from, source = source)
  categorical_rows <- .clinical_rows(profile$categorical, from, source = source)
  month_rows <- .clinical_rows(profile$date_months, from, source = source)
  summary_rows <- .clinical_rows(profile$date_summary, from, source = source)
  shape_rows <- .clinical_rows(profile$id_shapes, from, source = source)
  anchor_columns <- profile$anchors
  latent <- matrix(rep(NA_real_, n * length(keep)), n, length(keep), dimnames = list(NULL, keep))

  out <- vector("list", nrow(columns))
  names(out) <- columns$column
  for (i in seq_len(nrow(columns))) {
    column <- columns[i, , drop = FALSE]
    name <- column$column
    class <- column$class
    u <- if (!is.null(copula) && name %in% colnames(copula)) copula[, name] else stats::runif(n)
    if (!is.null(baseline) && !is.null(rows) && name %in% names(baseline$rho) && name %in% colnames(baseline$u)) {
      u0 <- baseline$u[rows, name]
      linked <- !is.na(u0)
      r <- 2 * sin(pi * baseline$rho[[name]] / 6)
      z <- r * stats::qnorm(u0[linked]) + sqrt(1 - r^2) * stats::qnorm(u[linked])
      u[linked] <- stats::pnorm(z)
    }
    value <- NULL
    if (!is.null(rows) && .clinical_is_person_key(name)) {
      key <- people$subject_id[rows]
      out[[i]] <- .clinical_as_class(if (class == "character") sprintf("%.0f", key) else key, class)
      next
    }
    date_key <- paste0(table$table, ":", name)
    anchor <- if (!is.null(rows) && name %in% anchor_columns) {
      .clinical_anchor_value(people[[names(anchor_columns)[match(name, anchor_columns)]]][rows], class)
    }
    if (!is.null(anchor)) {
      value <- anchor
    } else if (!is.null(rows) && !is.null(dates[[date_key]])) {
      value <- dates[[date_key]][rows]
    } else if (column$role == "identifier") {
      value <- if (!is.null(rows) && !is.null(ids[[name]])) ids[[name]][rows] else
        .clinical_identifiers(shape_rows[shape_rows$column == name, , drop = FALSE], name, n, class)
    } else if (column$role %in% c("categorical", "coded_numeric") &&
               any(categorical_rows$column == name)) {
      value <- .clinical_draw_levels(categorical_rows[categorical_rows$column == name, , drop = FALSE],
                                     strata, u)
    } else if (column$role %in% c("continuous", "coded_numeric")) {
      value <- .clinical_draw_numeric(numeric_rows[numeric_rows$column == name, , drop = FALSE], strata, u)
    } else if (column$role == "date") {
      value <- .clinical_draw_dates(month_rows[month_rows$column == name, , drop = FALSE],
                                    summary_rows[summary_rows$column == name, , drop = FALSE], n)
    } else if (column$role == "high_cardinality") {
      value <- .clinical_draw_shapes(shape_rows[shape_rows$column == name, , drop = FALSE], n)
      if (is.null(value) && n > 0L) {
        k <- max(1, min(column$n_distinct, 999, na.rm = TRUE))
        value <- sprintf("%s_%03d", name, sample.int(k, n, replace = TRUE))
      }
    } else if (column$role == "free_text") {
      value <- rep("Synthetic free text", n)
    }
    if (name %in% keep) {
      # a follow-up table links to the baseline value's rank, which carries the
      # anchors' effect as well as the column's own probability
      levels <- categorical_rows$level[categorical_rows$column == name & categorical_rows$stratum_var == "all"]
      levels <- levels[order(categorical_rows$level_order[categorical_rows$column == name &
                                                            categorical_rows$stratum_var == "all"])]
      latent[, name] <- if (is.null(value)) u else .clinical_percentile(value, levels)
    }
    if (is.null(value)) {
      out[[i]] <- .clinical_na(class, n)
      next
    }
    if (inherits(value, "Date") && class == "POSIXct") {
      value <- as.POSIXct(value) + floor(stats::runif(n) * 86400)
      attr(value, "tzone") <- "UTC"
    }
    value <- .clinical_as_class(value, class)
    n_ok <- if (is.na(column$n_nonmissing)) 0 else column$n_nonmissing
    n_missing <- if (is.na(column$n_missing)) 0 else column$n_missing
    if (n_ok + n_missing > 0 && n_missing > 0) value[stats::runif(n) < n_missing / (n_ok + n_missing)] <- NA
    out[[i]] <- value
  }
  data <- as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
  if (length(out) == 0L) data <- data.frame(row.names = seq_len(n))
  names(data) <- columns$column
  classes <- strsplit(if (is.na(table$class)) "data.frame" else table$class, "/", fixed = TRUE)[[1L]]
  if ("data.table" %in% classes) classes <- "data.frame"
  class(data) <- classes
  if (length(keep) > 0L) attr(data, "latent") <- list(rows = rows, u = latent)
  data
}

# Mid-rank percentiles of values (categorical values ranked by their level
# order), for linking a follow-up value to its baseline value
.clinical_percentile <- function(value, levels = character()) {
  x <- if (is.character(value) && length(levels) > 0L) match(value, levels) else suppressWarnings(as.numeric(value))
  ok <- !is.na(x)
  out <- rep(NA_real_, length(x))
  if (any(ok)) out[ok] <- (rank(x[ok], ties.method = "average") - 0.5) / sum(ok)
  out
}

# The person key of a table: subject_id however it is written ("Subject ID")
.clinical_is_person_key <- function(name) tolower(gsub("[^A-Za-z]", "", name)) == "subjectid"

# A leaf of the wrangled list that isn't a table, such as a date limit. These
# aren't profiled: a date whose name says min or max takes the first or last
# day of the released months of related tables (those whose path starts with
# the leaf's first word, such as "infection"), anything else is missing.
.clinical_value <- function(profile, table) {
  class <- strsplit(table$class, "/", fixed = TRUE)[[1L]][[1L]]
  n <- if (is.na(table$n_rows)) 1L else as.integer(table$n_rows)
  value <- .clinical_na(class, n)
  months <- profile$date_months[profile$date_months$source == .clinical_release_source &
                                  !is.na(profile$date_months$n_rows), , drop = FALSE]
  related <- startsWith(months$table, sub("[_/].*", "", table$table))
  if (any(related)) months <- months[related, , drop = FALSE]
  months <- months$level
  months <- sort(unique(months[!is.na(months) & !.clinical_placeholder_month(months)]))
  name <- tolower(basename(table$table))
  if (class == "Date" && length(months) > 0L) {
    if (grepl("min", name, fixed = TRUE)) value <- rep(as.Date(paste0(months[[1L]], "-01")), n)
    if (grepl("max", name, fixed = TRUE)) {
      last <- as.Date(paste0(months[[length(months)]], "-01"))
      value <- rep(as.Date(format(last + 31, "%Y-%m-01")) - 1, n)
    }
  }
  value
}

# A person's anchor label for an anchor column, or NULL when the column codes
# the anchor in a way the label can't fill (such as numbers)
.clinical_anchor_value <- function(label, class) {
  label[label == "(missing)"] <- NA
  filled <- suppressWarnings(.clinical_as_class(label, class))
  if (any(!is.na(label)) && all(is.na(filled))) return(NULL)
  label
}

# Identifier values for rows without people: unique shapes, or numbered
# placeholders when no shape was released
.clinical_identifiers <- function(shape_rows, name, n, class) {
  value <- .clinical_draw_shapes(shape_rows, n, unique = TRUE)
  if (!is.null(value)) return(value)
  if (class %in% c("numeric", "integer", "double")) return(9e8 + seq_len(n))
  sprintf("SYN-%s-%07d", toupper(name), seq_len(n))
}

# Each person's value of every identifier column (other than subject_id) that
# the selected tables need, the same in every table. Each column has its own
# random stream, so values don't depend on which tables were requested.
.clinical_person_ids <- function(profile, tables, people, seed, branch = "") {
  columns <- profile$columns[profile$columns$source == .clinical_release_source &
                               profile$columns$table %in% tables &
                               profile$columns$role == "identifier" &
                               !.clinical_is_person_key(profile$columns$column), , drop = FALSE]
  shapes <- profile$id_shapes[profile$id_shapes$source == .clinical_release_source &
                                profile$id_shapes$stratum_var == "all", , drop = FALSE]
  ids <- list()
  for (name in unique(columns$column)) {
    class <- columns$class[columns$column == name][1L]
    rows <- shapes[shapes$column == name, , drop = FALSE]
    if (nrow(rows) > 0L) {
      totals <- tapply(rows$n_rows, rows$table, sum, na.rm = TRUE)
      rows <- rows[rows$table == names(totals)[which.max(totals)], , drop = FALSE]
    }
    ids[[name]] <- .with_stream_seed(seed, paste0("clinical-id", branch), name,
                                     .clinical_identifiers(rows, name, nrow(people), class))
  }
  ids
}
