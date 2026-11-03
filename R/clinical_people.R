# Synthetic Clinical Study people, anchors and table membership ------------------

# The wrangled release is the "wrangled" source in the inventory kit
.clinical_release_source <- "wrangled"

# Clinical subject_id from a reactextract SUBJECT_ID. Synthetic IDs
# (SYN-SUBJECT-<round>-<number>) become 100000000 + 1000000 * round index +
# number: a nine-digit double like the real key, from which the reactextract
# person can be recovered. Whole-number IDs are kept as they are.
.clinical_subject_id <- function(ids) {
  ids <- as.character(ids)
  out <- rep(NA_real_, length(ids))
  digits <- !is.na(ids) & grepl("^[0-9]+$", ids)
  out[digits] <- as.numeric(ids[digits])
  pattern <- "^SYN-SUBJECT-(.+)-([0-9]{7})$"
  synthetic <- !is.na(ids) & grepl(pattern, ids)
  if (any(synthetic)) {
    rounds <- react_dictionary()$rounds$round_id
    index <- match(sub(pattern, "\\1", ids[synthetic]), toupper(gsub("[.]", "-", rounds)))
    number <- as.numeric(sub(pattern, "\\2", ids[synthetic]))
    if (anyNA(index) || any(number >= 1e6)) {
      stop("A synthetic SUBJECT_ID has an unknown round or a person number of 1,000,000 or more.",
           call. = FALSE)
    }
    out[synthetic] <- 1e8 + 1e6 * index + number
  }
  if (anyNA(out)) {
    stop("`people` SUBJECT_IDs must be whole numbers or reactextract synthetic IDs.", call. = FALSE)
  }
  out
}

# Adults with a recorded sex and age, from the source's synthetic people or
# from `people` (an extraction result or a data frame with SUBJECT_ID, sex and
# age, as for react_hes()).
.clinical_pool <- function(source, people) {
  if (is.null(people)) {
    if (!inherits(source, "react_synthetic_source")) {
      stop("Synthetic Clinical Study data need a `react_synthetic()` source or `people`.", call. = FALSE)
    }
    people <- react_extract(source, families = "people-households/demographics", progress = FALSE)
  }
  pool <- .hes_people(people)
  pool <- pool[!is.na(pool$sex) & !is.na(pool$age) & pool$age >= 18, , drop = FALSE]
  pool$subject_id <- .clinical_subject_id(pool$SUBJECT_ID)
  pool[!duplicated(pool$subject_id), , drop = FALSE]
}

# Wrangled tables in the order of the nested list, with their kit rows
.clinical_tables <- function(profile) {
  structure <- profile$structure[profile$structure$source == .clinical_release_source, , drop = FALSE]
  tables <- profile$tables[profile$tables$source == .clinical_release_source, , drop = FALSE]
  tables <- tables[match(structure$path, tables$table), , drop = FALSE]
  tables$table <- structure$path
  tables$class <- structure$class
  tables$n_rows[is.na(tables$n_rows)] <- structure$n_rows[is.na(tables$n_rows)]
  tables$has_persons[is.na(tables$has_persons)] <- FALSE
  if (!"person_level" %in% names(tables)) tables$person_level <- tables$has_persons
  tables$person_level[is.na(tables$person_level)] <- FALSE
  tables
}

.clinical_rows <- function(data, table, column = NULL) {
  keep <- data$source == .clinical_release_source & data$table == table
  if (!is.null(column)) keep <- keep & data$column == column
  data[keep, , drop = FALSE]
}

# Labels of an anchor's strata, as written in the profile: the levels of its
# column and the strata other columns are broken down by
.clinical_stratum_labels <- function(profile, stratum_var) {
  column <- if (stratum_var %in% names(profile$anchors)) profile$anchors[[stratum_var]] else NA_character_
  categorical <- profile$categorical
  labels <- c(categorical$level[categorical$source == .clinical_release_source &
                                  categorical$column %in% column & categorical$stratum_var == "all"],
              categorical$stratum[categorical$stratum_var == stratum_var],
              profile$numeric$stratum[profile$numeric$stratum_var == stratum_var])
  setdiff(unique(labels[!is.na(labels)]), "(missing)")
}

# Age group label for each age, from labels such as "18-24" and "75+"
.clinical_age_group <- function(age, labels) {
  out <- rep("(missing)", length(age))
  for (label in labels) {
    bounds <- regmatches(label, regexpr("^[0-9]+", label))
    if (length(bounds) == 0L) next
    lower <- as.numeric(bounds)
    upper <- if (grepl("^[0-9]+ *- *[0-9]+$", label)) {
      as.numeric(sub("^[0-9]+ *- *", "", label))
    } else if (grepl("^[0-9]+ *\\+$|^[0-9]+ (and|or) (over|older)$", label)) {
      Inf
    } else {
      next
    }
    out[!is.na(age) & age >= lower & age <= upper] <- label
  }
  out
}

# Sex label for reactextract's sex codes ("1" male, "2" female)
.clinical_sex_label <- function(code, labels) {
  male <- labels[grepl("^m", labels, ignore.case = TRUE)][1]
  female <- labels[grepl("^f", labels, ignore.case = TRUE)][1]
  out <- rep("(missing)", length(code))
  if (!is.na(male)) out[code %in% "1"] <- male
  if (!is.na(female)) out[code %in% "2"] <- female
  out
}

# The wrangled table whose categorical rows best describe a column (most
# people counted overall), used for the person-level anchors
.clinical_anchor_table <- function(profile, column) {
  rows <- profile$categorical[profile$categorical$source == .clinical_release_source &
                                profile$categorical$column == column &
                                profile$categorical$stratum_var == "all", , drop = FALSE]
  if (nrow(rows) == 0L) return(NA_character_)
  totals <- tapply(rows$n_rows, rows$table, sum, na.rm = TRUE)
  names(totals)[which.max(totals)]
}

# Number of rows in each stratum of a table, from its most complete column
.clinical_stratum_sizes <- function(profile, table, stratum_var) {
  for (data in list(profile$categorical, profile$numeric)) {
    rows <- .clinical_rows(data, table)
    rows <- rows[rows$stratum_var == stratum_var, , drop = FALSE]
    if (nrow(rows) == 0L) next
    size <- if ("n_rows" %in% names(rows)) rows$n_rows else rows$n
    size[is.na(size)] <- 0
    totals <- tapply(size, rows$column, sum)
    best <- rows[rows$column == names(totals)[which.max(totals)], , drop = FALSE]
    best_size <- if ("n_rows" %in% names(best)) best$n_rows else best$n
    best_size[is.na(best_size)] <- 0
    out <- tapply(best_size, best$stratum, sum)
    return(out[names(out) != "(missing)"])
  }
  NULL
}

# Draw the cohort and give each person all seven anchors
.clinical_people <- function(source, people, profile, n) {
  tables <- .clinical_tables(profile)
  person_tables <- tables[tables$has_persons & tables$person_level & !is.na(tables$n_persons), , drop = FALSE]
  if (nrow(person_tables) == 0L) stop("The clinical profile has no person-level wrangled table.", call. = FALSE)
  registration <- person_tables[which.max(person_tables$n_persons), , drop = FALSE]
  n_profile <- registration$n_persons
  if (is.null(n)) n <- n_profile
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L) stop("`n` must be one positive whole number.", call. = FALSE)
  scale <- n / n_profile

  pool <- .clinical_pool(source, people)
  if (nrow(pool) < n) {
    stop("The synthetic people include ", nrow(pool), " adults, fewer than the ", n,
         " registrants requested. Use a larger `n_per_round` in react_synthetic() or a smaller `n`.",
         call. = FALSE)
  }

  anchors <- profile$anchors
  sex_labels <- .clinical_stratum_labels(profile, "sex")
  age_labels <- .clinical_stratum_labels(profile, "age_group")
  pool$sex_label <- .clinical_sex_label(pool$sex, sex_labels)
  pool$age_label <- .clinical_age_group(pool$age, age_labels)

  # weight the pool so that age group and sex follow the registration counts
  weight <- rep(1, nrow(pool))
  age_column <- if ("age_group" %in% names(anchors)) anchors[["age_group"]] else NA_character_
  age_table <- if (is.na(age_column)) NA_character_ else .clinical_anchor_table(profile, age_column)
  if (!is.na(age_table)) {
    rows <- .clinical_rows(profile$categorical, age_table, age_column)
    joint <- rows[rows$stratum_var == "sex", , drop = FALSE]
    if (nrow(joint) == 0L) joint <- rows[rows$stratum_var == "all", , drop = FALSE]
    joint$n_rows[is.na(joint$n_rows)] <- 0
    cell <- if (all(joint$stratum_var == "sex")) paste(joint$level, joint$stratum) else joint$level
    target <- tapply(joint$n_rows, cell, sum)
    pool_cell <- if (all(joint$stratum_var == "sex")) paste(pool$age_label, pool$sex_label) else pool$age_label
    have <- table(pool_cell)
    ratio <- target[pool_cell] / as.numeric(have[pool_cell])
    ratio[is.na(ratio)] <- 0
    if (sum(ratio > 0) >= n) weight <- ratio
  }
  chosen <- sample.int(nrow(pool), n, prob = weight)
  out <- data.frame(
    SUBJECT_ID = pool$SUBJECT_ID[chosen], subject_id = pool$subject_id[chosen],
    sex_code = pool$sex[chosen], age = pool$age[chosen],
    sex = pool$sex_label[chosen], age_group = pool$age_label[chosen],
    stringsAsFactors = FALSE
  )
  out <- out[order(out$subject_id), , drop = FALSE]
  rownames(out) <- NULL

  # main cohort, weighted by age group where the profile has those counts
  main_labels <- .clinical_stratum_labels(profile, "main_cohort")
  in_label <- main_labels[grepl("^(yes|true|1)$", main_labels, ignore.case = TRUE)][1]
  out_label <- setdiff(main_labels, in_label)[1]
  if (is.na(in_label)) in_label <- "Yes"
  if (is.na(out_label)) out_label <- "No"
  n_main <- min(n, round(registration$n_persons_main * scale))
  main_weight <- rep(1, n)
  if (!is.na(age_table)) {
    rows <- .clinical_rows(profile$categorical, age_table, age_column)
    rows <- rows[rows$stratum_var == "main_cohort", , drop = FALSE]
    rows$n_rows[is.na(rows$n_rows)] <- 0
    yes <- tapply(rows$n_rows[rows$stratum == in_label], rows$level[rows$stratum == in_label], sum)
    all <- tapply(rows$n_rows, rows$level, sum)
    share <- yes[out$age_group] / all[out$age_group]
    share[is.na(share)] <- mean(share, na.rm = TRUE)
    if (any(share > 0, na.rm = TRUE) && sum(share > 0, na.rm = TRUE) >= n_main) main_weight <- share
  }
  main <- rep(FALSE, n)
  main[sample.int(n, n_main, prob = main_weight)] <- TRUE
  out$main_cohort <- ifelse(main, in_label, out_label)
  out$main <- main

  # other anchors from their counts by age group, sex and main cohort
  strata <- out[c("sex", "age_group", "main_cohort")]
  for (anchor in setdiff(names(anchors), c("sex", "age_group"))) {
    column <- anchors[[anchor]]
    table <- .clinical_anchor_table(profile, column)
    value <- rep("(missing)", n)
    if (!is.na(table)) {
      drawn <- .clinical_draw_levels(.clinical_rows(profile$categorical, table, column), strata, stats::runif(n))
      if (!is.null(drawn)) value <- drawn
    }
    out[[anchor]] <- value
  }

  # one ranking per visit: tables of a visit take the top of it, so they nest
  out$rank_main <- NA_real_
  out$rank_main[main] <- sample.int(n_main)
  out$rank_other <- NA_real_
  out$rank_other[!main] <- sample.int(n - n_main)
  out$rank_t1 <- NA_real_
  if (n_main > 0L) {
    lc_group <- if (is.null(out$lc_group)) rep("(missing)", n_main) else out$lc_group[main]
    t1_weight <- .clinical_followup_weight(profile, tables, lc_group)
    out$rank_t1[main] <- order(sample.int(n_main, n_main, prob = t1_weight))
  }
  attr(out, "scale") <- scale
  out
}

# Follow-up weights by long COVID group, so the T1 mix matches the T1 tables
.clinical_followup_weight <- function(profile, tables, lc_group) {
  weight <- rep(1, length(lc_group))
  person <- tables[tables$has_persons & tables$person_level & !is.na(tables$n_persons), , drop = FALSE]
  t0 <- person[person$visit %in% "t0", , drop = FALSE]
  t1 <- person[person$visit %in% "t1", , drop = FALSE]
  if (nrow(t0) == 0L || nrow(t1) == 0L) return(weight)
  size0 <- .clinical_stratum_sizes(profile, t0$table[which.max(t0$n_persons)], "lc_group")
  size1 <- .clinical_stratum_sizes(profile, t1$table[which.max(t1$n_persons)], "lc_group")
  if (is.null(size0) || is.null(size1)) return(weight)
  levels <- intersect(names(size0), names(size1))
  ratio <- (size1[levels] / sum(size1[levels])) / (size0[levels] / sum(size0[levels]))
  ratio[!is.finite(ratio)] <- 0
  matched <- lc_group %in% levels
  weight[matched] <- ratio[lc_group[matched]]
  weight[weight <= 0] <- min(c(ratio[ratio > 0], 1)) / 10
  weight
}

# Person index for each row of a table: the top of its visit's ranking, with
# several rows per person where the table has them
.clinical_table_people <- function(people, table) {
  scale <- attr(people, "scale")
  n_persons <- table$n_persons
  n_main <- table$n_persons_main
  if (is.na(n_main)) n_main <- n_persons
  take_main <- min(sum(people$main), round(n_main * scale))
  take_other <- min(sum(!people$main), round(max(n_persons - n_main, 0) * scale))
  rank <- if (table$visit %in% "t1") people$rank_t1 else people$rank_main
  chosen <- c(which(!is.na(rank) & rank <= take_main),
              which(!is.na(people$rank_other) & people$rank_other <= take_other))
  chosen <- sort(chosen)
  if (isTRUE(table$person_level)) return(chosen)
  bands <- c(table$rpp_1, table$rpp_2, table$rpp_3_5, table$rpp_6_10, table$rpp_11_plus)
  bands[is.na(bands)] <- 0
  if (sum(bands) == 0) return(chosen)
  band <- sample.int(5L, length(chosen), replace = TRUE, prob = bands)
  low <- c(1, 2, 3, 6, 11)[band]
  high <- c(1, 2, 5, 10, 20)[band]
  count <- low + floor(stats::runif(length(chosen)) * (high - low + 1))
  rep(chosen, count)
}
