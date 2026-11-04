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

# The synthetic REACT population: everyone in the source's synthetic people
# (or `people`, an extraction result or a data frame with SUBJECT_ID, sex and
# age, as for react_hes()), with sex as "1"/"2"
.clinical_population <- function(source, people) {
  if (is.null(people)) {
    if (!inherits(source, "react_synthetic_source")) {
      stop("Synthetic Clinical Study data need a `react_synthetic()` source or `people`.", call. = FALSE)
    }
    people <- react_extract(source, families = "people-households/demographics", progress = FALSE)
  }
  population <- .hes_people(people)
  population$subject_id <- .clinical_subject_id(population$SUBJECT_ID)
  population <- population[!duplicated(population$subject_id), , drop = FALSE]
  population[order(population$subject_id), , drop = FALSE]
}

# Wrangled tables in the order of the nested list, with their kit rows and:
# - kind: "table" for data frames, "value" for other leaves (such as dates);
# - branch: the first path element of a subtree that mirrors top-level tables
#   (a separate group of people, such as registrants who weren't REACT
#   participants), or "" for the main cohort;
# - profile_table: the table whose distributions are used; a structure-only
#   table in a branch borrows those of the table it mirrors;
# - population: person-level tables covering many more people than registered
#   (the whole REACT study), generated for the whole synthetic population.
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
  if (!"profile_level" %in% names(tables)) tables$profile_level <- "full"
  rownames(tables) <- NULL

  paths <- tables$table
  tables$kind <- ifelse(grepl("data.frame", tables$class, fixed = TRUE), "table", "value")
  rest <- sub("^[^/]+/", "", paths)
  mirrored <- grepl("/", paths, fixed = TRUE) & rest %in% paths
  branch_roots <- unique(sub("/.*", "", paths[mirrored]))
  first <- sub("/.*", "", paths)
  tables$branch <- ifelse(first %in% branch_roots & grepl("/", paths, fixed = TRUE), first, "")
  tables$profile_table <- paths
  borrow <- mirrored & tables$profile_level %in% "structure_only"
  tables$profile_table[borrow] <- rest[borrow]

  tables$population <- FALSE
  registration <- .clinical_registration(profile, tables, "")
  if (!is.null(registration)) {
    tables$population <- tables$kind == "table" & tables$branch == "" & tables$person_level &
      !is.na(tables$n_persons) & tables$n_persons > 2 * registration$n_persons
  }
  tables
}

# The registration table of a branch: among its person-level tables with no
# visit and no more than three people for each main-cohort member (which
# leaves out tables covering the whole REACT study), the one with most columns
.clinical_registration <- function(profile, tables, branch) {
  candidates <- tables[tables$branch == branch & tables$kind == "table" & tables$person_level &
                         !is.na(tables$n_persons), , drop = FALSE]
  if (nrow(candidates) == 0L) return(NULL)
  ratio <- candidates$n_persons / candidates$n_persons_main
  narrow <- candidates[is.na(ratio) | ratio <= 3, , drop = FALSE]
  if (nrow(narrow) > 0L) candidates <- narrow
  no_visit <- candidates[is.na(candidates$visit), , drop = FALSE]
  if (nrow(no_visit) > 0L) candidates <- no_visit
  candidates[which.max(candidates$n_cols), , drop = FALSE]
}

.clinical_rows <- function(data, table, column = NULL, source = .clinical_release_source) {
  keep <- data$source == source & data$table == table
  if (!is.null(column)) keep <- keep & data$column == column
  data[keep, , drop = FALSE]
}

# Labels of an anchor's strata, as written in the profile: the levels of its
# column and the strata other columns are broken down by
.clinical_stratum_labels <- function(profile, stratum_var) {
  column <- if (stratum_var %in% names(profile$anchors)) profile$anchors[[stratum_var]] else NA_character_
  rows <- .clinical_anchor_rows(profile, column)
  labels <- c(rows$level[rows$stratum_var == "all"],
              profile$categorical$stratum[profile$categorical$stratum_var == stratum_var],
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

# Categorical rows of the table (in any source) whose counts best describe
# an anchor column: the one counting most people. The kit takes the anchors
# from combined_data_set.rds, so their counts are usually in "master".
.clinical_anchor_rows <- function(profile, column) {
  rows <- profile$categorical[profile$categorical$column %in% column, , drop = FALSE]
  overall <- rows[rows$stratum_var == "all", , drop = FALSE]
  if (nrow(overall) == 0L) return(rows[0L, , drop = FALSE])
  key <- paste(overall$source, overall$table, sep = "\r")
  totals <- tapply(overall$n_rows, key, sum, na.rm = TRUE)
  rows[paste(rows$source, rows$table, sep = "\r") == names(totals)[which.max(totals)], , drop = FALSE]
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

# The synthetic people: the cohort's registrants with all seven anchors, then
# the rest of the synthetic population (for population tables), plus one
# group of invented people for each branch. Main-stream random draws only, so
# the same arguments always give the same people.
.clinical_cohort <- function(source, people, profile, tables, n) {
  registration <- .clinical_registration(profile, tables, "")
  if (is.null(registration)) stop("The clinical profile has no person-level wrangled table.", call. = FALSE)
  if (is.null(n)) n <- registration$n_persons
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L) stop("`n` must be one positive whole number.", call. = FALSE)
  scale <- n / registration$n_persons

  population <- .clinical_population(source, people)
  adults <- which(!is.na(population$sex) & !is.na(population$age) & population$age >= 18)
  if (length(adults) < n) {
    stop("The synthetic people include ", length(adults), " adults, fewer than the ", n,
         " registrants requested. Use a larger `n_per_round` in react_synthetic() or a smaller `n`.",
         call. = FALSE)
  }
  main <- .clinical_people(population[adults, , drop = FALSE], profile, tables, registration, n, scale)
  rest <- population[!population$subject_id %in% main$subject_id, , drop = FALSE]
  if (nrow(rest) > 0L) {
    extra <- main[rep(NA_integer_, nrow(rest)), , drop = FALSE]
    extra$SUBJECT_ID <- rest$SUBJECT_ID
    extra$subject_id <- rest$subject_id
    extra$sex_code <- rest$sex
    extra$age <- rest$age
    extra$sex <- .clinical_sex_label(rest$sex, .clinical_stratum_labels(profile, "sex"))
    extra$age_group <- .clinical_age_group(rest$age, .clinical_stratum_labels(profile, "age_group"))
    anchors <- setdiff(names(main), c("SUBJECT_ID", "subject_id", "sex_code", "age", "sex", "age_group",
                                      "main", "registrant", "followup",
                                      grep("^rank_", names(main), value = TRUE)))
    for (anchor in anchors) extra[[anchor]] <- "(missing)"
    extra$main <- FALSE
    extra$registrant <- FALSE
    extra$followup <- FALSE
    flags <- attr(main, "flags")
    main <- rbind(main, extra)
    attr(main, "flags") <- rbind(flags, matrix(FALSE, nrow(rest), ncol(flags), dimnames = list(NULL, colnames(flags))))
  }
  rownames(main) <- NULL
  attr(main, "scale") <- scale

  branches <- list()
  for (branch in setdiff(unique(tables$branch), "")) {
    registration_b <- .clinical_registration(profile, tables, branch)
    if (is.null(registration_b)) next
    n_b <- max(1L, round(registration_b$n_persons * scale))
    pool <- data.frame(
      SUBJECT_ID = sprintf("%.0f", 9e8 + 1e6 * match(branch, unique(tables$branch)) + seq_len(4L * n_b)),
      sex = sample(c("1", "2"), 4L * n_b, replace = TRUE),
      age = sample(18:90, 4L * n_b, replace = TRUE),
      stringsAsFactors = FALSE
    )
    pool$subject_id <- as.numeric(pool$SUBJECT_ID)
    branches[[branch]] <- .clinical_people(pool, profile, tables[tables$branch == branch, , drop = FALSE],
                                           registration_b, n_b, scale)
  }
  list(people = main, branches = branches)
}

# Draw `n` registrants from `pool` and give them anchors, rankings and
# availability flags
.clinical_people <- function(pool, profile, tables, registration, n, scale) {
  anchors <- profile$anchors
  sex_labels <- .clinical_stratum_labels(profile, "sex")
  age_labels <- .clinical_stratum_labels(profile, "age_group")
  pool$sex_label <- .clinical_sex_label(pool$sex, sex_labels)
  pool$age_label <- .clinical_age_group(pool$age, age_labels)

  # weight the pool so that age group and sex follow the registration counts
  weight <- rep(1, nrow(pool))
  age_column <- if ("age_group" %in% names(anchors)) anchors[["age_group"]] else NA_character_
  age_rows <- .clinical_anchor_rows(profile, age_column)
  if (nrow(age_rows) > 0L) {
    rows <- age_rows
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
  n_main <- if (is.na(registration$n_persons_main)) 0L else min(n, round(registration$n_persons_main * scale))
  main_weight <- rep(1, n)
  if (nrow(age_rows) > 0L) {
    rows <- age_rows[age_rows$stratum_var == "main_cohort", , drop = FALSE]
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
  out$registrant <- TRUE

  # other anchors from their counts by age group, sex and main cohort
  strata <- out[c("sex", "age_group", "main_cohort")]
  for (anchor in setdiff(names(anchors), c("sex", "age_group"))) {
    value <- rep("(missing)", n)
    drawn <- .clinical_draw_levels(.clinical_anchor_rows(profile, anchors[[anchor]]), strata, stats::runif(n))
    if (!is.null(drawn)) value <- drawn
    out[[anchor]] <- value
  }

  # one ranking per visit: tables of a visit take the top of it, so they nest
  out$rank_main <- NA_real_
  out$rank_main[main] <- sample.int(n_main)
  out$rank_other <- NA_real_
  out$rank_other[!main] <- sample.int(n - n_main)
  out$rank_any <- sample.int(n)
  out$rank_t1 <- NA_real_
  if (n_main > 0L) {
    lc_group <- if (is.null(out$lc_group)) rep("(missing)", n_main) else out$lc_group[main]
    t1_weight <- .clinical_followup_weight(profile, tables, lc_group)
    out$rank_t1[main] <- order(sample.int(n_main, n_main, prob = t1_weight))
  }
  t1 <- tables[tables$visit %in% "t1" & tables$kind == "table" & !is.na(tables$n_persons_main), , drop = FALSE]
  n_followup <- if (nrow(t1) == 0L) 0 else round(max(t1$n_persons_main) * scale)
  out$followup <- !is.na(out$rank_t1) & out$rank_t1 <= n_followup
  attr(out, "flags") <- .clinical_flags(profile, out)
  attr(out, "scale") <- scale
  out
}

# Follow-up weights by long COVID group, so the T1 mix matches the T1 tables
.clinical_followup_weight <- function(profile, tables, lc_group) {
  weight <- rep(1, length(lc_group))
  person <- tables[tables$kind == "table" & tables$person_level & !is.na(tables$n_persons), , drop = FALSE]
  t0 <- person[person$visit %in% "t0", , drop = FALSE]
  t1 <- person[person$visit %in% "t1", , drop = FALSE]
  if (nrow(t0) == 0L || nrow(t1) == 0L) return(weight)
  size0 <- .clinical_stratum_sizes(profile, t0$profile_table[which.max(t0$n_persons)], "lc_group")
  size1 <- .clinical_stratum_sizes(profile, t1$profile_table[which.max(t1$n_persons)], "lc_group")
  if (is.null(size0) || is.null(size1)) return(weight)
  levels <- intersect(names(size0), names(size1))
  ratio <- (size1[levels] / sum(size1[levels])) / (size0[levels] / sum(size0[levels]))
  ratio[!is.finite(ratio)] <- 0
  matched <- lc_group %in% levels
  weight[matched] <- ratio[lc_group[matched]]
  weight[weight <= 0] <- min(c(ratio[ratio > 0], 1)) / 10
  weight
}

# Availability flags of main-cohort members, one column per flag, drawn from
# the kit's availability patterns (16_availability_patterns.csv): one pattern
# per group of flags and person, in proportion to its count. Follow-up flags go
# only to the follow-up group. Suppressed patterns count as zero.
.clinical_flags <- function(profile, people) {
  rows <- profile$categorical[startsWith(profile$categorical$table, "availability_patterns:") &
                                profile$categorical$stratum_var == "all", , drop = FALSE]
  rows$n_rows[is.na(rows$n_rows)] <- 0
  rows <- rows[rows$n_rows > 0, , drop = FALSE]
  flags <- setdiff(unique(unlist(strsplit(rows$level, "+", fixed = TRUE))), "(none)")
  out <- matrix(FALSE, nrow(people), length(flags), dimnames = list(NULL, flags))
  for (group in unique(rows$column)) {
    part <- rows[rows$column == group, , drop = FALSE]
    members <- people$main
    if (any(grepl("_t1$", flags[flags %in% unlist(strsplit(part$level, "+", fixed = TRUE))]))) {
      members <- members & people$followup
      part <- part[part$level != "(none)", , drop = FALSE]
    }
    members <- which(members)
    if (length(members) == 0L || nrow(part) == 0L) next
    pattern <- part$level[sample.int(nrow(part), length(members), replace = TRUE, prob = part$n_rows)]
    for (i in seq_along(members)) {
      set <- setdiff(strsplit(pattern[[i]], "+", fixed = TRUE)[[1L]], "(none)")
      out[members[[i]], set] <- TRUE
    }
  }
  out
}

# Person index for each row of a table: the top of its visit's ranking, with
# several rows per person where the table has them
.clinical_table_people <- function(people, table) {
  if (isTRUE(table$population)) return(seq_len(nrow(people)))
  registrant <- people$registrant
  scale <- attr(people, "scale")
  n_persons <- table$n_persons
  if (is.na(n_persons)) return(integer())
  n_main <- table$n_persons_main
  t1 <- table$visit %in% "t1"
  if (is.na(n_main)) {
    rank <- if (t1) people$rank_t1 else people$rank_any
    take <- min(sum(registrant & !is.na(rank)), round(n_persons * scale))
    chosen <- which(registrant & !is.na(rank) & rank <= take)
  } else {
    take_main <- min(sum(people$main), round(n_main * scale))
    take_other <- min(sum(registrant & !people$main), round(max(n_persons - n_main, 0) * scale))
    rank <- if (t1) people$rank_t1 else people$rank_main
    chosen <- c(which(!is.na(rank) & rank <= take_main),
                which(registrant & !is.na(people$rank_other) & people$rank_other <= take_other))
  }
  chosen <- sort(chosen)
  if (isTRUE(table$person_level)) return(chosen)
  bands <- c(table$rpp_1, table$rpp_2, table$rpp_3_5, table$rpp_6_10, table$rpp_11_plus)
  bands[is.na(bands)] <- 0
  if (sum(bands) == 0) return(chosen)
  low <- c(1, 2, 3, 6, 11)
  high <- c(1, 2, 5, 10, 20)
  # the top band's upper limit is set so that rows per person average out to
  # the table's rows per person
  if (bands[[5L]] > 0 && !is.na(table$n_rows)) {
    other <- sum(bands[1:4] * (low[1:4] + high[1:4]) / 2)
    target <- table$n_rows * sum(bands) / n_persons
    high[[5L]] <- max(11, round(2 * (target - other) / bands[[5L]] - 11))
  }
  band <- sample.int(5L, length(chosen), replace = TRUE, prob = bands)
  count <- low[band] + floor(stats::runif(length(chosen)) * (high[band] - low[band] + 1))
  rep(chosen, count)
}

# Person-level dates linked by the kit's intervals (20_timing.csv): each
# interval's "from" date that isn't itself a "to" date is drawn from its
# months; each "to" date is its "from" date plus an interval drawn from the
# interval's quantiles. A table named "master" (combined_data_set.rds, not
# generated) is resolved to the first wrangled table holding the column.
# Returns dates keyed "table:column".
.clinical_timing <- function(profile, tables, people) {
  timing <- profile$timing[profile$timing$source %in% .clinical_release_source, , drop = FALSE]
  if (nrow(timing) == 0L || !all(c("from", "to") %in% names(timing))) return(list())
  columns <- profile$columns[profile$columns$source == .clinical_release_source, , drop = FALSE]
  main_tables <- tables[tables$branch == "" & tables$kind == "table", , drop = FALSE]
  resolve <- function(key) {
    table <- sub(":[^:]*$", "", key)
    column <- sub("^.*:", "", key)
    if (table %in% main_tables$table) {
      return(if (any(columns$table == table & columns$column == column)) key else NA_character_)
    }
    holders <- main_tables$table[main_tables$table %in% columns$table[columns$column == column]]
    if (length(holders) == 0L) return(NA_character_)
    paste0(holders[[1L]], ":", column)
  }
  timing$from_key <- vapply(timing$from, resolve, character(1L))
  timing$to_key <- vapply(timing$to, resolve, character(1L))
  timing <- timing[!is.na(timing$from_key) & !is.na(timing$to_key), , drop = FALSE]
  if (nrow(timing) == 0L) return(list())

  draw_root <- function(key) {
    table <- sub(":[^:]*$", "", key)
    column <- sub("^.*:", "", key)
    source_table <- main_tables$profile_table[main_tables$table == table][1L]
    .clinical_draw_dates(.clinical_rows(profile$date_months, source_table, column),
                         .clinical_rows(profile$date_summary, source_table, column), nrow(people))
  }
  dates <- list()
  for (key in setdiff(unique(timing$from_key), timing$to_key)) {
    value <- draw_root(key)
    if (!is.null(value)) dates[[key]] <- value
  }
  repeat {
    added <- FALSE
    for (i in seq_len(nrow(timing))) {
      from <- timing$from_key[[i]]
      to <- timing$to_key[[i]]
      if (is.null(dates[[from]]) || !is.null(dates[[to]])) next
      quantile <- .clinical_quantile_fun(unlist(timing[i, .clinical_quantile_columns], use.names = FALSE))
      if (is.null(quantile)) next
      # a placeholder "from" date stands for an unknown date, so the interval
      # starts from another person's real date instead
      start <- dates[[from]]
      fake <- !is.na(start) & start < as.Date("1900-01-01")
      real <- start[!is.na(start) & !fake]
      if (any(fake) && length(real) > 0L) start[fake] <- real[sample.int(length(real), sum(fake), replace = TRUE)]
      value <- start + round(quantile(stats::runif(nrow(people))))
      dates[[to]] <- .clinical_placeholders(value, profile, main_tables, to)
      added <- TRUE
    }
    if (!added) break
  }
  dates
}

# Keep placeholder dates (months before 1900, such as SPSS's 1582-10-14) at
# the share the released months give them
.clinical_placeholders <- function(value, profile, tables, key) {
  table <- tables$profile_table[tables$table == sub(":[^:]*$", "", key)][1L]
  months <- .clinical_rows(profile$date_months, table, sub("^.*:", "", key))
  months <- months[months$stratum_var == "all" & !is.na(months$level), , drop = FALSE]
  counts <- months$n_rows
  counts[is.na(counts)] <- 0
  early <- months$level < "1900"
  if (sum(counts) == 0 || !any(early & counts > 0)) return(value)
  placeholder <- stats::runif(length(value)) < sum(counts[early]) / sum(counts)
  levels <- months$level[early & counts > 0]
  picked <- levels[sample.int(length(levels), sum(placeholder), replace = TRUE, prob = counts[early & counts > 0])]
  value[placeholder] <- .clinical_placeholder_date(picked)
  value
}

# The date a placeholder month stands for: SPSS's zero date for 1582-10, the
# first of the month otherwise
.clinical_placeholder_date <- function(month) {
  out <- as.Date(sprintf("%s-01", month))
  out[month == "1582-10"] <- as.Date("1582-10-14")
  out
}
