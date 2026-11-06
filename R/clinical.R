# REACT Clinical Study wrangled release --------------------------------------------

#' Create a source for the real Clinical Study wrangled release
#'
#' The wrangled release is the nested list of tables that the canonical
#' wrangling pipeline saves as `ge_lc_wrangled.rds`, usually in
#' `file.path(Sys.getenv("REACT_DATA_ROOT"), "metadata")` in the secure
#' environment.
#'
#' @param path The path of `ge_lc_wrangled.rds`, or the list already read.
#' @return A source object for [react_clinical()].
#' @export
react_wrangled <- function(path) {
  if (is.list(path) && !is.data.frame(path)) {
    return(structure(list(kind = "wrangled", path = NA_character_, data = path),
                     class = c("react_wrangled_source", "react_source")))
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path)) {
    stop("`path` must be the wrangled release's RDS file or the list read from it.", call. = FALSE)
  }
  structure(list(kind = "wrangled", path = path, data = NULL),
            class = c("react_wrangled_source", "react_source"))
}

#' REACT Clinical Study tables
#'
#' Returns the REACT Clinical Study's wrangled release: the nested list of
#' tables (registration, clinic tests, bloods, sequencing status, health
#' surveys and REACT-1/2 records) that the canonical wrangling pipeline saves
#' as `ge_lc_wrangled.rds`. As with [react_extract()], only the source changes
#' between synthetic and real data:
#'
#' - with [react_synthetic()], a fictional cohort is drawn from the source's
#'   synthetic people and its tables are generated from a Clinical Study
#'   profile;
#' - with [react_wrangled()], the real list is read.
#'
#' Both give the same table paths, column names, column order and column
#' classes, so code written on the synthetic tables runs on the real ones.
#' Each synthetic `subject_id` is a nine-digit number made from the person's
#' reactextract `SUBJECT_ID`.
#'
#' Values follow the profile's distributions within sex, age group, long COVID
#' group, main-cohort membership and the other anchors, and columns of one
#' table follow its rank correlations; a person's tables share their anchors
#' and identifiers. Follow-up values are correlated with the same person's
#' baseline values, and dates in different tables follow the profile's
#' intervals between them. Each main-cohort member also gets availability
#' flags from the profile's patterns, which decide who has samples in
#' [react_clinical_omics()]. `inst/CLINICAL_SYNTHETIC.md` describes the design
#' and what is not generated yet. **Synthetic data are for learning and
#' writing code, not for findings.**
#'
#' @param source A source from [react_synthetic()] or [react_wrangled()].
#' @param tables `"all"`, or paths in the list such as `"registration_data"`
#'   or `"clinic_data/assay"`. A path selects that element and everything
#'   below it.
#' @param profile A profile from [react_clinical_profile()]. A synthetic
#'   source needs one and defaults to `react_clinical_profile()`. With a real
#'   source, a profile is optional: if given, the real tables are checked
#'   against its column names, order and classes, with a warning for any
#'   difference.
#' @param n Synthetic registrants; defaults to the profile's number. The
#'   number of people in every table scales with it; tables without people
#'   keep their row count.
#' @param people Optional synthetic people to draw the cohort from: an
#'   extraction result with the `people-households/demographics` family, or a
#'   data frame with `SUBJECT_ID`, `sex` and `age`. Defaults to every person
#'   in `source`. Only adults with a recorded sex are drawn.
#' @param seed Random seed for synthetic tables; defaults to the source's
#'   seed. The same source, profile, people, `n` and seed always give the same
#'   tables, whichever `tables` are requested, and the global random number
#'   state is left unchanged.
#'
#' @return A nested list of data frames, named like the real list.
#' @export
react_clinical <- function(source, tables = "all", profile = NULL, n = NULL, people = NULL,
                           seed = NULL) {
  if (!inherits(source, "react_source")) {
    stop("`source` must be created by `react_synthetic()` or `react_wrangled()`.", call. = FALSE)
  }
  if (!is.character(tables) || length(tables) == 0L || anyNA(tables)) {
    stop("`tables` must be \"all\" or table paths.", call. = FALSE)
  }
  if (inherits(source, "react_wrangled_source")) {
    data <- if (is.null(source$data)) readRDS(source$path) else source$data
    leaves <- .clinical_flatten(data)
    leaves <- leaves[.clinical_select_paths(names(leaves), tables)]
    if (!is.null(profile)) .clinical_check_structure(leaves, .clinical_check_profile(profile))
    return(.clinical_nest(leaves))
  }
  if (!inherits(source, "react_synthetic_source")) {
    stop("Clinical Study tables come from `react_synthetic()` or `react_wrangled()`.", call. = FALSE)
  }
  profile <- .clinical_check_profile(if (is.null(profile)) react_clinical_profile() else profile)
  schema <- .clinical_tables(profile)
  if (nrow(schema) == 0L) stop("The clinical profile has no wrangled tables.", call. = FALSE)
  paths <- schema$table[.clinical_select_paths(schema$table, tables)]
  .clinical_warn_partial(profile, schema[schema$table %in% paths & schema$kind == "table", , drop = FALSE])
  if (is.null(seed)) seed <- source$seed

  .hes_with_seed(seed, {
    cohort <- .clinical_cohort(source, people, profile, schema, n)
    dates <- .clinical_timing(profile, schema, cohort$people)
    branch_key <- function(branch) if (branch == "") ".main" else branch
    ids <- list()
    for (branch in unique(schema$branch[schema$table %in% paths])) {
      frame <- if (branch == "") cohort$people else cohort$branches[[branch]]
      if (is.null(frame)) next
      in_branch <- paths[schema$branch[match(paths, schema$table)] == branch]
      ids[[branch_key(branch)]] <- .clinical_person_ids(profile, in_branch, frame, seed, branch)
    }
    pairs <- .clinical_visit_pairs(profile, schema)
    generated <- new.env(parent = emptyenv())

    # each table has its own random stream; a follow-up table first makes its
    # baseline table, whose probabilities it uses
    make <- function(path) {
      if (exists(path, envir = generated, inherits = FALSE)) return(get(path, envir = generated))
      table <- schema[schema$table == path, , drop = FALSE]
      if (table$kind == "value") {
        data <- .clinical_value(profile, table)
      } else {
        frame <- if (table$branch == "") cohort$people else cohort$branches[[table$branch]]
        baseline <- NULL
        link <- pairs[[path]]
        if (!is.null(link) && !is.null(frame)) {
          latent <- attr(make(link$t0), "latent")
          if (!is.null(latent) && length(latent$rows) > 0L) {
            u <- matrix(NA_real_, nrow(frame), ncol(latent$u), dimnames = list(NULL, colnames(latent$u)))
            first <- !duplicated(latent$rows)
            u[latent$rows[first], ] <- latent$u[first, , drop = FALSE]
            baseline <- list(u = u, rho = link$rho)
          }
        }
        keep <- unique(unlist(lapply(pairs, function(p) if (identical(p$t0, path)) names(p$rho))))
        if (is.null(keep)) keep <- character()
        data <- .with_stream_seed(seed, "clinical", path, {
          rows <- if (is.null(frame) || !isTRUE(table$has_persons)) NULL else .clinical_table_people(frame, table)
          .clinical_table(profile, table, frame, rows, ids[[branch_key(table$branch)]],
                          if (table$branch == "") dates else list(), baseline, keep)
        })
      }
      assign(path, data, envir = generated)
      data
    }
    leaves <- list()
    for (path in paths) {
      data <- make(path)
      attr(data, "latent") <- NULL
      leaves[path] <- list(data)
    }
    .clinical_nest(leaves)
  })
}

# Follow-up tables linked to their baseline tables by the kit's visit pairs
# (19_visit_pairs.csv): a family names a path element, and the baseline table
# is the follow-up path with "t1" changed to "t0" in its last element. A list
# by follow-up path of the baseline path and the columns' rank correlations.
.clinical_visit_pairs <- function(profile, schema) {
  pairs <- profile$visit_pairs
  if (nrow(pairs) == 0L || !all(c("family", "column", "rho") %in% names(pairs))) return(list())
  pairs <- pairs[pairs$source %in% .clinical_release_source & !is.na(pairs$rho), , drop = FALSE]
  out <- list()
  follow <- schema[schema$visit %in% "t1" & schema$kind == "table" & schema$branch == "", , drop = FALSE]
  for (path in follow$table) {
    parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
    base <- paste(c(utils::head(parts, -1L), gsub("t1", "t0", parts[[length(parts)]], fixed = TRUE)),
                  collapse = "/")
    rows <- pairs[pairs$family %in% parts, , drop = FALSE]
    if (!base %in% schema$table || nrow(rows) == 0L) next
    out[[path]] <- list(t0 = base, rho = stats::setNames(rows$rho, rows$column))
  }
  out
}

# Tables whose inventory lists fewer columns than the table has (partial in
# the first kit run) are generated with the listed columns only
.clinical_warn_partial <- function(profile, schema) {
  listed <- table(factor(profile$columns$table[profile$columns$source == .clinical_release_source],
                         levels = schema$table))
  short <- schema$table[!is.na(schema$n_cols) & as.numeric(listed[schema$table]) < schema$n_cols]
  if (length(short) > 0L) {
    warning("The clinical profile lists only some columns of: ", paste(short, collapse = ", "),
            ". Only those columns are generated.", call. = FALSE)
  }
}

# Leaves of a nested list, named by their path ("clinic_data/assay/...")
.clinical_flatten <- function(x, prefix = NULL) {
  out <- list()
  for (name in names(x)) {
    path <- if (is.null(prefix)) name else paste(prefix, name, sep = "/")
    item <- x[[name]]
    if (is.list(item) && !is.data.frame(item)) {
      out <- c(out, .clinical_flatten(item, path))
    } else {
      out[path] <- list(item)
    }
  }
  out
}

.clinical_select_paths <- function(paths, tables) {
  if (identical(tables, "all")) return(seq_along(paths))
  tables <- sub("/+$", "", gsub("[$]", "/", tables))
  selected <- lapply(tables, function(t) which(paths == t | startsWith(paths, paste0(t, "/"))))
  unknown <- tables[lengths(selected) == 0L]
  if (length(unknown) > 0L) {
    stop("Unknown Clinical Study table(s): ", paste(unknown, collapse = ", "), ". Tables are: ",
         paste(unique(sub("/.*", "", paths)), collapse = ", "), ".", call. = FALSE)
  }
  sort(unique(unlist(selected)))
}

.clinical_nest <- function(leaves) {
  put <- function(tree, parts, value) {
    if (length(parts) == 1L) {
      tree[parts] <- list(value)
      return(tree)
    }
    child <- if (is.null(tree[[parts[[1L]]]])) list() else tree[[parts[[1L]]]]
    tree[[parts[[1L]]]] <- put(child, parts[-1L], value)
    tree
  }
  out <- list()
  for (path in names(leaves)) out <- put(out, strsplit(path, "/", fixed = TRUE)[[1L]], leaves[[path]])
  out
}

# Real tables against the profile's inventory: names, order and classes
.clinical_check_structure <- function(leaves, profile) {
  columns <- profile$columns[profile$columns$source == .clinical_release_source, , drop = FALSE]
  problems <- character()
  for (path in names(leaves)) {
    expected <- columns[columns$table == path, , drop = FALSE]
    expected <- expected[order(expected$position), , drop = FALSE]
    data <- leaves[[path]]
    if (nrow(expected) == 0L || !is.data.frame(data)) next
    observed_class <- vapply(data, function(x) class(x)[[1L]], character(1L))
    if (!identical(names(data), expected$column)) {
      problems <- c(problems, paste0(path, ": column names or order differ"))
    } else if (!identical(unname(observed_class), expected$class)) {
      problems <- c(problems, paste0(path, ": column classes differ"))
    }
  }
  if (length(problems) > 0L) {
    warning("The wrangled release differs from the clinical profile's inventory:\n",
            paste0("- ", problems, collapse = "\n"), call. = FALSE)
  }
  invisible(problems)
}
