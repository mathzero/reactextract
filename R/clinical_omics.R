# Synthetic Clinical Study omics files ----------------------------------------------

#' REACT Clinical Study omics files (synthetic)
#'
#' Returns synthetic versions of the Clinical Study's omics files for the same
#' synthetic cohort as [react_clinical()] (the same source, profile, `n`,
#' `people` and `seed` give the same people). Each file comes back shaped like
#' the object read from the real file: a data frame for a CSV or a single
#' table, a named list for an RDS holding several objects (such as the
#' proteomics tables, or a tximport list of RNA-seq matrices).
#'
#' Samples go to people with the matching availability flag, drawn from the
#' profile's availability patterns. Values come from each platform's low-rank
#' structure and feature quantiles; tables that hold the same measurements
#' (such as raw and normalised values, or one platform's rows in a table that
#' stacks several) share samples and values. A follow-up sample is correlated
#' with the same person's baseline sample feature by feature.
#'
#' Long tables take the source files' column order and classes, and reference
#' tables without people (such as the proteomics name lookup) are generated
#' from their profiles. Known gaps are listed in `inst/CLINICAL_SYNTHETIC.md`:
#' for example, RNA-seq matrices have only the profiled genes, and their
#' samples aren't linked to people because the sequencing manifests aren't
#' generated. There is no real-data path: in the secure environment, read the
#' files themselves. **Synthetic data are for learning and writing code, not
#' for findings.**
#'
#' @inheritParams react_clinical
#' @param source A source from [react_synthetic()].
#' @param sources `"all"`, or the inventory kit's source names (such as
#'   `"proteomics"` or `"metabolomics_rpos"`).
#' @return A named list by source.
#' @export
react_clinical_omics <- function(source, sources = "all", profile = NULL, n = NULL, people = NULL,
                                 seed = NULL) {
  if (!inherits(source, "react_synthetic_source")) {
    stop("Synthetic omics files come from a `react_synthetic()` source.", call. = FALSE)
  }
  profile <- .clinical_check_profile(if (is.null(profile)) react_clinical_profile() else profile)
  available <- .clinical_omics_sources(profile)
  if (identical(sources, "all")) sources <- available
  unknown <- setdiff(sources, available)
  if (length(unknown) > 0L) {
    stop("Unknown or unprofiled omics source(s): ", paste(unknown, collapse = ", "), ". Sources are: ",
         paste(available, collapse = ", "), ".", call. = FALSE)
  }
  schema <- .clinical_tables(profile)
  if (is.null(seed)) seed <- source$seed

  .hes_with_seed(seed, {
    people <- .clinical_cohort(source, people, profile, schema, n)$people
    out <- list()
    for (name in sources) {
      out[name] <- list(.with_stream_seed(seed, "clinical-omics", name,
                                          .clinical_omics_source(profile, name, people, schema, seed)))
    }
    out
  })
}

# Omics sources with something to generate (platforms, or person-level
# tables) held in data frames or matrices; workbook folders are left out
.clinical_omics_sources <- function(profile) {
  omics <- profile$source_status$source[profile$source_status$group %in% "omics"]
  with_platforms <- unique(profile$omics_platform$source)
  with_tables <- unique(profile$tables$source[profile$tables$has_persons %in% TRUE])
  readable <- unique(profile$structure$source[grepl("data.frame|matrix", profile$structure$class)])
  omics[omics %in% c(with_platforms, with_tables) & omics %in% readable]
}

# The objects of one source, named by their paths in the file
.clinical_omics_source <- function(profile, source, people, schema, seed) {
  structure <- profile$structure[profile$structure$source == source, , drop = FALSE]
  platforms <- profile$omics_platform[profile$omics_platform$source == source, , drop = FALSE]
  tables <- profile$tables[profile$tables$source == source, , drop = FALSE]
  scale <- attr(people, "scale")
  groups <- .clinical_omics_groups(platforms)
  # identifiers shared with the wrangled list (such as a barcode) take the
  # person's value from there, so the files link
  ids <- .clinical_person_ids(profile, schema$table[schema$branch == ""], people, seed)
  names(ids) <- tolower(names(ids))
  generated <- list()
  measure <- function(group) {
    key <- as.character(group)
    if (is.null(generated[[key]])) {
      generated[[key]] <<- .clinical_omics_measure(profile, platforms[groups == group, , drop = FALSE],
                                                   people, scale)
    }
    generated[[key]]
  }

  out <- list()
  single <- nrow(structure) == 1L
  if (any(platforms$layout %in% "tximport")) {
    return(.clinical_tximport(profile, platforms[platforms$layout == "tximport", , drop = FALSE][1L, ],
                              structure, measure(groups[platforms$layout == "tximport"][1L])))
  }
  for (i in seq_len(nrow(structure))) {
    path <- structure$path[[i]]
    stem <- tolower(tools::file_path_sans_ext(basename(path)))
    base <- tolower(sub(":.*", "", platforms$platform))
    matched <- platforms[endsWith(stem, base) | (single & nrow(platforms) > 0L), , drop = FALSE]
    if (nrow(matched) > 0L) {
      value <- .clinical_omics_object(profile, source, matched, groups[match(matched$platform, platforms$platform)],
                                      measure, structure[i, , drop = FALSE], people, ids)
      out[path] <- list(value)
      next
    }
    table <- tables[tables$table == tools::file_path_sans_ext(path) | tables$table == path, , drop = FALSE]
    if (nrow(table) != 1L) next
    if (isTRUE(table$has_persons)) {
      out[path] <- list(.clinical_omics_person_table(profile, source, table, structure[i, , drop = FALSE],
                                                     people, scale, ids))
    } else {
      names <- .clinical_omics_names(platforms, groups, measure, table$table)
      out[path] <- list(.clinical_omics_reference(profile, source, table, structure[i, , drop = FALSE], names))
    }
  }
  if (single && length(out) == 1L) out[[1L]] else out
}

# Platforms of a source that hold the same measurements (same samples and
# features), such as raw and normalised values or a platform's rows in a
# stacked table, share one group
.clinical_omics_groups <- function(platforms) {
  key <- paste(platforms$n_samples_source, platforms$n_features, sep = "x")
  match(key, unique(key))
}

# Samples and probabilities for one group of platforms: which people (and
# visits) have a sample, and each sample's probability for each feature of
# the group's first platform
.clinical_omics_measure <- function(profile, platforms, people, scale) {
  lead <- platforms[1L, , drop = FALSE]
  flags <- attr(people, "flags")
  sample_rows <- integer()
  visit <- character()
  for (v in c("t0", "t1")) {
    wanted <- lead[[paste0("n_samples_", v)]]
    if (is.na(wanted) || wanted == 0) next
    wanted <- round(wanted * scale)
    pool <- if (v == "t1") which(people$followup) else which(people$main)
    flag <- .clinical_omics_flag(lead$platform, v, flags, wanted)
    if (!is.na(flag)) pool <- intersect(pool, which(flags[, flag]))
    if (length(pool) < wanted) {
      extra <- setdiff(if (v == "t1") which(people$followup) else which(people$main), pool)
      pool <- c(pool, extra[seq_len(min(length(extra), wanted - length(pool)))])
    }
    chosen <- sort(pool[sample.int(length(pool), min(wanted, length(pool)))])
    sample_rows <- c(sample_rows, chosen)
    visit <- c(visit, rep(v, length(chosen)))
  }
  unlinked <- length(sample_rows) == 0L
  n <- if (unlinked) round(lead$n_samples_profiled * scale) else length(sample_rows)
  u <- .clinical_omics_scores(profile, lead, n, if (unlinked) NULL else sample_rows, visit)
  features <- .clinical_omics_features(profile, lead)$feature
  list(rows = if (unlinked) NULL else sample_rows, visit = visit, u = u, lead = lead, platforms = platforms,
       features = features, maps = .clinical_omics_maps(profile, platforms, features),
       ids = .clinical_omics_sample_ids(profile, lead$source, lead, n))
}

# For each platform of a group, the group's (first platform's) feature that
# each of its features stands for: by name (including names already matched
# for an earlier platform of the group), then by identical quantiles (a
# stacked table can name proteins differently but holds the same values),
# then by position
.clinical_omics_maps <- function(profile, platforms, features) {
  signature <- function(f) do.call(paste, c(lapply(f[.clinical_quantile_columns], format), sep = "|"))
  lead <- .clinical_omics_features(profile, platforms[1L, , drop = FALSE])
  known <- stats::setNames(seq_along(features), features)
  maps <- list()
  for (i in seq_len(nrow(platforms))) {
    rows <- .clinical_omics_features(profile, platforms[i, , drop = FALSE])
    at <- unname(known[rows$feature])
    if (anyNA(at)) {
      by_quantiles <- match(signature(rows), signature(lead))
      at[is.na(at)] <- by_quantiles[is.na(at)]
    }
    at[is.na(at)] <- ((seq_along(at) - 1L) %% length(features) + 1L)[is.na(at)]
    new <- !rows$feature %in% names(known)
    known <- c(known, stats::setNames(at[new], rows$feature[new]))
    maps[[platforms$platform[[i]]]] <- stats::setNames(at, rows$feature)
  }
  maps
}

# The availability flag for a platform at a visit: the flag (ending _t0 or
# _t1) sharing a word with the platform's name, otherwise the flag with the
# fewest people that still covers the samples wanted. Flags are matched by
# their names in the profile, so none is written here.
.clinical_omics_flag <- function(platform, visit, flags, wanted) {
  if (is.null(flags) || ncol(flags) == 0L) return(NA_character_)
  names <- colnames(flags)
  candidates <- names[grepl(paste0("_", visit, "$"), names)]
  if (length(candidates) == 0L) candidates <- names
  words <- function(x) setdiff(strsplit(tolower(x), "[^a-z0-9]+")[[1L]], c("", "has", visit, "data"))
  mine <- words(sub(":.*", "", platform))
  overlap <- vapply(candidates, function(flag) length(intersect(words(flag), mine)), integer(1L))
  if (any(overlap > 0L)) return(candidates[which.max(overlap)])
  # synthetic flag counts vary around the profile's, hence the 10% margin
  counts <- colSums(flags[, candidates, drop = FALSE])
  covering <- candidates[counts >= 0.9 * wanted]
  if (length(covering) == 0L) return(NA_character_)
  covering[which.min(counts[covering])]
}

.clinical_omics_features <- function(profile, platform) {
  profile$omics_features[profile$omics_features$source == platform$source &
                           profile$omics_features$platform == platform$platform, , drop = FALSE]
}

# Probabilities for each sample and feature: normal scores from the
# platform's components (33_omics_eigen.csv, 34_omics_loadings.csv), with a
# follow-up sample correlated feature by feature with the same person's
# baseline sample (36_omics_visit.csv)
.clinical_omics_scores <- function(profile, platform, n, rows = NULL, visit = character()) {
  features <- .clinical_omics_features(profile, platform)
  if (nrow(features) == 0L) {
    stop("The clinical profile has no features for platform ", platform$platform, ".", call. = FALSE)
  }
  pick <- function(data) data[data$source == platform$source & data$platform == platform$platform, , drop = FALSE]
  eigen_rows <- pick(profile$omics_eigen)
  eigen_rows <- eigen_rows[order(eigen_rows$pc), , drop = FALSE]
  loadings <- pick(profile$omics_loadings)
  n_features <- nrow(features)
  z <- matrix(stats::rnorm(n * n_features), n, n_features)
  pcs <- intersect(paste0("pc", eigen_rows$pc), names(loadings))
  if (length(pcs) > 0L) {
    lambda <- eigen_rows$eigenvalue[match(pcs, paste0("pc", eigen_rows$pc))]
    lambda[is.na(lambda) | lambda < 0] <- 0
    l <- as.matrix(loadings[match(features$feature, loadings$feature), pcs, drop = FALSE])
    l[is.na(l)] <- 0
    weighted <- l * rep(sqrt(lambda), each = n_features)
    common_variance <- rowSums(weighted^2)
    residual_sd <- sqrt(pmax(1 - common_variance, 0.01))
    scores <- matrix(stats::rnorm(n * length(pcs)), n, length(pcs))
    z <- scores %*% t(weighted) + z * rep(residual_sd, each = n)
    z <- z / rep(sqrt(common_variance + residual_sd^2), each = n)
  }
  stability <- pick(profile$omics_visit)
  if (!is.null(rows) && nrow(stability) > 0L && any(visit == "t1")) {
    rho <- stability$rho[match(features$feature, stability$feature)]
    r <- 2 * sin(pi * rho / 6)
    r[is.na(r)] <- 0
    later <- which(visit == "t1")
    earlier <- match(rows[later], rows[visit == "t0"])
    has <- !is.na(earlier)
    base <- which(visit == "t0")[earlier[has]]
    z[later[has], ] <- z[base, , drop = FALSE] * rep(r, each = sum(has)) +
      z[later[has], , drop = FALSE] * rep(sqrt(1 - r^2), each = sum(has))
  }
  stats::pnorm(z)
}

# Values of one platform's features at the group's probabilities (through
# the group's feature map)
.clinical_omics_values <- function(profile, platform, measure) {
  features <- .clinical_omics_features(profile, platform)
  at <- unname(measure$maps[[platform$platform]])
  n <- nrow(measure$u)
  out <- matrix(NA_real_, n, nrow(features), dimnames = list(NULL, features$feature))
  for (j in seq_len(nrow(features))) {
    quantile <- .clinical_quantile_fun(unlist(features[j, .clinical_quantile_columns], use.names = FALSE))
    if (is.null(quantile)) next
    # the kit's feature quantiles are of the raw values (its transform is only
    # for the low-rank summary, which uses ranks)
    value <- quantile(measure$u[, at[[j]]])
    missing <- features$share_missing[[j]]
    if (!is.na(missing) && missing > 0) value[stats::runif(n) < missing] <- NA
    out[, j] <- value
  }
  out
}

# The sample-level columns of a platform's table, with the sample column
# holding the group's sample IDs
.clinical_omics_samples <- function(profile, source, platform, measure, people, ids) {
  table_name <- c(paste0(platform$platform, "/samples"), paste0(sub(":.*", "", platform$platform), "/samples"))
  table_name <- table_name[table_name %in% profile$tables$table[profile$tables$source == source]][1L]
  n <- nrow(measure$u)
  if (is.na(table_name)) return(data.frame(row.names = seq_len(n)))
  table <- data.frame(table = table_name, profile_table = table_name, class = "data.frame", n_rows = n,
                      stringsAsFactors = FALSE)
  data <- .clinical_table(profile, table, people, measure$rows, ids = .clinical_ids_for(profile, source, table_name, ids),
                          source = source, n = n)
  data <- as.data.frame(data, stringsAsFactors = FALSE, optional = TRUE)
  if (!is.na(platform$sample_column) && platform$sample_column %in% names(data)) {
    data[[platform$sample_column]] <- .clinical_as_class(measure$ids, class(data[[platform$sample_column]])[1L])
  }
  data
}

# One object of a source built from one or more platforms: wide (samples by
# features), long (one row per sample and feature), or a stack of several
# platforms' long rows (split by a column such as "platform = Olink")
.clinical_omics_object <- function(profile, source, platforms, groups, measure, structure, people, ids) {
  classes <- strsplit(structure$class, "/", fixed = TRUE)[[1L]]
  if ("data.table" %in% classes) classes <- "data.frame"
  stacked <- "split" %in% names(platforms) && any(!is.na(platforms$split))
  parts <- list()
  for (group in unique(groups)) {
    in_group <- platforms[groups == group, , drop = FALSE]
    m <- measure(group)
    lead <- in_group[1L, , drop = FALSE]
    samples <- .clinical_omics_samples(profile, source, lead, m, people, ids)
    if (identical(lead$layout, "wide")) {
      values <- .clinical_omics_values(profile, lead, m)
      wide <- cbind(samples, as.data.frame(values, optional = TRUE))
      names(wide) <- c(names(samples), colnames(values))
      parts[[length(parts) + 1L]] <- wide
      next
    }
    parts[[length(parts) + 1L]] <- .clinical_omics_long(profile, source, in_group, m, samples)
  }
  data <- if (length(parts) == 1L) parts[[1L]] else do.call(rbind, .clinical_align(parts))
  rownames(data) <- NULL
  order <- profile$source_columns[profile$source_columns$source == source &
                                    profile$source_columns$table == sub(":.*", "", platforms$platform[[1L]]), ,
                                  drop = FALSE]
  if (nrow(order) > 0L) data <- .clinical_source_order(data, order)
  class(data) <- classes
  data
}

# A table's columns in the source file's order and classes
# (04b_source_columns.csv); a column the kit's tables don't hold is missing
.clinical_source_order <- function(data, order) {
  order <- order[order(order$position), , drop = FALSE]
  out <- lapply(seq_len(nrow(order)), function(i) {
    name <- order$column[[i]]
    if (name %in% names(data)) .clinical_as_class(data[[name]], order$class[[i]]) else
      .clinical_na(order$class[[i]], nrow(data))
  })
  names(out) <- order$column
  data <- as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
  names(data) <- order$column
  data
}

# Protein names for a source's reference tables (such as a name lookup):
# each platform's own names, with the name the group's stacked table uses
# for the same protein. Platforms whose names share a word with the table's
# are used if there are any.
.clinical_omics_names <- function(platforms, groups, measure, table) {
  if (nrow(platforms) == 0L) return(data.frame(own = character(), clean = character(), platform = character()))
  words <- function(x) strsplit(tolower(x), "[^a-z0-9]+")[[1L]]
  mine <- vapply(platforms$platform, function(p) length(intersect(words(sub(":.*", "", p)), words(table))) > 0L,
                 logical(1L))
  out <- list()
  for (group in unique(groups)) {
    m <- measure(group)
    stacked <- m$platforms$platform[!is.na(m$platforms$split)]
    clean <- if (length(stacked) > 0L) {
      map <- m$maps[[stacked[[1L]]]]
      names(map)[match(seq_along(m$features), map)]
    } else m$features
    for (platform in m$platforms$platform) {
      if (platform %in% stacked || (any(mine) && !platform %in% platforms$platform[mine])) next
      map <- m$maps[[platform]]
      out[[length(out) + 1L]] <- data.frame(own = names(map), clean = clean[map], platform = sub(":.*", "", platform),
                                            group = group, stringsAsFactors = FALSE)
    }
  }
  names <- if (length(out) == 0L) data.frame(own = character(), clean = character(), platform = character(),
                                             group = integer()) else do.call(rbind, out)
  # one row per protein name and group: tables of the same measurements (wide
  # and long) share their names
  names <- names[!duplicated(names[c("own", "group")]), , drop = FALSE]
  attr(names, "matched") <- any(mine)
  names
}

# A reference table of an omics source (no people): generated from its
# profile, with its name columns (high cardinality text) holding the
# source's protein names: a platform's own name in the first, the stacked
# table's name for the same protein in the others. A single name column
# holds the matched platforms' own names, or else the stacked table's names.
.clinical_omics_reference <- function(profile, source, table, structure, names) {
  table$profile_table <- table$table
  table$class <- if (grepl("data.frame", structure$class, fixed = TRUE)) structure$class else "data.frame"
  data <- .clinical_table(profile, table, NULL, NULL, source = source)
  columns <- .clinical_rows(profile$columns, table$table, source = source)
  name_columns <- columns$column[columns$role == "high_cardinality" & columns$class == "character"]
  n <- nrow(data)
  if (n == 0L || length(name_columns) == 0L || nrow(names) == 0L) return(data)
  if (length(name_columns) == 1L) {
    distinct <- unique(if (isTRUE(attr(names, "matched"))) names$own else names$clean)
    data[[name_columns]] <- distinct[(seq_len(n) - 1L) %% length(distinct) + 1L]
    return(data)
  }
  pick <- (seq_len(n) - 1L) %% nrow(names) + 1L
  data[[name_columns[[1L]]]] <- names$own[pick]
  for (name in name_columns[-1L]) data[[name]] <- names$clean[pick]
  # a categorical column whose levels are the platforms' names records the platform
  categorical <- .clinical_rows(profile$categorical, table$table, source = source)
  for (name in columns$column[columns$role == "categorical" & columns$class == "character"]) {
    levels <- unique(categorical$level[categorical$column == name & categorical$stratum_var == "all"])
    at <- match(tolower(names$platform[pick]), tolower(levels))
    if (length(levels) > 0L && !anyNA(at)) data[[name]] <- levels[at]
  }
  data
}

# A long table: the sample columns repeated for each feature, then the
# row-level columns. Row-level columns with no more distinct values than
# there are features describe the feature (one value per feature); the rest
# vary by row. The feature and value columns come from the platforms.
.clinical_omics_long <- function(profile, source, platforms, measure, samples) {
  lead <- platforms[1L, , drop = FALSE]
  n_samples <- nrow(measure$u)
  features <- .clinical_omics_features(profile, lead)$feature
  n <- n_samples * length(features)
  sample_index <- rep(seq_len(n_samples), each = length(features))
  feature_index <- rep(seq_along(features), times = n_samples)
  out <- samples[sample_index, , drop = FALSE]
  base <- sub(":.*", "", lead$platform)
  long_name <- c(paste0(base, "/long_rows"), paste0(sub(":.*", "", base), "/long_rows"))
  long_name <- long_name[long_name %in% profile$tables$table[profile$tables$source == source]][1L]
  if (is.na(long_name) && grepl(":", lead$platform)) {
    stacks <- profile$tables$table[profile$tables$source == source & endsWith(profile$tables$table, "/long_rows")]
    long_name <- stacks[startsWith(lead$platform, sub("/long_rows$", "", stacks))][1L]
  }
  if (!is.na(long_name)) {
    columns <- .clinical_rows(profile$columns, long_name, source = source)
    columns <- columns[order(columns$position), , drop = FALSE]
    table <- data.frame(table = long_name, profile_table = long_name, class = "data.frame",
                        n_rows = length(features), stringsAsFactors = FALSE)
    given <- c(lead$feature_column, platforms$value_column)
    describes_feature <- !is.na(columns$n_distinct) & columns$n_distinct <= length(features)
    per_feature <- .clinical_table(profile, table, NULL, NULL, source = source, n = length(features),
                                   only = setdiff(columns$column[describes_feature], given))
    per_row <- .clinical_table(profile, table, NULL, NULL, source = source, n = n,
                               only = setdiff(columns$column[!describes_feature], given))
    for (name in setdiff(columns$column, given)) {
      out[[name]] <- if (name %in% names(per_feature)) per_feature[[name]][feature_index] else per_row[[name]]
    }
  }
  if (!is.na(lead$feature_column)) out[[lead$feature_column]] <- features[feature_index]
  for (i in seq_len(nrow(platforms))) {
    platform <- platforms[i, , drop = FALSE]
    if (is.na(platform$value_column)) next
    values <- .clinical_omics_values(profile, platform, measure)
    out[[platform$value_column]] <- as.vector(t(values))
    if ("split" %in% names(platform) && !is.na(platform$split)) {
      split <- strsplit(platform$split, " = ", fixed = TRUE)[[1L]]
      if (length(split) == 2L) out[[split[[1L]]]] <- split[[2L]]
    }
  }
  out
}

# Unique sample IDs for a group, from the sample column's shapes
.clinical_omics_sample_ids <- function(profile, source, platform, n) {
  column <- platform$sample_column
  shapes <- profile$id_shapes[profile$id_shapes$source == source & profile$id_shapes$column %in% column, , drop = FALSE]
  shapes <- shapes[shapes$stratum_var == "all", , drop = FALSE]
  value <- .clinical_draw_shapes(shapes[!duplicated(shapes$level), , drop = FALSE], n, unique = TRUE)
  if (is.null(value)) value <- sprintf("SYN-%s-%06d", toupper(gsub("[^A-Za-z0-9]+", "", platform$platform)), seq_len(n))
  value
}

# Bind tables whose columns differ, filling absent columns with NA
.clinical_align <- function(parts) {
  columns <- unique(unlist(lapply(parts, names)))
  lapply(parts, function(part) {
    for (name in setdiff(columns, names(part))) part[[name]] <- rep(NA, nrow(part))
    part[columns]
  })
}

# A tximport list (abundance, counts, length, countsFromAbundance) with the
# profiled genes as rows and synthetic samples as columns. Abundance follows
# the profile (TPM); counts add a library size per sample (32_omics_samples.csv)
# and a length per gene.
.clinical_tximport <- function(profile, platform, structure, measure) {
  abundance <- t(.clinical_omics_values(profile, platform, measure))
  abundance[is.na(abundance)] <- 0
  n_samples <- ncol(abundance)
  colnames(abundance) <- .clinical_omics_sample_ids(profile, platform$source, platform, n_samples)
  gene_length <- stats::rlnorm(nrow(abundance), log(1500), 0.6)
  length <- gene_length * matrix(stats::runif(length(abundance), 0.95, 1.05), nrow(abundance))
  dimnames(length) <- dimnames(abundance)
  stats_rows <- profile$omics_samples[profile$omics_samples$source == platform$source &
                                        profile$omics_samples$platform == platform$platform &
                                        profile$omics_samples$stat == "library_size", , drop = FALSE]
  quantile <- if (nrow(stats_rows) > 0L) {
    .clinical_quantile_fun(unlist(stats_rows[1L, .clinical_quantile_columns], use.names = FALSE))
  }
  library_size <- if (is.null(quantile)) rep(2e7, n_samples) else quantile(stats::runif(n_samples))
  weight <- abundance * length
  counts <- round(sweep(weight, 2L, colSums(weight), "/") * rep(library_size, each = nrow(weight)))
  counts[!is.finite(counts)] <- 0
  out <- list(abundance = abundance, counts = counts, length = length, countsFromAbundance = "no")
  out[intersect(structure$path, names(out))]
}

# A person-level table of an omics source (such as genotype carriers): its
# people are those with the matching availability flag
.clinical_omics_person_table <- function(profile, source, table, structure, people, scale, ids) {
  wanted <- round(table$n_persons * scale)
  flags <- attr(people, "flags")
  pool <- which(people$registrant)
  flag <- .clinical_omics_flag(table$table, "none", flags, wanted)
  if (!is.na(flag) && sum(flags[, flag]) < 0.9 * wanted) flag <- NA_character_
  if (!is.na(flag)) pool <- intersect(pool, which(flags[, flag]))
  rows <- sort(pool[sample.int(length(pool), min(wanted, length(pool)))])
  table$profile_table <- table$table
  table$class <- if (grepl("data.frame", structure$class, fixed = TRUE)) structure$class else "data.frame"
  .clinical_table(profile, table, people, rows, ids = .clinical_ids_for(profile, source, table$table, ids),
                  source = source)
}

# Person-level identifiers for a table's identifier columns that share a name
# (ignoring case) with one in the wrangled list
.clinical_ids_for <- function(profile, source, table, ids) {
  columns <- .clinical_rows(profile$columns, table, source = source)
  columns <- columns[columns$role == "identifier", , drop = FALSE]
  out <- list()
  for (name in columns$column) {
    if (!is.null(ids[[tolower(name)]])) out[[name]] <- ids[[tolower(name)]]
  }
  out
}
