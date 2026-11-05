# 03_disclosure_inventory.R -----------------------------------------------------
# List everything in the built clinical profile that would become public, for
# the approver's disclosure review: every category label set, ID shape, date
# month range and omics feature name set, with flags for labels that may name
# a person, place, clinic or site, a plate, batch, barcode or instrument, a
# free-text fragment or a date.
#
# The listing holds real labels, so it is written to a local folder that git
# ignores (dist/clinical-profile-review/) and is never committed.
# scripts/clinical/DISCLOSURE_REVIEW.md summarises it in words.
#
# Run from the package root after 01_build_clinical_profile.R:
#   Rscript --no-init-file scripts/clinical/03_disclosure_inventory.R

rm(list = ls(all = TRUE))
profile_path <- "inst/extdata/clinical-profile.rds"
out_dir <- "dist/clinical-profile-review"
profile <- readRDS(profile_path)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

standard <- c("yes", "no", "true", "false", "unknown", "not known", "don't know", "dont know",
              "prefer not to say", "not applicable", "none", "missing", "male", "female", "other",
              "never", "rarely", "sometimes", "often", "always", "very often", "occasionally",
              "strongly agree", "agree", "disagree", "strongly disagree", "neither agree nor disagree",
              "baseline", "followup", "follow-up", "t0", "t1", "(none)")
is_number <- function(x) grepl("^-?[0-9]+([.][0-9]+)?$", x)
flag_column <- function(name) {
  rules <- c(
    place = "site|clinic|location|centre|center|hospital|ward|venue|region|area|town|city|postcode|lsoa|msoa|address|country|county|borough",
    person = "staff|nurse|operator|interviewer|researcher|technician|user|initials|assessor|collector|signed",
    lab = "plate|batch|barcode|well|run|lot|box|rack|position|analy[sz]er|instrument|device|serial|machine|kit|freezer|shipment",
    version = "version|form|protocol|pis"
  )
  hits <- names(rules)[vapply(rules, function(r) grepl(r, name, ignore.case = TRUE), logical(1L))]
  paste(hits, collapse = "+")
}
flag_levels <- function(levels) {
  hits <- character()
  if (any(grepl("^[A-Z][a-z]+ [A-Z][a-z]+$", levels))) hits <- c(hits, "two_capitalised_words")
  if (any(grepl("hospital|clinic|centre|center|street|road|lane|surgery|practice|university|college",
                levels, ignore.case = TRUE))) hits <- c(hits, "place_word")
  if (any(nchar(levels) > 40)) hits <- c(hits, "long_text")
  if (any(grepl("[0-9]{1,4}[-/.][0-9]{1,2}[-/.][0-9]{1,4}|[0-9]{1,2}:[0-9]{2}", levels))) hits <- c(hits, "date_or_time")
  if (any(grepl("^[A-Za-z]*[0-9]{3,}[A-Za-z0-9-]*$", levels) & !is_number(levels))) hits <- c(hits, "id_like")
  paste(hits, collapse = "+")
}

# 1. Category label sets -----------------------------------------------------------
level_tables <- list(categorical = profile$categorical, availability_patterns = profile$availability_patterns)
rows <- list()
for (kind in names(level_tables)) {
  data <- level_tables[[kind]]
  data <- data[data$stratum_var == "all" & data$n_rows > 0, , drop = FALSE]
  for (key in unique(paste(data$source, data$table, data$column, sep = "\r"))) {
    part <- data[paste(data$source, data$table, data$column, sep = "\r") == key, , drop = FALSE]
    levels <- part$level[order(part$level_order)]
    coding <- if (all(is_number(levels))) "numeric codes" else
      if (all(tolower(levels) %in% standard | is_number(levels))) "standard words" else "other labels"
    rows[[length(rows) + 1L]] <- data.frame(
      file = kind, source = part$source[[1L]], table = part$table[[1L]], column = part$column[[1L]],
      n_levels = length(levels), coding = coding, column_flag = flag_column(part$column[[1L]]),
      level_flag = flag_levels(levels), max_level_chars = max(nchar(levels)),
      levels = paste(levels, collapse = " | "), stringsAsFactors = FALSE)
  }
}
labels <- do.call(rbind, rows)
utils::write.csv(labels, file.path(out_dir, "labels.csv"), row.names = FALSE)

# 2. ID shapes -----------------------------------------------------------------------
shapes <- profile$id_shapes[profile$id_shapes$stratum_var == "all" & profile$id_shapes$n_rows > 0, ]
shape_sets <- do.call(rbind, lapply(split(shapes, paste(shapes$source, shapes$table, shapes$column, sep = "\r")),
  function(part) data.frame(source = part$source[[1L]], table = part$table[[1L]], column = part$column[[1L]],
                            n_shapes = nrow(part), shapes = paste(part$level, collapse = " | "),
                            stringsAsFactors = FALSE)))
utils::write.csv(shape_sets, file.path(out_dir, "id_shapes.csv"), row.names = FALSE)

# 3. Date months: range per column, and months outside 2019-2026 (placeholders apart)
months <- profile$date_months[profile$date_months$stratum_var == "all" & profile$date_months$n_rows > 0, ]
placeholder <- months$level <= "1900-01" | startsWith(months$level, "9999")
date_sets <- do.call(rbind, lapply(split(months, paste(months$source, months$table, months$column, sep = "\r")),
  function(part) {
    real <- part$level[!(part$level <= "1900-01" | startsWith(part$level, "9999"))]
    data.frame(source = part$source[[1L]], table = part$table[[1L]], column = part$column[[1L]],
               n_months = length(real), first = if (length(real)) min(real) else NA,
               last = if (length(real)) max(real) else NA,
               placeholder_months = paste(part$level[part$level <= "1900-01" | startsWith(part$level, "9999")],
                                          collapse = " | "),
               outside_2019_2026 = sum(real < "2019-01" | real > "2026-12"), stringsAsFactors = FALSE)
  }))
utils::write.csv(date_sets, file.path(out_dir, "date_months.csv"), row.names = FALSE)

# 4. Omics feature names ---------------------------------------------------------------
features <- profile$omics_features
feature_sets <- do.call(rbind, lapply(split(features, paste(features$source, features$platform, sep = "\r")),
  function(part) data.frame(source = part$source[[1L]], platform = part$platform[[1L]], n_features = nrow(part),
                            examples = paste(utils::head(part$feature, 5), collapse = " | "),
                            stringsAsFactors = FALSE)))
utils::write.csv(feature_sets, file.path(out_dir, "omics_features.csv"), row.names = FALSE)

# 5. Coverage -------------------------------------------------------------------------
utils::write.csv(profile$structure, file.path(out_dir, "coverage.csv"), row.names = FALSE)

cat("Label sets:", nrow(labels), "\n")
print(table(labels$coding))
cat("Flagged by column name:", sum(nzchar(labels$column_flag)), " by level:", sum(nzchar(labels$level_flag)), "\n")
cat("ID shape sets:", nrow(shape_sets), " date columns:", nrow(date_sets),
    " with months outside 2019-2026:", sum(date_sets$outside_2019_2026 > 0), "\n")
cat("Written to", out_dir, "\n")
