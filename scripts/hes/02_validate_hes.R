# 02_validate_hes.R ------------------------------------------------------------
# Compare synthetic HES generated for this package's synthetic participants
# with REACT's aggregate HES profile, and write scripts/hes/VALIDATION.md.
#
# Run from the package root after 01_build_hes_profile.R:
#   REACT_HES_AGGREGATES=/path/to/schema_profile \
#     Rscript --no-init-file scripts/hes/02_validate_hes.R

rm(list = ls(all = TRUE))
agg_dir <- Sys.getenv("REACT_HES_AGGREGATES")
if (!nzchar(agg_dir) || !dir.exists(agg_dir)) {
  stop("Set REACT_HES_AGGREGATES to the folder of REACT schema-profile CSVs.", call. = FALSE)
}
suppressMessages(pkgload::load_all(".", quiet = TRUE))

read_react <- function(name) {
  utils::read.csv(file.path(agg_dir, paste0(name, ".csv")), colClasses = "character",
                  na.strings = character(), check.names = FALSE)
}
as_num <- function(x) suppressWarnings(as.numeric(x))
values <- read_react("value_frequencies")
react_share <- function(id, column, codes) {
  x <- values[values$source_id == id & values$scope == "block_sample_1pct" &
                values$column_name == column & values$year == "ALL", ]
  sum(as_num(x$pct[x$value %in% codes]), na.rm = TRUE) / 100
}
quantiles <- read_react("numeric_quantiles")
react_q <- function(id, column) {
  x <- quantiles[quantiles$source_id == id & quantiles$scope == "block_sample_1pct" &
                   quantiles$column_name == column, ]
  as_num(unlist(x[1, c("p05", "p25", "p50", "p75", "p95")]))
}

profile <- .hes_profile()
src <- react_synthetic(n_per_round = 1000L, seed = 20261029L)
res <- react_extract(src, families = "people-households/demographics", progress = FALSE)
started <- Sys.time()
hes <- react_hes(src, res, seed = 1L)
elapsed <- as.numeric(Sys.time() - started, units = "secs")
apc <- hes$NHSE_HES_ADMITTED_PATIENTS
op <- hes$NHSE_HES_OUTPATIENTS
n <- length(unique(res$data$SUBJECT_ID))

linked <- profile$linked_n
tables <- read_react("dictionary_tables")
dict <- read_react("dictionary_columns")
react_rows <- function(id) as_num(tables$num_rows_stats[tables$source_id == id])
react_subjects <- function(id) as_num(dict$num_distinct_stats[dict$source_id == id & dict$column_name == "SUBJECT_ID"])

emergency <- c("21", "22", "23", "24", "25", "28", "2A", "2B", "2C", "2D")
rows <- list(
  c("APC", "Episodes per person", react_rows("hes_inpatient") / linked, nrow(apc) / n),
  c("APC", "Share of people with any episode", react_subjects("hes_inpatient") / linked,
    length(unique(apc$SUBJECT_ID)) / n),
  c("APC", "STARTAGE 5th/25th/50th/75th/95th percentiles",
    paste(react_q("hes_inpatient", "STARTAGE"), collapse = " / "),
    paste(stats::quantile(apc$STARTAGE[apc$STARTAGE < 7000], c(.05, .25, .5, .75, .95), type = 1),
          collapse = " / ")),
  c("APC", "Female share of episodes (SEX 2)", react_share("hes_inpatient", "SEX", "2"), mean(apc$SEX == "2")),
  c("APC", "Day cases (CLASSPAT 2)", react_share("hes_inpatient", "CLASSPAT", "2"), mean(apc$CLASSPAT == "2")),
  c("APC", "Emergency admissions (ADMIMETH 2x)", react_share("hes_inpatient", "ADMIMETH", emergency),
    mean(apc$ADMIMETH %in% emergency)),
  c("APC", "Waiting-list admissions (ADMIMETH 11)", react_share("hes_inpatient", "ADMIMETH", "11"),
    mean(apc$ADMIMETH == "11")),
  c("APC", "First episode of spell (EPIORDER 1)", react_share("hes_inpatient", "EPIORDER", "1"),
    mean(apc$EPIORDER == 1)),
  c("APC", "Spell not ended (SPELEND N)", react_share("hes_inpatient", "SPELEND", "N"), mean(apc$SPELEND == "N")),
  c("APC", "Died in hospital (DISMETH 4)", react_share("hes_inpatient", "DISMETH", "4"), mean(apc$DISMETH == "4")),
  c("APC", "Delivery episodes (EPITYPE 2)", react_share("hes_inpatient", "EPITYPE", "2"), mean(apc$EPITYPE == "2")),
  c("APC", "One diagnosis only (DIAG_COUNT 1)", react_share("hes_inpatient", "DIAG_COUNT", "1"),
    mean(apc$DIAG_COUNT == 1)),
  c("APC", "Ten or more diagnoses (DIAG_COUNT >= 10)",
    react_share("hes_inpatient", "DIAG_COUNT", as.character(10:30)), mean(apc$DIAG_COUNT >= 10)),
  c("APC", "Episodes with a procedure", 0.702, mean(apc$OPERTN_01 != "-")),
  c("APC", "Ethnicity White British (ETHNOS A)", react_share("hes_inpatient", "ETHNOS", "A"), mean(apc$ETHNOS == "A")),
  c("OP", "Appointments per person", react_rows("hes_outpatient") / linked, nrow(op) / n),
  c("OP", "Share of people with any appointment", react_subjects("hes_outpatient") / linked,
    length(unique(op$SUBJECT_ID)) / n),
  c("OP", "ACTIVAGE 5th/25th/50th/75th/95th percentiles",
    paste(react_q("hes_outpatient", "ACTIVAGE"), collapse = " / "),
    paste(stats::quantile(op$ACTIVAGE, c(.05, .25, .5, .75, .95), type = 1), collapse = " / ")),
  c("OP", "First attendances (FIRSTATT 1 or 3)", react_share("hes_outpatient", "FIRSTATT", c("1", "3")),
    mean(op$FIRSTATT %in% c("1", "3"))),
  c("OP", "No diagnosis recorded (NODIAGS 0)", react_share("hes_outpatient", "NODIAGS", "0"), mean(op$NODIAGS == 0)),
  c("OP", "No procedure recorded (NOPROCS 0)", react_share("hes_outpatient", "NOPROCS", "0"), mean(op$NOPROCS == 0))
)
fmt <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), x, ifelse(v < 1, sprintf("%.3f", v), sprintf("%.2f", v)))
}
table_md <- c("| Table | Measure | REACT | Synthetic |", "|---|---|---|---|",
              vapply(rows, function(r) sprintf("| %s | %s | %s | %s |", r[1], r[2], fmt(r[3]), fmt(r[4])),
                     character(1)))

covid <- read_react("hes_code_by_period")
covid <- covid[covid$source_id == "hes_inpatient" & covid$code4 %in% c("U071", "U072") &
                 grepl("^[0-9]{4}-[0-9]{2}$", covid$period), ]
covid_react <- tapply(as_num(covid$n), covid$period, sum, na.rm = TRUE) / linked * 1e5
synthetic_month <- format(apc$EPISTART[grepl("U07[12]", apc$DIAG_4_CONCAT)], "%Y-%m")
covid_synth <- table(synthetic_month) / n * 1e5
peaks <- c("2020-04", "2020-11", "2021-01", "2021-07", "2022-01", "2022-07", "2023-01")
covid_md <- c("| Month | REACT | Synthetic |", "|---|---|---|",
              sprintf("| %s | %.0f | %.0f |", peaks, covid_react[peaks],
                      ifelse(is.na(covid_synth[peaks]), 0, covid_synth[peaks])))

year_synth <- table(format(apc$EPISTART, "%Y")) / n * 1e3

report <- c(
  "# Synthetic HES validation",
  "",
  sprintf("Generated %s with reactextract %s (%s) for %s synthetic REACT participants (%s per round, seed 20261029), HES seed 1. Generation took %.0f seconds.",
          format(Sys.Date(), "%d %B %Y"), as.character(utils::packageVersion("reactextract")), profile$version,
          format(n, big.mark = ","), "1,000", elapsed),
  "",
  "REACT figures come from the disclosure-controlled schema profile of REACT's linked HES tables (22 October 2026): whole-table volumes are Oracle optimiser statistics, divided by the number of people with any GDPPR record; other figures are from a 1% block sample. Suppressed counts (1-9) count as zero.",
  "",
  table_md,
  "",
  "## COVID-19 episodes per 100,000 people (U07.1/U07.2, any position)",
  "",
  covid_md,
  "",
  "## Episodes per 1,000 people by calendar year",
  "",
  "2015 covers April to December and 2023 January to March.",
  "",
  paste0("| ", paste(names(year_synth), collapse = " | "), " |"),
  paste0("|", paste(rep("---", length(year_synth)), collapse = "|"), "|"),
  paste0("| ", paste(sprintf("%.0f", year_synth), collapse = " | "), " |"),
  ""
)
writeLines(report, "scripts/hes/VALIDATION.md")
cat(report, sep = "\n")
