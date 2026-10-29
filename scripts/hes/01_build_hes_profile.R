# 01_build_hes_profile.R -----------------------------------------------------
# Build inst/extdata/hes-profile.rds, the profile behind synthetic react_hes().
#
# Inputs (provenance in scripts/hes/README.md):
# - scripts/hes/sources/: public NHS England files under the Open Government
#   Licence v3 (fetch with scripts/hes/00_download_sources.sh): the artificial
#   HES APC and OP samples (Feb 2023, v1) and the HES 2023-24 diagnosis and
#   procedure open data.
# - REACT_HES_AGGREGATES: the folder holding the schema profile of REACT's
#   linked tables that the dementia project returned from the enclave on 22 Oct
#   2026 (counts 1-9 suppressed, others rounded to 5). These files stay local
#   and are never committed; the profile records their checksums.
# - This package's synthetic REACT participants, used only as the reference
#   population when calibrating admission rates.
#
# Run from the package root, without the renv profile:
#   REACT_HES_AGGREGATES=/path/to/schema_profile \
#     Rscript --no-init-file scripts/hes/01_build_hes_profile.R
# Then pin the printed SHA-256 in R/hes_profile.R and record approval in
# inst/extdata/hes-profile-approval.csv.

rm(list = ls(all = TRUE))
set.seed(20261029)

src_dir <- "scripts/hes/sources"
agg_dir <- Sys.getenv("REACT_HES_AGGREGATES")
if (!nzchar(agg_dir) || !dir.exists(agg_dir)) {
  stop("Set REACT_HES_AGGREGATES to the folder of REACT schema-profile CSVs.", call. = FALSE)
}
out_path <- "inst/extdata/hes-profile.rds"

table_ids <- c(apc = "hes_inpatient", op = "hes_outpatient")
table_names <- c(apc = "NHSE_HES_ADMITTED_PATIENTS", op = "NHSE_HES_OUTPATIENTS")

# ages are anchored to this date when people do not supply their own
reference_date <- as.Date("2021-04-01")


# 1. Helpers -------------------------------------------------------------------

read_react <- function(name) {
  utils::read.csv(file.path(agg_dir, paste0(name, ".csv")), colClasses = "character",
                  na.strings = character(), check.names = FALSE)
}

as_num <- function(x) suppressWarnings(as.numeric(x))

# suppressed REACT counts (1-9) are blank; they count as zero and are never
# reconstructed
react_count <- function(x) {
  n <- as_num(x)
  n[is.na(n)] <- 0
  n
}

is_hidden_group <- function(x) grepl("^<[0-9]+ other value", x)

read_artificial <- function(zip) {
  files <- grep("[.]csv$", utils::unzip(zip, list = TRUE)$Name, value = TRUE)
  do.call(rbind, lapply(files, function(f) {
    utils::read.csv(unz(zip, f), colClasses = "character", na.strings = character(),
                    check.names = FALSE)
  }))
}


# 2. REACT schema, volumes and coverage ---------------------------------------

dict <- read_react("dictionary_columns")
schema <- lapply(table_ids, function(id) {
  d <- dict[dict$source_id == id, ]
  d <- d[order(as_num(d$column_id)), ]
  data.frame(column = d$column_name, type = d$data_type, stringsAsFactors = FALSE)
})

# optimiser statistics: approximate, but the only whole-table volumes available
tables <- read_react("dictionary_tables")
rows_total <- as_num(tables$num_rows_stats[match(table_ids, tables$source_id)])
names(rows_total) <- names(table_ids)
subjects_total <- vapply(table_ids, function(id) {
  as_num(dict$num_distinct_stats[dict$source_id == id & dict$column_name == "SUBJECT_ID"])
}, numeric(1))
# people with any GDPPR record approximate the linked REACT population
linked_n <- as_num(dict$num_distinct_stats[dict$source_id == "gp" & dict$column_name == "SUBJECT_ID"])

dates <- read_react("date_distributions")
date_counts <- function(id, column) {
  x <- dates[dates$source_id == id & dates$scope == "block_sample_1pct" &
               dates$column_name == column & grepl("^[0-9]{4}", dates$period), ]
  data.frame(period = x$period, n = react_count(x$n), stringsAsFactors = FALSE)
}
main_date <- c(apc = "EPISTART", op = "APPTDATE")
counts_by_period <- Map(date_counts, table_ids, main_date)

# HES coverage in REACT: April 2015 to March 2023 (financial years 2015-16 to
# 2022-23). The 2015 and 2023 calendar-year counts match those part years.
window <- c(start = as.Date("2015-04-01"), end = as.Date("2023-03-31"))
months <- seq(window[["start"]], window[["end"]], by = "month")
month_mid <- months + 14


# 3. REACT column-level aggregates --------------------------------------------

values <- read_react("value_frequencies")
summary_cols <- read_react("column_summary")
patterns <- read_react("value_patterns")
quantiles <- read_react("numeric_quantiles")

react_values <- function(id, column) {
  x <- values[values$source_id == id & values$scope == "block_sample_1pct" &
                values$column_name == column & values$year == "ALL" &
                !is_hidden_group(values$value), ]
  n <- react_count(x$n)
  keep <- n > 0
  if (!any(keep)) return(NULL)
  v <- x$value[keep]
  v[v == "<NULL>"] <- NA
  list(values = v, prob = n[keep] / sum(n[keep]))
}

react_completeness <- function(id, column) {
  x <- summary_cols[summary_cols$source_id == id & summary_cols$scope == "block_sample_1pct" &
                      summary_cols$column_name == column, ]
  if (nrow(x) == 0) return(NA_real_)
  as_num(x$pct_nonnull[1]) / 100
}

react_patterns <- function(id, column) {
  x <- patterns[patterns$source_id == id & patterns$scope == "block_sample_1pct" &
                  patterns$column_name == column & !is_hidden_group(patterns$pattern) &
                  patterns$pattern != "<NULL>", ]
  n <- react_count(x$n)
  keep <- n > 0
  if (!any(keep)) return(NULL)
  list(values = x$pattern[keep], prob = n[keep] / sum(n[keep]))
}

react_quantiles <- function(id, column) {
  x <- quantiles[quantiles$source_id == id & quantiles$scope == "block_sample_1pct" &
                   quantiles$column_name == column, ]
  if (nrow(x) == 0) return(NULL)
  as_num(unlist(x[1, c("p05", "p25", "p50", "p75", "p95")]))
}


# 4. Artificial HES value distributions ----------------------------------------

# keep financial years that overlap REACT's HES coverage (2015-16 onwards);
# earlier years lack fields such as the CCG codes
artificial <- list(
  apc = read_artificial(file.path(src_dir, "artificial_hes_apc_202302_v1_sample.zip")),
  op = read_artificial(file.path(src_dir, "artificial_hes_op_202302_v1_sample.zip"))
)
artificial <- lapply(artificial, function(d) d[d$FYEAR >= "1516", , drop = FALSE])

artificial_values <- function(d, column, max_values = 5000L) {
  x <- d[[column]]
  x <- x[x != ""]
  if (length(x) == 0) return(NULL)
  tab <- sort(table(x), decreasing = TRUE)
  tab <- utils::head(tab, max_values)
  list(values = names(tab), prob = as.numeric(tab) / sum(tab),
       completeness = length(x) / length(d[[column]]))
}


# 5. Fill rules for columns the generator does not model directly --------------

# columns that stay the same for every record of a person or of a spell/course
person_columns <- c("LSOA11", "CCG_RESIDENCE", "CCG_GP_PRACTICE", "CCG_RESPONSIBILITY",
                    "CCG_RESPONSIBILITY_ORIGIN", "CURRWARD_ONS", "MARSTAT", "CARERSI")
group_columns <- c("PROCODE3", "PROCODE5", "PROCODET", "SITETRET", "PROTYPE",
                   "CCG_TREATMENT", "CCG_TREATMENT_ORIGIN", "MAINSPEF", "TRETSPEF")

fill_rule <- function(table, column, type) {
  id <- table_ids[[table]]
  level <- if (column %in% person_columns) "person" else if (column %in% group_columns) "group" else "row"
  completeness <- react_completeness(id, column)
  rule <- list(level = level, completeness = completeness)

  rv <- react_values(id, column)
  if (!is.null(rv) && any(!is.na(rv$values))) {
    return(c(rule, method = "values", source = "react", rv))
  }
  av <- if (column %in% names(artificial[[table]])) artificial_values(artificial[[table]], column)
  if (!is.null(av)) {
    if (is.na(completeness)) rule$completeness <- av$completeness
    return(c(rule, method = "values", source = "artificial", av[c("values", "prob")]))
  }
  q <- react_quantiles(id, column)
  if (!is.null(q)) return(c(rule, method = "quantiles", source = "react", list(q = q)))
  rp <- react_patterns(id, column)
  if (!is.null(rp)) return(c(rule, method = "pattern", source = "react", rp))
  c(rule, method = "missing", source = "none")
}

fill_rules <- lapply(names(table_ids), function(table) {
  s <- schema[[table]]
  rules <- Map(function(column, type) fill_rule(table, column, type), s$column, s$type)
  names(rules) <- s$column
  rules
})
names(fill_rules) <- names(table_ids)


# 6. National diagnosis and procedure tables (HES 2023-24) ---------------------

age_bands <- c(AGE_0_SUM = 0, AGE_1_4_SUM = 1, AGE_5_9_SUM = 5, AGE_10_14_SUM = 10,
               AGE_15_SUM = 15, AGE_16_SUM = 16, AGE_17_SUM = 17, AGE_18_SUM = 18,
               AGE_19_SUM = 19, AGE_20_24_SUM = 20, AGE_25_29_SUM = 25, AGE_30_34_SUM = 30,
               AGE_35_39_SUM = 35, AGE_40_44_SUM = 40, AGE_45_49_SUM = 45, AGE_50_54_SUM = 50,
               AGE_55_59_SUM = 55, AGE_60_64_SUM = 60, AGE_65_69_SUM = 65, AGE_70_74_SUM = 70,
               AGE_75_79_SUM = 75, AGE_80_84_SUM = 80, AGE_85_89_SUM = 85, AGE_90_120_SUM = 90)

read_publication <- function(path, categories) {
  d <- utils::read.csv(path, colClasses = "character", na.strings = character())
  d <- d[d$CATEGORY %in% categories, ]
  value <- as_num(d$MEASURE_VALUE)
  value[d$MEASURE_VALUE == "*"] <- 3  # NHS England suppresses 1-7 as "*"
  value[is.na(value)] <- 0
  m <- tapply(value, list(d$CODE, d$MEASURE), function(x) x[1])
  m[is.na(m)] <- 0
  m
}

# codes x (age bands, sex, admission type, day case, length of stay)
code_table <- function(m) {
  codes <- rownames(m)
  age <- m[, names(age_bands), drop = FALSE]
  colnames(age) <- age_bands
  storage.mode(age) <- "double"
  sexed <- m[, "FCE_MALE_SUM"] + m[, "FCE_FEMALE_SUM"]
  admission <- m[, c("FAE_EMERGENCY_SUM", "FAE_WAITLIST_SUM", "FAE_PLANNED_SUM", "FAE_OTHER_SUM")]
  colnames(admission) <- c("emergency", "waitlist", "planned", "other")
  fce <- m[, "FCE_SUM"]
  list(
    codes = codes,
    age = age,
    female_share = ifelse(sexed > 0, m[, "FCE_FEMALE_SUM"] / sexed, NA_real_),
    admission = admission,
    day_case_share = ifelse(fce > 0, pmin(1, m[, "FCE_DAY_CASES"] / fce), 0),
    spell_days_mean = m[, "SPELDUR_CALC_MEAN"],
    fce = fce
  )
}

diag_m <- read_publication(file.path(src_dir, "hosp-epis-stat-admi-diag-2023-24.csv"), "DIAG_4_01")
diagnosis <- code_table(diag_m)

proc_m4 <- read_publication(file.path(src_dir, "hosp-epis-stat-admi-proc-2023-24.csv"), "OPERTN_4_01")
proc_m3 <- read_publication(file.path(src_dir, "hosp-epis-stat-admi-proc-2023-24.csv"), "OPERTN_3_01")
# "-" (no procedure) appears only at 3-character level; add it to the 4-character table
proc_m <- rbind(proc_m4, proc_m3["-", colnames(proc_m4), drop = FALSE])
procedure <- code_table(proc_m)

# secondary diagnoses: national frequencies from the artificial data, positions 2-12
secondary_columns <- sprintf("DIAG_4_%02d", 2:12)
secondary_codes <- unlist(artificial$apc[secondary_columns], use.names = FALSE)
secondary_codes <- secondary_codes[secondary_codes != "" & secondary_codes != "-" &
                                     grepl("^[A-Z][0-9]{2}[0-9A-Z]$", secondary_codes)]
secondary_tab <- sort(table(secondary_codes), decreasing = TRUE)
secondary <- list(codes = names(secondary_tab), prob = as.numeric(secondary_tab) / sum(secondary_tab))

# outpatient diagnoses and procedures, used for the minority of appointments
# that record them
op_procedures <- local({
  v <- artificial_values(artificial$op, "OPERTN_4_01")
  keep <- grepl("^[A-Z][0-9]{3}$", v$values)
  list(values = v$values[keep], prob = v$prob[keep] / sum(v$prob[keep]))
})


# 7. Reference population and admission-rate calibration -----------------------

suppressMessages(pkgload::load_all(".", quiet = TRUE))
ref_source <- react_synthetic(n_per_round = 2000L, seed = 20261029L)
ref_result <- react_extract(ref_source, families = "people-households/demographics",
                            progress = FALSE)
# gender is coded differently across rounds ("Female", "female", "2", ...)
ref_gender <- toupper(trimws(ref_result$data[["people_households.demographics.gender"]]))
ref_people <- data.frame(
  age = ref_result$data[["people_households.demographics.age__field__U_AGE"]],
  female = ifelse(ref_gender %in% c("FEMALE", "F", "2"), 1,
                  ifelse(ref_gender %in% c("MALE", "M", "1"), 0, NA)),
  stringsAsFactors = FALSE
)
ref_people <- ref_people[!is.na(ref_people$age) & !is.na(ref_people$female), ]

# one row per age and sex; ref_n weights each row by its share of people
ref <- stats::aggregate(list(n = rep(1, nrow(ref_people))), ref_people, sum)
ref_n <- ref$n / sum(ref$n)
ref_female <- ref$female

# age at the middle of each month (NA before birth); U_AGE is completed years
ref_age <- outer(ref$age + 0.5, as.numeric(month_mid - reference_date) / 365.25, "+")
ref_age[ref_age < 0] <- NA

rate_eta <- function(p, age, female) {
  z <- (age - 50) / 20
  p[["b0"]] + p[["b1"]] * z + p[["b2"]] * z^2 + p[["b3"]] * z^3 +
    p[["bf"]] * female + p[["bm"]] * female * (age >= 18 & age < 45)
}

# expected events per person-month, without frailty
rate_matrix <- function(p, period) {
  lam <- exp(rate_eta(p, ref_age, ref_female)) * rep(period, each = nrow(ref_age)) / 12
  lam[is.na(lam)] <- 0
  lam
}

# share of events before each age, weighting each row by its people
weighted_cdf <- function(lam, at) {
  a <- as.vector(ref_age)
  w <- as.vector(lam * ref_n)
  keep <- !is.na(a)
  vapply(at, function(q) sum(w[keep][a[keep] < q]) / sum(w[keep]), numeric(1))
}

female_event_share <- function(lam) {
  sum((lam * ref_n)[ref_female == 1, ]) / sum(lam * ref_n)
}

fit_rates <- function(target, period, fixed = list()) {
  free <- setdiff(c("b0", "b1", "b2", "b3", "bf", "bm", "log_k"), names(fixed))
  start <- c(b0 = log(target$mean_events / 8), b1 = 0.5, b2 = 0, b3 = 0, bf = 0.1, bm = 0.3,
             log_k = 0)[free]
  loss <- function(par) {
    p <- c(as.list(par), fixed)
    lam <- rate_matrix(p, period)
    per_person <- rowSums(lam)
    k <- exp(p[["log_k"]])
    l <- log(sum(ref_n * per_person) / target$mean_events)^2 +
      (sum(ref_n * (1 - (1 + per_person / k)^(-k))) - target$p_any)^2 +
      sum((weighted_cdf(lam, target$age_q + 0.5) - c(0.05, 0.25, 0.5, 0.75, 0.95))^2)
    if (!is.null(target$female_share)) {
      l <- l + (female_event_share(lam) - target$female_share)^2
    }
    l
  }
  fit <- stats::optim(start, loss, method = "Nelder-Mead", control = list(maxit = 4000, reltol = 1e-12))
  fit <- stats::optim(fit$par, loss, method = "BFGS", control = list(maxit = 500))
  c(as.list(fit$par), fixed)
}

# yearly calendar factors from REACT's block sample, after removing the effect
# of the cohort ageing; 2020 also keeps its monthly shape (the first COVID-19
# wave). Finer monthly detail in the block sample reflects how blocks were
# sampled rather than real change, so it is not used.
period_factors <- function(counts, lam) {
  month_year <- format(months, "%Y")
  expected_month <- colSums(lam * ref_n)
  observed_year <- tapply(counts$n[nchar(counts$period) == 4],
                          counts$period[nchar(counts$period) == 4], sum)
  monthly <- counts[nchar(counts$period) == 7, ]
  observed_year <- c(observed_year, tapply(monthly$n, substr(monthly$period, 1, 4), sum))
  # part years: April-December 2015 and January-March 2023
  expected_year <- tapply(expected_month, month_year, sum)
  ratio <- observed_year[names(expected_year)] / expected_year
  f <- unname(ratio[month_year])
  in_2020 <- month_year == "2020"
  m2020 <- monthly$n[match(format(months[in_2020], "%Y-%m"), monthly$period)]
  f[in_2020] <- f[in_2020] * (m2020 / mean(m2020)) / (expected_month[in_2020] / mean(expected_month[in_2020]))
  f / stats::weighted.mean(f, expected_month)
}

episodes_per_spell <- local({
  v <- react_values(table_ids[["apc"]], "EPIORDER")
  order_n <- as_num(v$values)
  keep <- !is.na(order_n) & order_n <= 10
  share <- tapply(v$prob[keep], order_n[keep], sum)
  # P(spell has at least k episodes) is proportional to the share with EPIORDER = k
  at_least <- share / share[["1"]]
  exactly <- at_least - c(at_least[-1], 0)
  list(n = as.integer(names(exactly)), prob = pmax(exactly, 0) / sum(pmax(exactly, 0)))
})
mean_episodes_per_spell <- sum(episodes_per_spell$n * episodes_per_spell$prob)

first_attendance_share <- local({
  v <- react_values(table_ids[["op"]], "FIRSTATT")
  sum(v$prob[v$values %in% c("1", "3")]) / sum(v$prob[v$values %in% c("1", "2", "3", "4")])
})
appointments_per_course <- 1 / first_attendance_share

sex_values <- react_values(table_ids[["apc"]], "SEX")
apc_target <- list(
  mean_events = rows_total[["apc"]] / linked_n / mean_episodes_per_spell,
  p_any = subjects_total[["apc"]] / linked_n,
  age_q = react_quantiles(table_ids[["apc"]], "STARTAGE"),
  female_share = sex_values$prob[sex_values$values == "2"] / sum(sex_values$prob[sex_values$values %in% c("1", "2")])
)
op_target <- list(
  mean_events = rows_total[["op"]] / linked_n / appointments_per_course,
  p_any = subjects_total[["op"]] / linked_n,
  age_q = react_quantiles(table_ids[["op"]], "ACTIVAGE")
)

calibrate <- function(target, counts, fixed = list()) {
  period <- rep(1, length(months))
  for (i in 1:3) {
    p <- fit_rates(target, period, fixed)
    period <- period_factors(counts, rate_matrix(p, rep(1, length(months))))
  }
  p <- fit_rates(target, period, fixed)
  list(par = unlist(p), period = period)
}

message("Calibrating admitted patient care rates")
apc_rates <- calibrate(apc_target, counts_by_period$apc)
message("Calibrating outpatient rates")
# REACT's outpatient profile has no sex breakdown; reuse the admitted-care sex terms
op_rates <- calibrate(op_target, counts_by_period$op,
                      fixed = list(bf = apc_rates$par[["bf"]], bm = apc_rates$par[["bm"]]))

rate_fit_check <- function(rates, target, per_event) {
  lam <- rate_matrix(as.list(rates$par), rates$period)
  per_person <- rowSums(lam)
  k <- exp(rates$par[["log_k"]])
  cdf <- weighted_cdf(lam, 0:110 + 0.5)
  data.frame(
    measure = c("records per person", "share with any record", "age p05", "age p25",
                "age p50", "age p75", "age p95", "female share of records"),
    target = c(target$mean_events * per_event, target$p_any, target$age_q,
               if (is.null(target$female_share)) NA else target$female_share),
    fitted = c(sum(ref_n * per_person) * per_event, sum(ref_n * (1 - (1 + per_person / k)^(-k))),
               vapply(c(0.05, 0.25, 0.5, 0.75, 0.95), function(p) (0:110)[which(cdf >= p)[1]],
                      numeric(1)),
               female_event_share(lam))
  )
}
calibration <- list(
  apc = rate_fit_check(apc_rates, apc_target, mean_episodes_per_spell),
  op = rate_fit_check(op_rates, op_target, appointments_per_course)
)
print(calibration)


# 8. COVID-19 admissions ------------------------------------------------------

# REACT's monthly counts of admitted-care rows with U07.1 or U07.2 in any position
covid_period <- read_react("hes_code_by_period")
covid_period <- covid_period[covid_period$source_id == "hes_inpatient" &
                               covid_period$code4 %in% c("U071", "U072") &
                               grepl("^[0-9]{4}-[0-9]{2}$", covid_period$period), ]
covid_period$n <- react_count(covid_period$n)
covid_month <- tapply(covid_period$n, covid_period$period, sum)
covid_rows <- unname(covid_month[format(months, "%Y-%m")])
covid_rows[is.na(covid_rows)] <- 0

# national age profile of COVID-19 relative to all admissions (HES 2023-24)
covid_codes <- intersect(c("U071", "U072"), diagnosis$codes)
covid_age <- colSums(diagnosis$age[covid_codes, , drop = FALSE])
all_age <- colSums(diagnosis$age)
covid_relative <- (covid_age / sum(covid_age)) / (all_age / sum(all_age))

age_band_index <- function(age) findInterval(age, age_bands)

# scale per month so the reference population reproduces REACT's monthly counts
covid_scale <- local({
  apc_lam <- rate_matrix(as.list(apc_rates$par), rep(1, length(months)))
  rel <- matrix(covid_relative[age_band_index(ref_age)], nrow = nrow(ref_age))
  rel[is.na(rel)] <- 0
  per_person_rate <- colSums(apc_lam * rel * ref_n)
  ifelse(per_person_rate > 0, (covid_rows / linked_n) / per_person_rate, 0)
})

covid_positions <- read_react("hes_code_positions")
covid_positions <- covid_positions[covid_positions$source_id == "hes_inpatient" &
                                     covid_positions$code4 %in% c("U071", "U072"), ]
covid_positions$n <- react_count(covid_positions$n)
covid <- list(
  scale = covid_scale,
  relative_by_band = unname(covid_relative),
  code_share = tapply(covid_positions$n, covid_positions$code4, sum) / sum(covid_positions$n),
  primary_share = sum(covid_positions$n[covid_positions$position == "01"]) / sum(covid_positions$n),
  secondary_position = local({
    s <- covid_positions[covid_positions$position != "01", ]
    p <- tapply(s$n, as.integer(s$position), sum)
    list(position = as.integer(names(p)), prob = as.numeric(p) / sum(p))
  })
)


# 9. Day cases, procedures and in-hospital deaths -----------------------------

# episode mix of the reference population by age band and sex
episode_mix <- local({
  lam <- rate_matrix(as.list(apc_rates$par), apc_rates$period)
  band <- age_band_index(as.vector(ref_age))
  female <- rep(ref_female, times = ncol(ref_age))
  w <- as.vector(lam * ref_n)
  keep <- !is.na(band)
  m <- tapply(w[keep], list(band[keep], female[keep]), sum)
  m[is.na(m)] <- 0
  m / sum(m)
})

code_prob <- function(tab, band, female, exclude = character()) {
  p <- tab$age[, band]
  sex_tilt <- ifelse(is.na(tab$female_share), 1,
                     if (female == 1) tab$female_share / 0.5 else (1 - tab$female_share) / 0.5)
  p <- p * sex_tilt
  p[tab$codes %in% exclude] <- 0
  p / sum(p)
}

admission_share <- function(tab) {
  a <- tab$admission
  total <- rowSums(a)
  overall <- colSums(a) / sum(a)
  a <- a / ifelse(total > 0, total, 1)
  a[total == 0, ] <- rep(overall, each = sum(total == 0))
  a
}
diagnosis$admission_share <- admission_share(diagnosis)
procedure$admission_share <- admission_share(procedure)

# U codes (COVID-19 and other emergency-use codes) come only from the COVID-19
# process, which follows REACT's monthly counts
u_codes <- diagnosis$codes[grepl("^U", diagnosis$codes)]

# REACT's mix of admission types differs from England's (more elective care);
# scale each type's share so the reference population reproduces it
admission_target <- local({
  v <- react_values(table_ids[["apc"]], "ADMIMETH")
  groups <- list(emergency = c("21", "22", "23", "24", "25", "28", "2A", "2B", "2C", "2D"),
                 waitlist = "11", planned = c("12", "13"),
                 other = c("31", "32", "81", "82", "83", "84", "89", "98", "99"))
  t <- vapply(groups, function(g) sum(v$prob[v$values %in% g]), numeric(1))
  t / sum(t)
})
expected_admission <- function(share) {
  total <- 0
  for (b in seq_len(nrow(episode_mix))) for (s in 1:2) {
    w <- episode_mix[b, s]
    if (w == 0) next
    p <- code_prob(diagnosis, as.integer(rownames(episode_mix)[b]), s - 1, exclude = u_codes)
    total <- total + w * colSums(share * p)
  }
  total
}
admission_multiplier <- rep(1, 4)
for (i in 1:30) {
  share <- sweep(diagnosis$admission_share, 2, admission_multiplier, "*")
  share <- share / rowSums(share)
  admission_multiplier <- admission_multiplier * admission_target / expected_admission(share)
}
diagnosis$admission_share <- share
print(rbind(target = admission_target, fitted = expected_admission(share)))

# day cases happen in elective admissions; convert the all-admission share
elective_share <- diagnosis$admission_share[, "waitlist"] + diagnosis$admission_share[, "planned"]
diagnosis$elective_day_case <- pmin(0.98, diagnosis$day_case_share / pmax(elective_share, 0.02))

expected_share <- function(f) {
  total <- 0
  for (b in seq_len(nrow(episode_mix))) for (s in 1:2) {
    w <- episode_mix[b, s]
    if (w == 0) next
    total <- total + w * f(as.integer(rownames(episode_mix)[b]), s - 1)
  }
  total
}

odds_shift <- function(p, m) {
  o <- p / (1 - p) * m
  ifelse(is.finite(o), o / (1 + o), 1)
}

# REACT target: CLASSPAT 2 (day case) share of episodes, converted to a share
# of spells (day cases are single-episode spells)
day_case_target <- local({
  v <- react_values(table_ids[["apc"]], "CLASSPAT")
  min(0.9, sum(v$prob[v$values == "2"]) * mean_episodes_per_spell)
})
day_case_multiplier <- stats::uniroot(function(log_m) {
  expected_share(function(band, female) {
    p <- code_prob(diagnosis, band, female, exclude = u_codes)
    sum(p * elective_share * odds_shift(diagnosis$elective_day_case, exp(log_m)))
  }) - day_case_target
}, c(-5, 5))$root

# REACT target: share of episodes with a procedure in the first position
procedure_target <- local({
  v <- react_patterns(table_ids[["apc"]], "OPERTN_01")
  sum(v$prob[v$values == "A999"])
})
no_procedure <- which(procedure$codes == "-")
procedure_multiplier <- stats::uniroot(function(log_m) {
  expected_share(function(band, female) {
    p <- code_prob(procedure, band, female)
    p_none <- p[no_procedure]
    1 - odds_shift(p_none, exp(-log_m))
  }) - procedure_target
}, c(-5, 5))$root

# in-hospital deaths: risk doubles about every eight years of age up to 95
# (assumption), scaled to REACT's share of episodes ending in death (DISMETH 4)
death_target <- local({
  v <- react_values(table_ids[["apc"]], "DISMETH")
  sum(v$prob[v$values == "4"])
})
death_slope <- log(2) / 8
death_base <- local({
  lam <- rate_matrix(as.list(apc_rates$par), apc_rates$period)
  a <- as.vector(ref_age)
  w <- as.vector(lam * ref_n)
  keep <- !is.na(a)
  death_target * mean_episodes_per_spell / stats::weighted.mean(exp(death_slope * (pmin(a[keep], 95) - 75)), w[keep])
})


# 10. Codes for admission method, source and discharge --------------------------

react_subset <- function(table, column, keep_values) {
  v <- react_values(table_ids[[table]], column)
  keep <- v$values %in% keep_values
  list(values = v$values[keep], prob = v$prob[keep] / sum(v$prob[keep]))
}
admission_method <- list(
  emergency = react_subset("apc", "ADMIMETH", c("21", "22", "23", "24", "25", "28", "2A", "2B", "2C", "2D")),
  waitlist = react_subset("apc", "ADMIMETH", "11"),
  planned = react_subset("apc", "ADMIMETH", c("12", "13")),
  other = react_subset("apc", "ADMIMETH", c("31", "32", "81", "82", "83", "84", "89", "98", "99"))
)
admission_source <- react_values(table_ids[["apc"]], "ADMISORC")
discharge_method <- react_subset("apc", "DISMETH", c("1", "2", "3", "5", "9"))
discharge_destination <- local({
  v <- react_values(table_ids[["apc"]], "DISDEST")
  keep <- !is.na(v$values) & !v$values %in% c("79", "98")
  list(values = v$values[keep], prob = v$prob[keep] / sum(v$prob[keep]))
})
elective_wait <- react_quantiles(table_ids[["apc"]], "ELECDUR")

# REACT records "no procedure" as "-" or as "-" padded to four characters
no_procedure_dash <- vapply(table_ids, function(id) {
  v <- react_patterns(id, "OPERTN_4_01")
  sum(v$prob[v$values == "-"]) / sum(v$prob[v$values %in% c("-", "-   ")])
}, numeric(1))
op_wait <- react_quantiles(table_ids[["op"]], "WAITDAYS")

ethnicity <- list(
  # self-reported REACT categories (reactextract labels) to HES ETHNOS codes
  map = c(
    "English / Welsh / Scottish / Northern Irish / British" = "A",
    "Irish" = "B", "Gypsy or Irish Traveller" = "C", "Any other White background" = "C",
    "White and Black Caribbean" = "D", "White and Black African" = "E", "White and Asian" = "F",
    "Any other Mixed / Multiple ethnic background" = "G",
    "Indian" = "H", "Pakistani" = "J", "Bangladeshi" = "K", "Chinese" = "R",
    "Any other Asian background" = "L", "Caribbean" = "M", "African" = "N",
    "Any other Black / African / Caribbean background" = "P",
    "Arab" = "S", "Any other ethnic group" = "S"
  ),
  # HES records "not stated" (Z) or "not known" (99) for some episodes regardless
  not_stated = local({
    v <- react_values(table_ids[["apc"]], "ETHNOS")
    c(Z = v$prob[v$values == "Z"], `99` = v$prob[v$values == "99"])
  }),
  known = local({
    v <- react_values(table_ids[["apc"]], "ETHNOS")
    keep <- !v$values %in% c("Z", "99") & !is.na(v$values)
    list(values = v$values[keep], prob = v$prob[keep] / sum(v$prob[keep]))
  })
)


# 11. Save ---------------------------------------------------------------------

react_files <- file.path(agg_dir, paste0(c("dictionary_columns", "dictionary_tables", "date_distributions",
                                           "value_frequencies", "column_summary", "value_patterns",
                                           "numeric_quantiles", "hes_code_by_period", "hes_code_positions"),
                                         ".csv"))
input_files <- c(list.files(src_dir, full.names = TRUE), react_files)
profile <- list(
  version = "hes-profile-v1",
  built = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
  sources = data.frame(
    file = basename(input_files),
    origin = ifelse(input_files %in% react_files, "REACT schema profile (local only)", "NHS England (OGL v3)"),
    sha256 = vapply(input_files, .sha256_file, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  ),
  reference_date = reference_date,
  window = window,
  months = months,
  table_names = table_names,
  schema = schema,
  fill_rules = fill_rules,
  linked_n = linked_n,
  rates = list(apc = apc_rates, op = op_rates),
  calibration = calibration,
  episodes_per_spell = episodes_per_spell,
  appointments_per_course = appointments_per_course,
  frailty_correlation = 0.5,  # assumption: heavy users of one service use the other
  age_bands = unname(age_bands),
  diagnosis = diagnosis[c("codes", "age", "female_share", "admission_share", "elective_day_case",
                          "spell_days_mean", "fce")],
  procedure = procedure[c("codes", "age", "female_share", "admission_share", "fce")],
  secondary = secondary,
  op_procedures = op_procedures,
  covid = covid,
  covid_codes = covid_codes,
  admission_multiplier = stats::setNames(admission_multiplier, names(admission_target)),
  no_procedure_dash = no_procedure_dash,
  day_case_target = day_case_target,
  day_case_multiplier = exp(day_case_multiplier),
  procedure_multiplier = exp(procedure_multiplier),
  death = list(base = death_base, slope = death_slope),
  admission_method = admission_method,
  admission_source = admission_source,
  discharge_method = discharge_method,
  discharge_destination = discharge_destination,
  elective_wait = elective_wait,
  op_wait = op_wait,
  ethnicity = ethnicity
)

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(profile, out_path, compress = "xz")
message("Wrote ", out_path, " (", round(file.size(out_path) / 1e6, 1), " MB), SHA-256 ",
        .sha256_file(out_path))
