# Writes clinical-candidate/: an invented Clinical Study inventory candidate in
# the file layout of reactextract's clinical inventory kit. Table paths, column
# names, counts and values are made up; nothing comes from REACT data. Counts
# follow the kit's rules (at least 10, multiples of 5, blank when suppressed),
# and anchored quantiles have only p10 to p90, as the kit writes them.
#
# Run from tests/testthat/fixtures: Rscript make-clinical-candidate.R

out <- "clinical-candidate"
unlink(out, recursive = TRUE)
dir.create(file.path(out, "inventory"), recursive = TRUE)
dir.create(file.path(out, "profile"), recursive = TRUE)
put <- function(data, file) {
  utils::write.csv(data, file.path(out, file), row.names = FALSE, na = "")
}
frame <- function(...) data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
qcols <- c("p01", "p05", "p10", "p25", "p50", "p75", "p90", "p95", "p99")
codes <- c("n_code_m77", "n_code_m91", "n_code_m92", "n_code_m99", "n_code_m66", "n_code_m555", "n_code_m999")

put(frame(source = "wrangled", group = "wrangled", reader = "rds_tree", status = "ok", message = "",
          size_mb = 1, modified = "2026-01-01", seconds = 1), "inventory/00_sources.csv")
put(frame(key = c("cohort_ids", "anchor:sex", "anchor:age_group", "anchor:lc_group", "anchor:route",
                  "anchor:study", "anchor:ethnicity", "identifiers_ambiguous"),
          value = c("invented", "sex_reg", "age_band", "lc_status", "route_reg", "study_src",
                    "ethnic_grp", "0")), "inventory/01_context.csv")

tables <- frame(
  path = c("registration_data", "clinic_data/clinical_tests/tests_t0", "clinic_data/clinical_tests/tests_t1",
           "health_survey/survey_t0", "health_survey/dictionary", "react_records"),
  class = c("tbl_df/tbl/data.frame", "tbl_df/tbl/data.frame", "data.frame", "tbl_df/tbl/data.frame",
            "data.frame", "data.table/data.frame"),
  n_rows = c(400, 300, 60, 280, 50, 450),
  n_cols = c(15, 10, 4, 5, 3, 4)
)
put(cbind(source = "wrangled", tables), "inventory/02_structure.csv")
put(frame(source = "wrangled", table = tables$path, visit = c(NA, "t0", "t1", "t0", NA, NA),
          n_rows = tables$n_rows, n_rows_source = NA, n_cols = tables$n_cols,
          has_persons = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
          person_level = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
          n_persons = c(400, 300, 60, 280, NA, 390), n_persons_main = c(300, 300, 60, 270, NA, 300),
          n_rows_no_person = NA, profile_level = "full",
          rpp_1 = c(400, 300, 60, 280, NA, 330), rpp_2 = c(NA, NA, NA, NA, NA, 60),
          rpp_3_5 = NA, rpp_6_10 = NA, rpp_11_plus = NA), "inventory/03_tables.csv")

col <- function(table, column, class, role, n_nonmissing, n_missing = NA, n_distinct = NA) {
  frame(source = "wrangled", table = table, column = column, class = class, role = role,
        n_nonmissing = n_nonmissing, n_missing = n_missing, n_distinct = n_distinct)
}
reg <- "registration_data"; t0 <- "clinic_data/clinical_tests/tests_t0"
t1 <- "clinic_data/clinical_tests/tests_t1"; sv <- "health_survey/survey_t0"
dict <- "health_survey/dictionary"; rr <- "react_records"
columns <- rbind(
  col(reg, "subject_id", "numeric", "identifier", 400, NA, 400),
  col(reg, "reg_code", "character", "identifier", 400, NA, 400),
  col(reg, "sex_reg", "character", "categorical", 400, NA, 2),
  col(reg, "age_band", "character", "categorical", 400, NA, 4),
  col(reg, "lc_status", "character", "categorical", 390, 10, 3),
  col(reg, "route_reg", "character", "categorical", 400, NA, 2),
  col(reg, "study_src", "character", "categorical", 400, NA, 2),
  col(reg, "ethnic_grp", "character", "categorical", 395, NA, 3),
  col(reg, "reg_date", "Date", "date", 400, NA, 60),
  col(reg, "symptom_count", "numeric", "continuous", 380, 20, 25),
  col(reg, "fatigue", "character", "categorical", 360, 40, 4),
  col(reg, "free_comment", "character", "free_text", 100, 300, 95),
  col(reg, "blank_col", "logical", "empty", NA, 400, NA),
  col(reg, "dob", "Date", "withheld", 400, NA, NA),
  col(reg, "when_text", "character", "high_cardinality", 200, 200, 120),
  col(t0, "subject_id", "numeric", "identifier", 300, NA, 300),
  col(t0, "reg_code", "character", "identifier", 300, NA, 300),
  col(t0, "visit_date", "Date", "date", 300, NA, 40),
  col(t0, "sys_bp", "numeric", "continuous", 295, NA, 80),
  col(t0, "dia_bp", "numeric", "continuous", 295, NA, 60),
  col(t0, "body_weight", "numeric", "continuous", 290, 10, 150),
  col(t0, "grip_hand", "character", "categorical", 250, 50, 2),
  col(t0, "o2_sat", "numeric", "coded_numeric", 280, 20, 9),
  col(t0, "sample_time", "POSIXct", "date", 270, 30, 270),
  col(t0, "kit_batch", "integer", "coded_numeric", 300, NA, 4),
  col(t1, "subject_id", "numeric", "identifier", 60, NA, 60),
  col(t1, "visit_date", "Date", "date", 60, NA, 20),
  col(t1, "sys_bp", "numeric", "continuous", 60, NA, 40),
  col(t1, "dia_bp", "numeric", "continuous", 60, NA, 35),
  col(sv, "subject_id", "numeric", "identifier", 280, NA, 280),
  col(sv, "sex_reg", "character", "categorical", 280, NA, 2),
  col(sv, "mood_score", "numeric", "coded_numeric", 270, 10, 5),
  col(sv, "smoker", "character", "categorical", 275, NA, 3),
  col(sv, "sleep_hours", "integer", "continuous", 265, 15, 10),
  col(dict, "variable", "character", "high_cardinality", 50, NA, 45),
  col(dict, "label", "character", "high_cardinality", 50, NA, 40),
  col(dict, "value_label", "character", "categorical", 45, NA, 3),
  col(rr, "subject_id", "numeric", "identifier", 450, NA, 390),
  col(rr, "round_num", "numeric", "coded_numeric", 450, NA, 5),
  col(rr, "test_result", "character", "categorical", 430, 20, 3),
  col(rr, "record_date", "POSIXct", "date", 450, NA, 30)
)
columns$position <- stats::ave(seq_len(nrow(columns)), columns$table, FUN = seq_along)
put(columns[c("source", "table", "position", "column", "class", "role", "n_nonmissing", "n_missing",
              "n_distinct")], "inventory/04_columns.csv")
put(frame(source = "wrangled", set = c("main/Registered", "main/Completed survey"), n = c(300, 270),
          n_main = c(300, 270)), "inventory/08_id_sets.csv")

# numeric quantiles: overall rows have p05-p95 (p01 and p99 need 1,000 values)
num <- function(table, column, n, q, stratum_var = "all", stratum = "all", decimals = 0,
                share_integer = 1, m77 = NA) {
  if (stratum_var != "all") q[c(1, 2, 8, 9)] <- NA
  row <- frame(source = "wrangled", table = table, column = column, stratum_var = stratum_var,
               stratum = stratum, n = n, mean_trimmed = NA, n_zero = NA, n_negative = NA,
               share_integer = share_integer, decimals = decimals)
  row[qcols] <- as.list(q)
  row[codes] <- NA
  row$n_code_m77 <- m77
  row
}
numeric <- rbind(
  num(reg, "symptom_count", 380, c(NA, 0, 0, 1, 3, 6, 9, 11, NA)),
  num(reg, "symptom_count", 160, c(NA, NA, 0, 1, 2, 4, 7, NA, NA), "lc_group", "None"),
  num(reg, "symptom_count", 140, c(NA, NA, 2, 4, 6, 9, 12, NA, NA), "lc_group", "Persistent"),
  num(t0, "sys_bp", 295, c(NA, 105, 110, 118, 128, 140, 152, 160, NA), decimals = 1, share_integer = 0.3),
  num(t0, "sys_bp", 150, c(NA, NA, 105, 112, 121, 132, 143, NA, NA), "sex", "Female", 1, 0.3),
  num(t0, "sys_bp", 145, c(NA, NA, 116, 124, 134, 146, 158, NA, NA), "sex", "Male", 1, 0.3),
  num(t0, "dia_bp", 295, c(NA, 65, 68, 74, 80, 87, 94, 98, NA), decimals = 1, share_integer = 0.3),
  num(t0, "body_weight", 290, c(NA, 55, 59, 66, 76, 88, 100, 108, NA), decimals = 1, share_integer = 0.1),
  num(t0, "o2_sat", 260, c(NA, 94, 95, 96, 97, 98, 99, 99, NA), m77 = 20),
  num(t0, "kit_batch", 300, c(NA, 1, 1, 2, 3, 3, 4, 4, NA)),
  num(t1, "sys_bp", 60, c(NA, 106, 111, 119, 129, 141, 151, 158, NA), decimals = 1, share_integer = 0.3),
  num(t1, "dia_bp", 60, c(NA, 66, 69, 75, 81, 88, 93, 97, NA), decimals = 1, share_integer = 0.3),
  num(sv, "mood_score", 260, c(NA, 0, 0, 1, 1, 2, 3, 3, NA), m77 = 10),
  num(sv, "sleep_hours", 265, c(NA, 5, 5, 6, 7, 8, 8, 9, NA)),
  num(rr, "round_num", 450, c(NA, 1, 1, 2, 3, 4, 5, 5, NA)),
  num(rr, ".rows_per_person", 390, c(NA, NA, 1, 1, 1, 1, 2, NA, NA))
)
put(numeric, "profile/10_numeric.csv")

cat_rows <- function(table, column, levels, n, stratum_var = "all", stratum = "all") {
  frame(source = "wrangled", table = table, column = column, stratum_var = stratum_var, stratum = stratum,
        level = levels, level_order = seq_along(levels), n_rows = n, n_persons = n)
}
ages <- c("18-34", "35-54", "55-74", "75+")
categorical <- rbind(
  cat_rows(reg, "sex_reg", c("Female", "Male"), c(220, 180)),
  cat_rows(reg, "age_band", ages, c(80, 140, 150, 30)),
  cat_rows(reg, "age_band", ages, c(50, 80, 75, 15), "sex", "Female"),
  cat_rows(reg, "age_band", ages, c(30, 60, 75, 15), "sex", "Male"),
  cat_rows(reg, "age_band", ages, c(55, 105, 115, 25), "main_cohort", "Yes"),
  cat_rows(reg, "age_band", ages, c(25, 35, 35, NA), "main_cohort", "No"),
  cat_rows(reg, "lc_status", c("None", "Short", "Persistent"), c(160, 90, 140)),
  cat_rows(reg, "lc_status", c("None", "Short", "Persistent"), c(40, 20, 20), "age_group", "18-34"),
  cat_rows(reg, "lc_status", c("None", "Short", "Persistent"), c(10, NA, 15), "age_group", "75+"),
  cat_rows(reg, "lc_status", c("None", "Short", "Persistent"), c(95, 50, 75), "sex", "Female"),
  cat_rows(reg, "route_reg", c("Swab", "Antibody"), c(250, 150)),
  cat_rows(reg, "study_src", c("REACT-1", "REACT-2"), c(240, 160)),
  cat_rows(reg, "ethnic_grp", c("White", "Asian", "Other"), c(330, 40, 25)),
  # "Rarely" was suppressed (fewer than 10 people), so it is never generated
  cat_rows(reg, "fatigue", c("Never", "Rarely", "Sometimes", "Often"), c(150, NA, 120, 90)),
  cat_rows(reg, "fatigue", c("Never", "Rarely", "Sometimes", "Often"), c(80, NA, 40, 20), "lc_group", "None"),
  cat_rows(reg, "fatigue", c("Never", "Rarely", "Sometimes", "Often"), c(15, NA, 50, 65), "lc_group", "Persistent"),
  cat_rows(t0, "grip_hand", c("Left", "Right"), c(30, 220)),
  cat_rows(t0, "o2_sat", c("-77", "93", "94", "95", "96", "97", "98", "99"), c(20, NA, 15, 30, 55, 70, 55, 35)),
  cat_rows(t0, "kit_batch", c("1", "2", "3", "4"), c(70, 80, 75, 75)),
  cat_rows(sv, "sex_reg", c("Female", "Male"), c(155, 125)),
  cat_rows(sv, "mood_score", c("-77", "0", "1", "2", "3"), c(10, 60, 90, 70, 40)),
  cat_rows(sv, "smoker", c("Never", "Former", "Current"), c(160, 85, 30)),
  cat_rows(sv, "smoker", c("Never", "Former", "Current"), c(25, 15, 10), "age_group", "18-34"),
  cat_rows(dict, "value_label", c("Yes", "No", "Unsure"), c(20, 15, 10)),
  cat_rows(rr, "round_num", c("1", "2", "3", "4", "5"), c(80, 95, 100, 90, 85)),
  cat_rows(rr, "test_result", c("Negative", "Positive", "Void"), c(370, 50, 10))
)
put(categorical, "profile/11_categorical.csv")

summary <- frame(source = "wrangled", table = t1, column = "visit_date", n = 60, n_before_2019_12 = NA,
                 n_after_run_date = NA)
summary[qcols] <- as.list(c(NA, "2021-10", "2021-10", "2021-10", "2021-11", "2021-11", "2021-12", "2021-12", NA))
put(summary, "profile/12_date_summary.csv")

month_rows <- function(table, column, months, n) cat_rows(table, column, months, n)
months_2021 <- sprintf("2021-%02d", 1:9)
put(rbind(
  month_rows(reg, "reg_date", c("2020-12", months_2021[1:6]), c(40, 75, 70, 65, 60, 50, 40)),
  month_rows(t0, "visit_date", months_2021, c(20, 35, 40, 45, 40, 35, 30, 30, 25)),
  month_rows(t0, "sample_time", months_2021, c(20, 30, 35, 40, 35, 35, 30, 25, 20)),
  month_rows(rr, "record_date", c("2020-06", "2020-09", "2021-01", "2021-03"), c(100, 120, 110, 120))
), "profile/13_date_months.csv")

put(rbind(
  cat_rows(reg, "subject_id", "999999999", 400),
  cat_rows(reg, "reg_code", "AA9999", 400),
  cat_rows(t0, "reg_code", "AA9999", 300),
  cat_rows(reg, "when_text", c("99/99/9999", "AAA 9999"), c(150, 50)),
  cat_rows(dict, "variable", c("AAAA9", "AAAAA"), c(30, 20))
), "profile/14_id_shapes.csv")

empty_header <- "\"source\",\"table\",\"column\",\"stratum_var\",\"stratum\",\"level\",\"level_order\",\"n_rows\",\"n_persons\""
writeLines(empty_header, file.path(out, "profile/15_availability.csv"))
writeLines(empty_header, file.path(out, "profile/16_availability_patterns.csv"))
for (f in c("17_availability_pairs", "19_visit_pairs", "20_timing")) {
  writeLines("\"\"", file.path(out, "profile", paste0(f, ".csv")))
}

put(frame(source = "wrangled", scope = c(t0, t0, sv, t1),
          var1 = c("sys_bp", "body_weight", "mood_score", "sys_bp"),
          var2 = c("dia_bp", "sys_bp", "sleep_hours", "dia_bp"),
          rho = c(0.7, 0.2, -0.4, 0.65), n = c(295, 290, 255, 60)), "profile/18_correlations.csv")

# omics: one invented platform; features f1-f3 share a component (rank
# correlation about 0.6), f4-f6 are independent
features <- paste0("f", 1:6)
put(frame(source = "panel", platform = "panel_a", n_samples_source = 300, n_samples = 300,
          n_samples_mapped = 300, n_samples_profiled = 300, n_persons = 280, n_persons_main = 270,
          n_samples_t0 = 280, n_samples_t1 = 20, n_samples_visit_unknown = NA, n_features = 6,
          n_features_profiled = 6, share_missing = 0.05, share_nonpositive = 0, transform = "none",
          profiled_on = "all_samples", layout = "wide", value_column = NA, feature_column = NA,
          sample_column = "sample_id", n_duplicate_cells = NA, first_feature = "f1",
          n_features_source = 6, feature_filter = NA), "profile/30_omics_platform.csv")
omics <- frame(source = "panel", platform = "panel_a", feature = features, n_total = 300,
               n = c(300, 285, 300, 270, 300, 300), share_missing = c(0, 0.05, 0, 0.1, 0, 0),
               share_zero = NA, share_at_min = NA)
omics[qcols] <- list(NA, 8, 8.5, 9.2, 10, 10.8, 11.5, 12, NA)
omics[omics$feature == "f6", qcols] <- list(NA, NA, NA, NA, NA, NA, NA, NA, NA)
put(omics, "profile/31_omics_features.csv")
put(frame(source = "panel", platform = "panel_a", pc = 1:2, eigenvalue = c(2.2, 1.0),
          share = c(0.37, 0.17), cumulative = c(0.37, 0.53), n_samples = 300, n_features = 6),
    "profile/33_omics_eigen.csv")
loadings <- frame(source = "panel", platform = "panel_a", feature = features)
loadings[paste0("pc", 1:10)] <- NA
loadings$pc1 <- c(0.577, 0.577, 0.577, 0, 0, 0)
loadings$pc2 <- c(0, 0, 0, 0, 1, 0)
put(loadings, "profile/34_omics_loadings.csv")
