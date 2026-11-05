# Writes clinical-candidate/: an invented Clinical Study inventory candidate in
# the file layout of reactextract's clinical inventory kit (v1.2). Table paths, column
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

put(frame(source = c("wrangled", "master", "panel", "rnaseq_t0", "gene"),
          group = c("wrangled", "wrangled", "omics", "omics", "omics"), reader = "invented", status = "ok",
          message = "", size_mb = 1, modified = "2026-01-01", seconds = 1, kit = "clinical-inventory-v1.2"),
    "inventory/00_sources.csv")
put(frame(key = c("cohort_ids", "anchor:sex", "anchor:age_group", "anchor:lc_group", "anchor:route",
                  "anchor:study", "anchor:ethnicity", "identifiers_ambiguous"),
          value = c("invented", "sex_reg", "age_band", "lc_status", "route_reg", "study_src",
                    "ethnic_grp", "0")), "inventory/01_context.csv")

# the wrangled list, including a population table (the whole study), a branch
# of registrants who weren't study participants (structure only), a table of
# many rows per person and two date limits that aren't tables
tables <- frame(
  path = c("registration_data", "clinic_data/clinical_tests/tests_t0", "clinic_data/clinical_tests/tests_t1",
           "health_survey/survey_t0", "health_survey/dictionary", "react_records", "infection_log",
           "date_rules/min_date", "date_rules/max_date", "test_log", "outsiders/registration_data",
           "outsiders/clinic_data/clinical_tests/tests_t1"),
  class = c("tbl_df/tbl/data.frame", "tbl_df/tbl/data.frame", "data.frame", "tbl_df/tbl/data.frame",
            "data.frame", "data.table/data.frame", "tbl_df/tbl/data.frame", "Date", "Date", "data.frame",
            "tbl_df/tbl/data.frame", "data.frame"),
  n_rows = c(400, 300, 60, 280, 50, 450, 5000, NA, NA, 4000, 10, NA),
  n_cols = c(15, 10, 4, 5, 3, 4, 3, NA, NA, 2, 15, 4)
)
put(rbind(cbind(source = "wrangled", tables),
          frame(source = c("master", rep("panel", 7), rep("rnaseq_t0", 4), "gene"),
                path = c("combined_data_set", "df_panel_a", "df_panel_long", "df_stack", "names_lookup",
                         "protein_summary", "qc_record", "panel_unread", "abundance", "counts", "length",
                         "countsFromAbundance", "carrier.csv"),
                class = c("tbl_df/tbl/data.frame", rep("tbl_df/tbl/data.frame", 3), "data.frame",
                          "tbl_df/tbl/data.frame", "data.frame", "data.frame", rep("matrix/array", 3),
                          "character", "data.frame"),
                n_rows = c(410, 240, 1440, 1440, 12, 6, NA, 20, 8, 8, 8, NA, 45),
                n_cols = c(7, 9, 7, 5, 3, 2, 2, 2, 50, 50, 50, NA, 2))), "inventory/02_structure.csv")
wrangled_tables <- tables[!tables$class %in% "Date", ]
put(rbind(
  frame(source = "wrangled", table = wrangled_tables$path,
        visit = c(NA, "t0", "t1", "t0", NA, NA, NA, NA, NA, "t1"),
        n_rows = wrangled_tables$n_rows, n_rows_source = NA, n_cols = wrangled_tables$n_cols,
        has_persons = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
        person_level = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE),
        n_persons = c(400, 300, 60, 280, NA, 390, 5000, 200, 10, NA),
        n_persons_main = c(300, 300, 60, 270, NA, 300, 300, 200, NA, NA),
        n_rows_no_person = NA,
        profile_level = c(rep("full", 8), "structure_only", "structure_only"),
        rpp_1 = c(400, 300, 60, 280, NA, 330, 5000, 20, 10, NA), rpp_2 = c(NA, NA, NA, NA, NA, 60, NA, 30, NA, NA),
        rpp_3_5 = c(rep(NA, 7), 50, NA, NA), rpp_6_10 = c(rep(NA, 7), 50, NA, NA),
        rpp_11_plus = c(rep(NA, 7), 50, NA, NA), person_key = "subject_id"),
  frame(source = c("master", rep("panel", 8), "gene"),
        table = c("combined_data_set", "panel_a/samples", "panel_long/samples", "panel_long/long_rows",
                  "stack:A/samples", "stack/long_rows", "names_lookup", "protein_summary", "qc_record", "carrier"),
        visit = NA, n_rows = c(410, 240, 240, 1440, 240, 1440, 12, 6, NA, 45), n_rows_source = NA,
        n_cols = c(7, 3, 2, 5, 2, 3, 3, 2, 2, 2), has_persons = c(rep(TRUE, 6), FALSE, FALSE, FALSE, TRUE),
        person_level = c(TRUE, rep(FALSE, 8), TRUE),
        n_persons = c(410, rep(220, 5), NA, NA, NA, 45), n_persons_main = c(300, rep(215, 5), NA, NA, NA, 45),
        n_rows_no_person = NA, profile_level = c(rep("full", 8), "structure_only", "full"),
        rpp_1 = NA, rpp_2 = NA, rpp_3_5 = NA, rpp_6_10 = NA, rpp_11_plus = NA, person_key = NA)
), "inventory/03_tables.csv")

col <- function(table, column, class, role, n_nonmissing, n_missing = NA, n_distinct = NA, source = "wrangled") {
  frame(source = source, table = table, column = column, class = class, role = role,
        n_nonmissing = n_nonmissing, n_missing = n_missing, n_distinct = n_distinct)
}
reg <- "registration_data"; t0 <- "clinic_data/clinical_tests/tests_t0"
t1 <- "clinic_data/clinical_tests/tests_t1"; sv <- "health_survey/survey_t0"
dict <- "health_survey/dictionary"; rr <- "react_records"; inf <- "infection_log"; tl <- "test_log"
registration_columns <- function(table, n) rbind(
  col(table, "subject_id", "numeric", "identifier", n, NA, n),
  col(table, "reg_code", "character", "identifier", n, NA, n),
  col(table, "sex_reg", "character", "categorical", n, NA, 2),
  col(table, "age_band", "character", "categorical", n, NA, 4),
  col(table, "lc_status", "character", "categorical", n, NA, 3),
  col(table, "route_reg", "character", "categorical", n, NA, 2),
  col(table, "study_src", "character", "categorical", n, NA, 2),
  col(table, "ethnic_grp", "character", "categorical", n, NA, 3),
  col(table, "reg_date", "Date", "date", n, NA, 60),
  col(table, "symptom_count", "numeric", "continuous", NA, NA, NA),
  col(table, "fatigue", "character", "categorical", NA, NA, NA),
  col(table, "free_comment", "character", "free_text", NA, NA, NA),
  col(table, "blank_col", "logical", "empty", NA, n, NA),
  col(table, "dob", "Date", "withheld", n, NA, NA),
  col(table, "when_text", "character", "high_cardinality", NA, NA, NA)
)
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
  col(rr, "record_date", "POSIXct", "date", 450, NA, 30),
  col(inf, "subject_id", "numeric", "identifier", 5000, NA, 5000),
  col(inf, "onset_date", "Date", "date", 1000, 4000, 300),
  col(inf, "onset_source", "character", "categorical", 1000, 4000, 2),
  col(tl, "subject_id", "numeric", "identifier", 4000, NA, 200),
  col(tl, "test_kind", "character", "categorical", 4000, NA, 2),
  registration_columns("outsiders/registration_data", NA),
  col("outsiders/clinic_data/clinical_tests/tests_t1", "subject_id", "numeric", "identifier", NA),
  col("outsiders/clinic_data/clinical_tests/tests_t1", "visit_date", "Date", "date", NA),
  col("outsiders/clinic_data/clinical_tests/tests_t1", "sys_bp", "numeric", "continuous", NA),
  col("outsiders/clinic_data/clinical_tests/tests_t1", "dia_bp", "numeric", "continuous", NA),
  # master holds the anchors (the kit takes them from combined_data_set.rds)
  col("combined_data_set", "subject_id", "numeric", "identifier", 410, NA, 410, "master"),
  col("combined_data_set", "sex_reg", "character", "categorical", 410, NA, 2, "master"),
  col("combined_data_set", "age_band", "character", "categorical", 410, NA, 4, "master"),
  col("combined_data_set", "lc_status", "character", "categorical", 400, 10, 3, "master"),
  col("combined_data_set", "route_reg", "character", "categorical", 410, NA, 2, "master"),
  col("combined_data_set", "study_src", "character", "categorical", 410, NA, 2, "master"),
  col("combined_data_set", "ethnic_grp", "character", "categorical", 405, NA, 3, "master"),
  # omics sample tables, long rows and a genotype table
  col("panel_a/samples", "subject_id", "character", "identifier", 240, NA, 220, "panel"),
  col("panel_a/samples", "sample_id", "character", "identifier", 240, NA, 240, "panel"),
  col("panel_a/samples", "timepoint", "character", "categorical", 240, NA, 2, "panel"),
  col("panel_long/samples", "subject_id", "character", "identifier", 240, NA, 220, "panel"),
  col("panel_long/samples", "sample_ref", "character", "identifier", 240, NA, 240, "panel"),
  col("panel_long/long_rows", "protein", "character", "high_cardinality", 1440, NA, 6, "panel"),
  col("panel_long/long_rows", "assay_group", "character", "categorical", 1440, NA, 2, "panel"),
  col("panel_long/long_rows", "value", "numeric", "continuous", 1440, NA, 1400, "panel"),
  col("panel_long/long_rows", "value_norm", "numeric", "continuous", 1440, NA, 1400, "panel"),
  col("panel_long/long_rows", "lod", "numeric", "continuous", 1440, NA, 400, "panel"),
  col("stack:A/samples", "subject_id", "character", "identifier", 240, NA, 220, "panel"),
  col("stack:A/samples", "sample_id", "character", "identifier", 240, NA, 240, "panel"),
  col("stack/long_rows", "platform", "character", "categorical", 1440, NA, 1, "panel"),
  col("stack/long_rows", "protein", "character", "high_cardinality", 1440, NA, 6, "panel"),
  col("stack/long_rows", "value", "numeric", "continuous", 1440, NA, 1400, "panel"),
  # reference tables: a name lookup (one row per platform feature), a protein
  # summary and a structure-only QC record
  col("names_lookup", "native_name", "character", "high_cardinality", 12, NA, 12, "panel"),
  col("names_lookup", "common_name", "character", "high_cardinality", 12, NA, 6, "panel"),
  col("names_lookup", "origin", "character", "categorical", 12, NA, 2, "panel"),
  col("protein_summary", "common_name", "character", "high_cardinality", 6, NA, 6, "panel"),
  col("protein_summary", "n_platforms", "integer", "coded_numeric", 6, NA, 1, "panel"),
  col("qc_record", "step", "character", "categorical", NA, NA, 3, "panel"),
  col("qc_record", "n_removed", "integer", "coded_numeric", NA, NA, 3, "panel"),
  col("carrier", "REG_CODE", "character", "identifier", 45, NA, 45, "gene"),
  col("carrier", "carrier", "character", "categorical", 45, NA, 2, "gene")
)
columns$position <- stats::ave(seq_len(nrow(columns)), columns$table, FUN = seq_along)
put(columns[c("source", "table", "position", "column", "class", "role", "n_nonmissing", "n_missing",
              "n_distinct")], "inventory/04_columns.csv")
# source column order of the long tables (the kit's tables split them into
# sample-level and row-level columns); one class differs from the kit's tables
put(frame(source = "panel",
          table = c(rep("panel_long", 7), rep("stack", 5)),
          position = c(1:7, 1:5),
          column = c("protein", "subject_id", "value", "sample_ref", "value_norm", "assay_group", "lod",
                     "platform", "sample_id", "protein", "value", "subject_id"),
          class = c("character", "character", "numeric", "character", "numeric", "factor", "numeric",
                    "character", "character", "character", "numeric", "character")),
    "inventory/04b_source_columns.csv")
put(frame(source = "invented", set = c("main/Registered", "main/Completed survey"), n = c(300, 270),
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

cat_rows <- function(table, column, levels, n, stratum_var = "all", stratum = "all", source = "wrangled") {
  frame(source = source, table = table, column = column, stratum_var = stratum_var, stratum = stratum,
        level = levels, level_order = seq_along(levels), n_rows = n, n_persons = n)
}
anchor_rows <- function(...) {
  rows <- cat_rows("combined_data_set", ...)
  rows$source <- "master"
  rows
}
ages <- c("18-34", "35-54", "55-74", "75+")
categorical <- rbind(
  anchor_rows("sex_reg", c("Female", "Male"), c(225, 185)),
  anchor_rows("age_band", ages, c(80, 140, 155, 35)),
  anchor_rows("age_band", ages, c(50, 80, 75, 20), "sex", "Female"),
  anchor_rows("age_band", ages, c(30, 60, 80, 15), "sex", "Male"),
  anchor_rows("age_band", ages, c(55, 105, 115, 25), "main_cohort", "Yes"),
  anchor_rows("age_band", ages, c(25, 35, 40, NA), "main_cohort", "No"),
  anchor_rows("lc_status", c("None", "Short", "Persistent"), c(160, 95, 145)),
  anchor_rows("lc_status", c("None", "Short", "Persistent"), c(40, 20, 20), "age_group", "18-34"),
  anchor_rows("lc_status", c("None", "Short", "Persistent"), c(10, NA, 15), "age_group", "75+"),
  anchor_rows("lc_status", c("None", "Short", "Persistent"), c(95, 50, 75), "sex", "Female"),
  anchor_rows("route_reg", c("Swab", "Antibody"), c(255, 155)),
  anchor_rows("study_src", c("REACT-1", "REACT-2"), c(245, 165)),
  anchor_rows("ethnic_grp", c("White", "Asian", "Other"), c(340, 40, 25)),
  cat_rows(inf, "onset_source", c("registration", "history"), c(400, 600)),
  cat_rows(tl, "test_kind", c("PCR", "LFD"), c(1500, 2500)),
  cat_rows("panel_a/samples", "timepoint", c("baseline", "followup"), c(200, 40), source = "panel"),
  cat_rows("panel_long/long_rows", "assay_group", c("core", "extra"), c(960, 480), source = "panel"),
  cat_rows("stack/long_rows", "platform", "A", 1440, source = "panel"),
  cat_rows("carrier", "carrier", c("Yes", "No"), c(15, 30), source = "gene"),
  cat_rows("names_lookup", "origin", c("Panel_A", "Panel_Long"), c(6, 6), source = "panel"),
  cat_rows("protein_summary", "n_platforms", "2", 6, source = "panel"),
  # availability patterns of the main cohort (combined_data_set's flags)
  frame(source = "master", table = "availability_patterns:main_cohort", column = "baseline",
        stratum_var = "all", stratum = "all",
        level = c("has_bloods_t0+has_panel_t0", "has_bloods_t0+has_panel_t0+has_genome",
                  "has_bloods_t0+has_rnaseq_t0+has_panel_t0", "has_bloods_t0", "has_bloods_t0+has_genome"),
        level_order = 1:5, n_rows = c(120, 55, 70, 30, NA), n_persons = c(120, 55, 70, 30, NA)),
  frame(source = "master", table = "availability_patterns:main_cohort", column = "followup",
        stratum_var = "all", stratum = "all", level = c("(none)", "has_bloods_t1+has_panel_t1", "has_bloods_t1"),
        level_order = 1:3, n_rows = c(240, 45, 15), n_persons = c(240, 45, 15)),
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

# placeholders (dates up to 1900-01-01 or in 9999) are counted in
# n_placeholder and left out of the quantiles; the onset dates' placeholder
# months are suppressed one by one, so only the count shows them
summary <- frame(source = "wrangled", table = c(t1, inf), column = c("visit_date", "onset_date"), n = c(60, 1000),
                 n_placeholder = c(NA, 100), n_before_2019_12 = NA, n_after_run_date = NA)
summary[qcols] <- list(NA, c("2021-10", "2020-04"), c("2021-10", "2020-04"), c("2021-10", "2020-06"),
                       c("2021-11", "2020-10"), c("2021-11", "2020-12"), c("2021-12", "2021-01"),
                       c("2021-12", "2021-01"), NA)
put(summary, "profile/12_date_summary.csv")

month_rows <- function(table, column, months, n) cat_rows(table, column, months, n)
months_2021 <- sprintf("2021-%02d", 1:9)
put(rbind(
  month_rows(reg, "reg_date", c("1582-10", "2020-12", months_2021[1:6]), c(20, 40, 75, 70, 65, 60, 50, 40)),
  month_rows(inf, "onset_date", c("2020-04", "2020-06", "2020-10", "2020-12", "2021-01"), c(150, 200, 250, 200, 200)),
  month_rows(t0, "visit_date", months_2021, c(20, 35, 40, 45, 40, 35, 30, 30, 25)),
  month_rows(t0, "sample_time", months_2021, c(20, 30, 35, 40, 35, 35, 30, 25, 20)),
  month_rows(rr, "record_date", c("2020-06", "2020-09", "2021-01", "2021-03", "9999-12"), c(100, 120, 110, 120, 20))
), "profile/13_date_months.csv")

put(rbind(
  cat_rows("panel_a/samples", "sample_id", "S99999", 240, source = "panel"),
  cat_rows(reg, "subject_id", "999999999", 400),
  cat_rows(reg, "reg_code", "AA9999", 400),
  cat_rows(t0, "reg_code", "AA9999", 300),
  cat_rows(reg, "when_text", c("99/99/9999", "AAA 9999"), c(150, 50)),
  cat_rows(dict, "variable", c("AAAA9", "AAAAA"), c(30, 20))
), "profile/14_id_shapes.csv")

empty_header <- "\"source\",\"table\",\"column\",\"stratum_var\",\"stratum\",\"level\",\"level_order\",\"n_rows\",\"n_persons\""
writeLines(empty_header, file.path(out, "profile/15_availability.csv"))
writeLines(empty_header, file.path(out, "profile/16_availability_patterns.csv"))
writeLines("\"\"", file.path(out, "profile/17_availability_pairs.csv"))
put(frame(source = "wrangled", family = "clinical_tests", column = c("sys_bp", "dia_bp"),
          role = "continuous", n = 60, rho = c(0.8, 0.75), share_same = NA, mean_trimmed = NA,
          p10 = NA, p25 = NA, p50 = NA, p75 = NA, p90 = NA), "profile/19_visit_pairs.csv")
# intervals in days; "master:" columns are resolved to the wrangled table holding them
timing <- frame(source = "wrangled", label = c("onset_to_registration", "registration_to_baseline", "baseline_to_followup"),
                from = c("master:onset_date", "master:reg_date", "clinic_data/clinical_tests/tests_t0:visit_date"),
                to = c("master:reg_date", "clinic_data/clinical_tests/tests_t0:visit_date",
                       "clinic_data/clinical_tests/tests_t1:visit_date"),
                n = c(300, 300, 60), mean_trimmed = NA, n_zero = NA, n_negative = NA, share_integer = 1, decimals = 0)
timing[qcols] <- list(NA, c(40, 5, 160), c(60, 8, 170), c(100, 12, 180), c(150, 20, 195), c(200, 30, 215),
                      c(260, 45, 235), c(290, 55, 245), NA)
timing[codes] <- NA
put(timing, "profile/20_timing.csv")

put(frame(source = "wrangled", scope = c(t0, t0, sv, t1),
          var1 = c("sys_bp", "body_weight", "mood_score", "sys_bp"),
          var2 = c("dia_bp", "sys_bp", "sleep_hours", "dia_bp"),
          rho = c(0.7, 0.2, -0.4, 0.65), n = c(295, 290, 255, 60)), "profile/18_correlations.csv")

# omics: source "panel" has a wide table, a long table with raw and
# normalised values, and a stacked table, all holding the same measurements
# of six features; f1-f3 share a component (rank correlation about 0.6).
# "rnaseq_t0" is a tximport list; "gene" a genotype table.
platform <- function(name, layout, value_column = NA, feature_column = NA, sample_column = "sample_id",
                     split = NA, source = "panel", n = 240, t0 = 200, t1 = 40, features = 6, transform = "none") {
  frame(source = source, platform = name, n_samples_source = n, n_samples = n, n_samples_mapped = n,
        n_samples_profiled = n, n_persons = 220, n_persons_main = 215, n_samples_t0 = t0, n_samples_t1 = t1,
        n_samples_visit_unknown = NA, n_features = features, n_features_profiled = features,
        share_missing = 0.02, share_nonpositive = 0, transform = transform, profiled_on = "all_samples",
        layout = layout, value_column = value_column, feature_column = feature_column,
        sample_column = sample_column, split = split, n_duplicate_cells = NA, first_feature = NA,
        n_features_source = features, feature_filter = NA)
}
put(rbind(
  platform("panel_a", "wide"),
  platform("panel_long:value", "long", "value", "protein", "sample_ref"),
  platform("panel_long:value_norm", "long", "value_norm", "protein", "sample_ref"),
  platform("stack:A", "long", "value", "protein", split = "platform = A"),
  platform("rna_seq", "tximport", sample_column = NA, source = "rnaseq_t0", n = 50, t0 = 50, t1 = NA,
           features = 5, transform = "log2p1")
), "profile/30_omics_platform.csv")
feature_rows <- function(platform, features, medians, source = "panel", missing = 0) {
  rows <- frame(source = source, platform = platform, feature = features, n_total = 240, n = 240,
                share_missing = missing, share_zero = NA, share_at_min = NA)
  rows[qcols] <- NA
  for (j in seq_along(features)) {
    if (is.na(medians[[j]])) next
    rows[j, qcols] <- as.list(c(NA, medians[[j]] + c(-2, -1.5, -0.8, 0, 0.8, 1.5, 2), NA))
  }
  rows
}
medians <- c(10, 11, 12, 13, 14, NA)
put(rbind(
  feature_rows("panel_a", paste0("f", 1:6), medians, missing = c(0, 0.05, 0, 0.1, 0, 0)),
  # the long and stacked tables name the features P1-P6 in reverse order;
  # identical quantiles show which is which
  feature_rows("panel_long:value", paste0("P", 1:6), rev(medians)),
  feature_rows("panel_long:value_norm", paste0("P", 1:6), rev(medians) - 10),
  feature_rows("stack:A", paste0("P", 1:6), rev(medians)),
  feature_rows("rna_seq", paste0("G", 1:5), c(2, 4, 6, 3, 5), source = "rnaseq_t0")
), "profile/31_omics_features.csv")
library_size <- frame(source = "rnaseq_t0", platform = "rna_seq", stat = "library_size", n = 50,
                      mean_trimmed = NA, n_zero = NA, n_negative = NA, share_integer = 1, decimals = 0)
library_size[qcols] <- list(NA, 1.5e7, 1.7e7, 2e7, 2.3e7, 2.6e7, 2.9e7, 3.1e7, NA)
library_size[codes] <- NA
put(library_size, "profile/32_omics_samples.csv")
put(frame(source = "panel", platform = "panel_a", pc = 1:2, eigenvalue = c(2.2, 1.0),
          share = c(0.37, 0.17), cumulative = c(0.37, 0.53), n_samples = 240, n_features = 6),
    "profile/33_omics_eigen.csv")
loadings <- frame(source = "panel", platform = "panel_a", feature = paste0("f", 1:6))
loadings[paste0("pc", 1:10)] <- NA
loadings$pc1 <- c(0.577, 0.577, 0.577, 0, 0, 0)
loadings$pc2 <- c(0, 0, 0, 0, 1, 0)
put(loadings, "profile/34_omics_loadings.csv")
put(frame(source = "panel", platform = "panel_a", feature = paste0("f", 1:6), n = 40, rho = 0.8),
    "profile/36_omics_visit.csv")
