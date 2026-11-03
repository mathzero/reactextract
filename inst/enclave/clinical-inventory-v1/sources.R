# Sources read by the clinical inventory, in the order they are profiled.
# Paths follow code/revised/01_data_extract_clean.R and 02_cohort_definition.R
# in the clinical wrangling project (hub decision H04). Each entry names a
# reader in inventory.R; optional fields:
#   map_via      how sample or project IDs reach subject_id: "lookup" (assay
#                barcode, ge_lc_sample_lookup.rds), "identifiers"
#                (SUBJECT_IDENTIFIERS_V project_identifier), "genomics"
#                (genotyping sample well or LPID) or "auto" (whichever column
#                and map link most rows)
#   id_col       column holding that ID when the table has no subject_id
#   sample_col   omics sample key; feature_from: first feature column
#   layouts      per table of an omics list: feature, value (each value
#                column is profiled as its own platform), sample, split, or
#                type = "wide"
#   cohort_only  profile only rows that map to the main cohort (sources that
#                cover REACT participants outside the Clinical Study)
#   na           strings read as missing from a CSV ("" as well, as read_csv())
ci_sources <- function(s) {
  meta <- file.path(s$data_root, "metadata")
  omics <- file.path(s$saved_root, "ge_lc", "ge_lc_omics_objects")
  metab <- file.path(s$data_root, "metabolomics", "metabolomics_lc_ms")
  tx_manifests <- file.path(s$data_root, "transcriptomics", "manifests")
  metab_file <- function(mode) {
    run <- paste0("REACT_LCMS_P_", mode, "_PeakPantheR_2024-12-13")
    file.path(metab, run, run, paste0("REACT_LCMS_P_", mode, "_PeakPantheR_combinedData.csv"))
  }
  list(
    # wrangled layer: the stage 01 nested list and the stage 02 master table
    list(id = "wrangled", group = "wrangled", reader = "rds_tree",
      path = file.path(meta, "ge_lc_wrangled.rds"), wrangled = TRUE),
    list(id = "master", group = "wrangled", reader = "master",
      path = file.path(meta, "combined_data_set.rds")),
    list(id = "sample_lookup", group = "wrangled", reader = "rds_tree",
      path = file.path(meta, "ge_lc_sample_lookup.rds"), map_via = "lookup", id_col = "barcode"),
    list(id = "availability_sets", group = "wrangled", reader = "id_sets",
      path = file.path(meta, "cohort_availability_sets.rds")),
    list(id = "provenance", group = "wrangled", reader = "provenance",
      path = c(extraction = file.path(meta, "ge_lc_wrangled_provenance.rds"),
        cohort = file.path(meta, "cohort_provenance.rds"))),

    # omics
    # Olink, Alamar and LC-MS long tables hold one row per sample and protein;
    # df_allplats stacks the three, so it is profiled per platform. Where a
    # normalised value exists, both values are profiled.
    list(id = "proteomics", group = "omics", reader = "omics_list",
      path = file.path(omics, "olink_alamar_lcms_combined.rds"), map_via = "lookup",
      layouts = list(
        allplats = list(feature = "protein_cleaned", value = "value", sample = "sample_id", split = "platform"),
        olink = list(feature = "olink_id", value = "npx", sample = "sample_id"),
        alamar = list(feature = "protein_cleaned", value = c("npq", "npq_normalised"), sample = "sample_id"),
        lcms = list(type = "wide", sample = "sample_name"),
        lcms_long = list(feature = "protein_cleaned", value = c("value", "value_normalised"),
          sample = c("sample_id", "sample_name")))),
    list(id = "metabolomics_rpos", group = "omics", reader = "omics_csv", path = metab_file("RPOS"),
      sample_col = "Sample ID", feature_from = 23L, map_via = "lookup", na = c("", "NA")),
    list(id = "metabolomics_rneg", group = "omics", reader = "omics_csv", path = metab_file("RNEG"),
      sample_col = "Sample ID", feature_from = 23L, map_via = "lookup", na = c("", "NA")),
    # untargeted features without sample IDs: profiled unlinked to people
    list(id = "metabolomics_xcms", group = "omics", reader = "xcms",
      path = file.path(metab, "XCMS_metabolomics_intensity_data_2_noSampleID_CL.csv"), na = c("", "NA")),
    list(id = "lipids", group = "omics", reader = "omics_rds",
      path = file.path(s$data_root, "Signaling-Lipids-Analysis", "lipids_with_subject_id.rds"),
      sample_col = "Sample.Name", map_via = "identifiers"),
    # expression matrix columns are BioSample names: the manifests map them to barcodes
    list(id = "transcriptomics_t0", group = "omics", reader = "tximport",
      path = file.path(omics, "tx_baseline.rds"), map_via = "lookup", manifests = tx_manifests, visit = "t0"),
    list(id = "transcriptomics_t1", group = "omics", reader = "tximport",
      path = file.path(omics, "tx_recall.rds"), map_via = "lookup", manifests = tx_manifests, visit = "t1"),
    list(id = "transcriptomics_manifests", group = "omics", reader = "xlsx_dir",
      path = tx_manifests, id_col = "Patient.ID", map_via = "lookup"),
    list(id = "autoantibodies", group = "omics", reader = "autoantibodies",
      path = file.path(s$data_root, "autoantibodies", "REACT LC Aab data.xlsx"),
      sheets = c(controls = 4L, cases = 5L), header_in_first_row = "controls",
      id_col = "sample_id", map_via = "identifiers"),
    list(id = "genomics_samples", group = "omics", reader = "psam_dir",
      path = file.path(s$data_root, "genomics", "REACT_PGEN"),
      lookup = ci_genomics_lookup(s)),
    list(id = "apoe4", group = "omics", reader = "csv",
      path = file.path(s$data_root, "genomics", "APOE4", "apoe4_carrier.csv"), map_via = "auto",
      cohort_only = TRUE, na = c("", "NA")),

    # later questionnaires and cognition, restricted to cohort members
    list(id = "followup_2022", group = "followup", reader = "rds_tree",
      path = file.path(s$saved_root, "react_long_covid_2022_data.rds"),
      id_col = "U_PASSCODE", map_via = "identifiers", cohort_only = TRUE),
    list(id = "followup_2025", group = "followup", reader = "csv",
      path = file.path(s$saved_root, "phier_react_data_2025", "questionnaires.csv"),
      id_col = "user", map_via = "identifiers", cohort_only = TRUE),
    list(id = "cognitron_1", group = "followup", reader = "csv",
      path = file.path(s$data_root, "cog_data", "cog_data_148k.csv"), cohort_only = TRUE),
    list(id = "cognitron_2", group = "followup", reader = "csv",
      path = file.path(s$data_root, "cog_data", "merged_cognitive_final.csv"),
      id_col = "user_code", map_via = "identifiers", cohort_only = TRUE),

    # NHS linkage extracts and identifiers. Structure, timing and records per
    # person only: outcome code counts belong to reactAtlas (hub decision H10).
    list(id = "nhs_hes", group = "linkage", reader = "rds_tree",
      path = file.path(s$saved_root, "gelc_hes_data.rds"), cohort_only = TRUE),
    list(id = "nhs_gp", group = "linkage", reader = "rds_tree",
      path = file.path(s$saved_root, "gelc_gpes_data.rds"), cohort_only = TRUE),
    list(id = "linkage_ids", group = "linkage", reader = "id_sets",
      path = file.path(s$saved_root, "subject_ids_all_nhs_linked_data.rds")),
    list(id = "subject_identifiers", group = "linkage", reader = "rds_tree",
      path = file.path(s$saved_root, "subject_identifiers_gelc.rds")),
    list(id = "invites", group = "linkage", reader = "rds_tree",
      path = file.path(s$saved_root, "ge_lc_invites.rds"))
  )
}

# genotyping sample well and LPID to DNA sample barcode
ci_genomics_lookup <- function(s) {
  file.path(s$data_root, "genomics", "genomics", "data", "REACT_Covid19_DNA_Sample_LPIDs-1.tsv")
}

# Sources profiled and exported by the v1.1 supplement: the seven that failed
# in the first enclave run (2 November 2026) and the two added in v1.1.
ci_supplement_sources <- c("wrangled", "master", "proteomics", "metabolomics_rpos", "metabolomics_rneg",
  "metabolomics_xcms", "transcriptomics_t0", "transcriptomics_t1", "apoe4")

# Person-level tables joined on subject_id for cross-domain rank correlations.
ci_panels <- list(
  baseline = c("clinic_data/clinical_tests/clinical_tests_t0", "clinic_data/assay/assay_t0"),
  followup = c("clinic_data/clinical_tests/clinical_tests_t1", "clinic_data/assay/assay_t1")
)

# Baseline and follow-up versions of the same table, compared column by column.
ci_visit_families <- list(
  clinical_tests = c("clinic_data/clinical_tests/clinical_tests_t0", "clinic_data/clinical_tests/clinical_tests_t1"),
  assay = c("clinic_data/assay/assay_t0", "clinic_data/assay/assay_t1"),
  health_survey = c("health_survey/survey_t0_raw", "health_survey/survey_t1_raw")
)

# Intervals between dates held in different tables. "master" is
# combined_data_set.rds; the others are paths in ge_lc_wrangled.rds.
ci_timing_spec <- data.frame(
  label = c("onset_to_registration", "registration_to_baseline_clinic", "onset_to_baseline_blood",
    "baseline_to_followup_clinic", "baseline_to_followup_blood", "onset_to_followup_blood"),
  from_table = c("master", "master", "master", "clinic_data/clinical_tests/clinical_tests_t0",
    "clinic_data/assay/assay_t0", "master"),
  from_col = c("onset_date_harmonised", "registration_date", "onset_date_harmonised", "clinic_date",
    "sample_collected", "onset_date_harmonised"),
  to_table = c("master", "clinic_data/clinical_tests/clinical_tests_t0", "clinic_data/assay/assay_t0",
    "clinic_data/clinical_tests/clinical_tests_t1", "clinic_data/assay/assay_t1", "clinic_data/assay/assay_t1"),
  to_col = c("registration_date", "clinic_date", "sample_collected", "clinic_date", "sample_collected",
    "sample_collected"),
  stringsAsFactors = FALSE
)

# Anchor variables from the master table: the first column found is used.
# main_cohort (in subject_ids_main_cohort.rds or not) is added by the kit.
ci_anchor_spec <- list(
  sex = c("sex", "gender_group", "gender"),
  age_group = c("age_at_registration_group", "age_at_first_clinic_group"),
  lc_group = c("lc_categorical", "lc_ever_categorical"),
  route = "registered_under",
  study = "study_cat",
  ethnicity = c("ethnic_new", "ethnicity")
)

# Folders listed (names, file counts and sizes only) to find data sets the
# pipeline does not read. Depth is counted from each root.
ci_discovery_roots <- function(s) {
  data <- c("metadata", "transcriptomics", "proteomics", "metabolomics", "genomics",
    "Signaling-Lipids-Analysis", "autoantibodies", "cog_data")
  roots <- c(stats::setNames(file.path(s$data_root, data), paste0("data_root/", data)),
    "saved_root/ge_lc" = file.path(s$saved_root, "ge_lc"))
  list(deep = roots, shallow = c(data_root = s$data_root, saved_root = s$saved_root))
}
