# Invented data laid out like the enclave sources in sources.R, for the self
# test. Nothing here comes from REACT data: values are random draws, and the
# structure follows the wrangling code (code/revised/01 and 02). Sentinels
# (rare levels, free text, extreme values, one-person months, a tiny
# non-REACT branch) must never reach the exported candidate. v1.1 adds the
# layouts that stopped the first enclave run: Windows-1252 free text, long
# proteomics tables with empty name columns, a CSV with an unnamed column,
# and expression matrices named by BioSample.

ci_fixture_sentinels <- c("SENTINEL", "RareSentinelLevel", "987.654", "987.65", "987.7", "123.456", "2014-03")

ci_fixture_tree <- function(root, n = 600L, seed = 20261102L) {
  set.seed(seed)
  saved <- file.path(root, "saved_objects")
  data <- file.path(root, "react_data")
  project <- file.path(root, "project")
  meta <- file.path(data, "metadata")
  omics <- file.path(saved, "ge_lc", "ge_lc_omics_objects")
  for (d in c(meta, omics, project, file.path(saved, "phier_react_data_2025"), file.path(data, "cog_data"),
    file.path(data, "Signaling-Lipids-Analysis"), file.path(data, "autoantibodies"),
    file.path(data, "transcriptomics", "manifests"), file.path(data, "genomics", "REACT_PGEN"),
    file.path(data, "genomics", "genomics", "data"), file.path(data, "proteomics", "olink"))) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  ids <- list()
  pick <- function(x, k, ...) sample(x, k, replace = TRUE, ...)

  # people: numeric subject IDs, registration and follow-up subsets
  subject <- 7000000 + seq_len(n)
  cohort <- subject[seq_len(round(n * 0.9))]
  fu <- sort(sample(cohort, round(length(cohort) * 0.3)))
  barcode_t0 <- stats::setNames(sprintf("GCCQ%06d", seq_len(n)), subject)
  barcode_t1 <- stats::setNames(sprintf("LCRQ%06d", match(fu, subject)), fu)
  passcode <- stats::setNames(sprintf("PZ%06d", seq_len(n)), subject)
  ids$subject <- sprintf("%.0f", subject)
  ids$barcode <- c(barcode_t0, barcode_t1)
  ids$passcode <- passcode
  sex <- pick(c("Male", "Female"), n)
  age <- round(stats::runif(n, 18, 85))
  age_group <- as.character(cut(age, c(17, 24, 34, 44, 54, 64, 74, 200),
    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")))
  lc <- pick(c("Asymptomatic", "Non-persistent symptoms", "Long COVID"), n, prob = c(0.2, 0.5, 0.3))
  route <- pick(c("GE", "LC"), n)
  reg_date <- as.Date("2021-01-10") + pick(0:200, n)
  onset <- reg_date - pick(30:500, n)
  duration <- c("Less than 2 weeks", "2-4 weeks", "1-3 months", "3-6 months", "6+ months")

  # registration with free text and a symptom block
  other_text <- rep(NA_character_, n)
  other_text[1:40] <- paste("SENTINEL free text", 1:40)
  # Windows-1252 bytes (e acute, curly quotes) that are not valid UTF-8
  cp1252 <- c("caf\xe9 SENTINEL", "\x93SENTINEL\x94 note", "na\xefve SENTINEL")
  add_comm <- rep(NA_character_, n)
  add_comm[sample(n, 120)] <- paste(pick(cp1252, 120), seq_len(120))
  date_other <- rep(NA_character_, n)
  date_other[sample(n, 200)] <- format(as.Date("2020-01-01") + pick(0:700, 200))
  reg <- data.frame(subject_id = subject, u_password = sprintf("UPW%06d", seq_len(n)), registration_date = reg_date,
    registered_under = route, symptoms_present_01 = pick(c(0, 1, NA), n), symptoms_duration_01 = pick(c(duration, NA), n),
    symptoms_present_02 = pick(c(0, 1), n), lc_categorical = lc, symptoms_other_text = other_text,
    age_at_registration = age, gender_group = sex, add_comm = add_comm, symptoms_date_other = date_other,
    stringsAsFactors = FALSE)
  clinic <- function(who, visit) {
    k <- length(who)
    d <- data.frame(subject_id = who, clinic_date = as.Date(if (visit == 0) "2021-03-01" else "2021-10-20") + pick(0:150, k),
      height = round(stats::rnorm(k, 170, 9), 1), weight = round(stats::rnorm(k, 78, 15), 1),
      sbp = round(stats::rnorm(k, 128, 16)), dbp = round(stats::rnorm(k, 80, 10)), pulse = round(stats::rnorm(k, 70, 11)),
      grip = round(stats::rnorm(k, 32, 9), 1), fev1 = round(stats::rnorm(k, 3.1, 0.7), 2),
      site = pick(c("Prescot", "Warwick", "Southend", "Luton"), k), stringsAsFactors = FALSE)
    d$weight[d$weight < 35] <- 35
    d
  }
  clinic_t0 <- clinic(cohort, 0)
  clinic_t0$weight[1] <- 987.654
  clinic_t0$clinic_date[2] <- as.Date("2014-03-15")
  clinic_t1 <- clinic(fu, 1)
  assay <- function(who, barcodes, visit) {
    k <- length(who)
    data.frame(subject_id = who, barcode = unname(barcodes[as.character(who)]),
      sample_collected = as.POSIXct(as.Date(if (visit == 0) "2021-03-01" else "2021-10-20") + pick(0:150, k)),
      hba1c = round(stats::rlnorm(k, log(37), 0.12)), cholesterol = round(stats::rnorm(k, 5.1, 1), 1),
      hdl = round(stats::rnorm(k, 1.5, 0.4), 2), triglycerides = round(stats::rlnorm(k, log(1.3), 0.4), 2),
      covid_outcome = pick(c("Positive", "Negative", "N/A"), k, prob = c(0.6, 0.35, 0.05)),
      igg_index = round(stats::rlnorm(k, 1, 1.2), 2),
      clinic_location = pick(c("GE Prescot", "LC Warwick", "GE Luton"), k), stringsAsFactors = FALSE)
  }
  assay_t0 <- assay(cohort, barcode_t0, 0)
  assay_t1 <- assay(fu, barcode_t1, 1)
  likert <- c("Not at all", "Several days", "More than half the days", "Nearly every day")
  survey <- function(who, raw) {
    k <- length(who)
    codes <- replicate(6, pick(c(1:4, -77, -91), k, prob = c(0.4, 0.3, 0.15, 0.1, 0.03, 0.02)))
    d <- data.frame(subject_id = who, stringsAsFactors = FALSE)
    for (j in 1:6) d[[paste0("gad7_", j)]] <- if (raw) codes[, j] else ifelse(codes[, j] > 0, likert[pmax(codes[, j], 1)], NA)
    d$wemwbs_total <- round(stats::rnorm(k, 48, 9))
    d$rare_choice <- if (raw) c(rep(2, 3), rep(1, k - 3)) else c(rep("RareSentinelLevel", 3), rep("Common answer", k - 3))
    d$evbirth <- if (raw) pick(1:2, k) else pick(c("Yes", "No"), k)
    d$numbirth_num <- pick(0:4, k)
    d
  }
  illumina <- function(who, barcodes, stage) data.frame(passcode = unname(passcode[as.character(who)]),
    barcode = unname(barcodes[as.character(who)]), project_stage = stage,
    plate = pick(sprintf("PL%02d", 1:6), length(who)), subject_id = who, stringsAsFactors = FALSE)
  non_react <- 7100000 + 1:8
  nonreact_clinic <- clinic(non_react, 0)
  nonreact_clinic$weight[1] <- 123.456
  rounds <- do.call(rbind, lapply(subject, function(s) {
    r <- sort(sample(1:19, sample(1:4, 1)))
    data.frame(subject_id = s, U_PASSCODE = unname(passcode[as.character(s)]), round = r,
      estbinres = pick(0:1, length(r)), U_AGE = age[subject == s], stringsAsFactors = FALSE)
  }))
  rounds$add_comm <- NA_character_
  rounds$add_comm[sample(nrow(rounds), 150)] <- paste(pick(cp1252, 150), seq_len(150))
  rounds$react_infection_date_raw <- as.Date("2020-09-01") + pick(0:500, nrow(rounds))
  # placeholders as found in the enclave: the SPSS zero date a day early, and 0001-01-01
  placeholder_rows <- sample(nrow(rounds), 40)
  rounds$react_infection_date_raw[placeholder_rows] <- as.Date(rep(c("1582-10-13", "0001-01-01"), each = 20))
  vaccines <- do.call(rbind, lapply(subject[seq_len(round(n * 0.8))], function(s) {
    k <- sample(1:4, 1)
    data.frame(subject_id = s, recorded_date = as.Date("2021-01-01") + sort(sample(0:500, k)),
      product = pick(c("Pfizer", "AstraZeneca", "Moderna"), k), dose_number = seq_len(k))
  }))
  pillar2 <- do.call(rbind, lapply(subject[seq_len(round(n * 0.7))], function(s) {
    k <- sample(1:6, 1)
    data.frame(subject_id = s, teststartdate = as.Date("2020-06-01") + sort(sample(0:600, k)),
      testresult = pick(c("SCT:1240581000000104", "SCT:1322791000000100"), k),
      processinglabcode = pick(c("LFDSelfTest", "LAB01", "LAB02"), k), ch1cq = round(stats::runif(k, 15, 40), 1))
  }))
  dictionary <- data.frame(field = rep(paste0("gad7_", 1:6), each = 4), code = rep(1:4, 6),
    option = rep(likert, 6), stringsAsFactors = FALSE)
  in_react <- function(d) d[d$subject_id %in% subject, , drop = FALSE]
  wrangled <- list(
    registration_data = reg,
    clinic_data = list(
      clinical_tests = list(clinical_tests_t0 = clinic_t0, clinical_tests_t1 = clinic_t1),
      assay = list(assay_t0 = assay_t0, assay_t1 = assay_t1),
      illumina = list(illumina_t0 = illumina(cohort, barcode_t0, "GE"), illumina_t1 = illumina(fu, barcode_t1, "LCR"))),
    health_survey = list(survey_t0 = survey(cohort, FALSE), survey_t1 = survey(fu, FALSE),
      survey_t0_raw = survey(cohort, TRUE), survey_t1_raw = survey(fu, TRUE), data_dictionary = dictionary),
    original_react_data = rounds,
    infection_dates = data.frame(subject_id = subject, react_infection_date = onset - 3,
      onset_date_harmonised = onset, onset_date_source = pick(c("registration", "react_history"), n)),
    infection_date_rules = list(min_date = as.Date("2019-12-01"), max_date = as.Date("2026-10-12")),
    registration_symptom_selection_qc = data.frame(pathway = c("GE", "LC"), status = "ok", n = c(5000, 8000)),
    nhs_record_age_gender_ethnicity = data.frame(subject_id = subject, gender_group = sex, age_jan2024 = age + 3,
      ethnicity = pick(c("White", "Asian", "Black", "Mixed", "Other"), n, prob = c(0.8, 0.08, 0.05, 0.04, 0.03)),
      nhs_linkage_status = "linked"),
    non_reacters = list(registration_data = data.frame(subject_id = non_react, registration_date = Sys.Date(),
      registered_under = "LC"), clinic_data = list(clinical_tests = list(clinical_tests_t0 = nonreact_clinic))),
    nhs_vaccine_data = in_react(vaccines),
    pillar_ii_data = in_react(pillar2)
  )
  ids$subject <- c(ids$subject, sprintf("%.0f", non_react))
  ids$u_password <- reg$u_password
  saveRDS(wrangled, file.path(meta, "ge_lc_wrangled.rds"))

  # master table with availability flags (all original REACT registrants)
  yn <- function(x) ifelse(x, "Yes", "No")
  olink_people <- sample(cohort, 200)
  master <- data.frame(subject_id = subject, registration_date = reg_date, onset_date_harmonised = onset,
    age_at_registration = age, age_at_registration_group = age_group, sex = sex,
    ethnic_new = wrangled$nhs_record_age_gender_ethnicity$ethnicity,
    imd_quintile_cat = pick(as.character(1:5), n), registered_under = route,
    study_cat = pick(c("REACT-1", "REACT-2"), n), lc_categorical = lc,
    attended_baseline_clinic = yn(subject %in% cohort), attended_followup_clinic = yn(subject %in% fu),
    has_blood_biochem_t0 = yn(subject %in% cohort), has_blood_biochem_t1 = yn(subject %in% fu),
    has_proteins_olink_t0 = yn(subject %in% olink_people), has_proteins_olink_t1 = yn(subject %in% intersect(olink_people, fu)),
    has_genome = yn(subject %in% sample(cohort, 400)), has_linked_HES_data = yn(stats::runif(n) < 0.6),
    has_linked_GP_data = yn(stats::runif(n) < 0.95), has_cog_1 = yn(stats::runif(n) < 0.4),
    has_2022_data = yn(stats::runif(n) < 0.5), add_comm = add_comm, stringsAsFactors = FALSE)
  saveRDS(master, file.path(meta, "combined_data_set.rds"))
  saveRDS(cohort, file.path(project, "subject_ids_main_cohort.rds"))
  lookup <- data.frame(subject_id = c(subject, fu), barcode = c(unname(barcode_t0), unname(barcode_t1)))
  saveRDS(lookup, file.path(meta, "ge_lc_sample_lookup.rds"))
  saveRDS(list(all = list(Registered = subject, `Has blood chemistry` = cohort),
    main = list(Registered = cohort, `Has blood chemistry` = cohort, `Has any omics data` = olink_people),
    omics = list(Registered = subject, `Has proteins Alamar` = olink_people)), file.path(meta, "cohort_availability_sets.rds"))
  saveRDS(list(run_id = "20261012T101010", output_hash = "0123456789abcdef0123456789abcdef",
    sample_lookup_hash = "fedcba9876543210fedcba9876543210"), file.path(meta, "ge_lc_wrangled_provenance.rds"))
  saveRDS(list(created_at = as.POSIXct("2026-10-12 11:00:00"), extraction_run_id = "20261012T101010",
    input_hashes = c("D:/saved_objects/react1_and_2_combined_wrangled_deduped.rds" = "00aa"),
    session = utils::sessionInfo()), file.path(meta, "cohort_provenance.rds"))

  # proteomics, as olink_alamar_lcms_combined.rds: long Olink (NPX), Alamar
  # (NPQ, raw and normalised) and LC-MS (raw and normalised) tables, where
  # protein_name is empty or partly filled, wide LC-MS, and df_allplats
  # stacking the three long tables
  op <- olink_people
  op_t1 <- intersect(op, fu)
  long_omics <- function(who, k, prefix, scale) {
    t1 <- intersect(who, fu)
    smp <- data.frame(subject_id = c(who, t1), timepoint = c(rep("First visit", length(who)), rep("Second visit", length(t1))),
      sample_id = c(unname(barcode_t0[as.character(who)]), unname(barcode_t1[as.character(t1)])),
      plate_id = pick(sprintf("Plate%d", 1:4), length(who) + length(t1)), stringsAsFactors = FALSE)
    d <- smp[rep(seq_len(nrow(smp)), each = k), ]
    d$protein_cleaned <- rep(sprintf("%s protein %02d", prefix, seq_len(k)), nrow(smp))
    latent <- stats::rnorm(nrow(smp))
    d$value <- round(rep(latent, each = k) * 0.8 + stats::rnorm(nrow(d), scale, 1), 3)
    d$lod <- rep(round(stats::runif(k, 0.5, 1.5), 2), nrow(smp))
    rownames(d) <- NULL
    d
  }
  olink <- long_omics(op, 40, "Olink", 3)
  olink$olink_id <- sprintf("OID%05d", match(olink$protein_cleaned, unique(olink$protein_cleaned)))
  olink$assay <- toupper(sub(" protein ", "", olink$protein_cleaned))
  names(olink)[names(olink) == "value"] <- "npx"
  olink$qc_warning <- pick(c("PASS", "PASS", "PASS", "WARN"), nrow(olink))
  alamar <- long_omics(sample(cohort, 150), 30, "Alamar", 12)
  alamar$target_name <- alamar$protein_cleaned
  alamar$protein_name <- ifelse(stats::runif(nrow(alamar)) < 0.5, alamar$protein_cleaned, NA)
  alamar$npq <- alamar$value
  alamar$npq_normalised <- round(alamar$value - stats::ave(alamar$value, alamar$plate_id) + 12, 3)
  alamar$value <- NULL
  lcms_long <- long_omics(cohort, 50, "LCMS", 20)
  lcms_long$protein_name <- NA_character_
  lcms_long$protein_original_name <- lcms_long$protein_cleaned
  lcms_long$value_normalised <- round(lcms_long$value - stats::ave(lcms_long$value, lcms_long$plate_id) + 20, 3)
  lcms <- reshape(lcms_long[c("subject_id", "timepoint", "sample_id", "protein_cleaned", "value")],
    idvar = c("subject_id", "timepoint", "sample_id"), timevar = "protein_cleaned", direction = "wide")
  names(lcms) <- sub("^value[.]LCMS protein ", "lcms_", names(lcms))
  names(lcms)[names(lcms) == "sample_id"] <- "sample_name"
  lcms$platform <- "LC-MS"
  stack <- function(d, platform, value) data.frame(platform = platform, sample_id = d$sample_id, subject_id = d$subject_id,
    timepoint = d$timepoint, protein_cleaned = d$protein_cleaned, value = d[[value]], stringsAsFactors = FALSE)
  allplats <- rbind(stack(olink, "Olink", "npx"), stack(alamar, "Alamar", "npq"), stack(lcms_long, "LC-MS", "value"))
  allplats$protein_name <- ifelse(allplats$platform == "Alamar", allplats$protein_cleaned, NA)
  # a protein lookup and a QC log beside the omics tables
  prot_names <- unique(allplats[c("platform", "protein_cleaned")])
  prot_names$protein_label <- toupper(prot_names$protein_cleaned)
  qc_log <- data.frame(sample_id = unique(olink$sample_id), plate_id = pick(sprintf("Plate%d", 1:4), length(unique(olink$sample_id))),
    qc_status = pick(c("PASS", "PASS", "WARN"), length(unique(olink$sample_id))), stringsAsFactors = FALSE)
  proteomics <- list(df_allplats = allplats, df_olink = olink, df_alamar = alamar, df_lcms = lcms, df_lcms_long = lcms_long,
    prot_names_lookup = prot_names, qc_log = qc_log)
  saveRDS(proteomics, file.path(omics, "olink_alamar_lcms_combined.rds"))

  # metabolomics: an unnamed row-number column, 21 metadata columns, then
  # features from column 23 (PeakPantheR layout)
  metab <- function(mode, k) {
    who <- sample(cohort, round(length(cohort) * 0.8))
    t1 <- intersect(who, fu)
    sid <- c(unname(barcode_t0[as.character(who)]), unname(barcode_t1[as.character(t1)]), sprintf("SR_QC_%02d", 1:12))
    m <- length(sid)
    d <- data.frame(`Sample File Name` = sprintf("REACT_%s_%05d", mode, seq_len(m)), `Batch` = pick(1:5, m),
      `Run Order` = seq_len(m), `Acquired Time` = format(as.POSIXct("2024-03-01") + seq_len(m) * 600),
      `Sample ID` = sid, `Subject ID` = c(sprintf("S%06d", seq_len(m - 12)), rep(NA, 12)),
      `Sample Type` = c(rep("Study Sample", m - 12), rep("Study Reference", 12)), check.names = FALSE, stringsAsFactors = FALSE)
    for (j in 8:21) d[[sprintf("meta_%02d", j)]] <- pick(c("a", "b"), m)
    batch_shift <- d$Batch * 0.3
    for (j in seq_len(k)) d[[sprintf("%s_feature_%03d", mode, j)]] <- round(exp(8 + batch_shift + stats::rnorm(m, 0, 1)), 1)
    utils::write.csv(d, metab_path(mode))
    c(d[["Sample ID"]], d[["Subject ID"]], d[["Sample File Name"]])
  }
  metab_path <- function(mode) {
    run <- paste0("REACT_LCMS_P_", mode, "_PeakPantheR_2024-12-13")
    dir <- file.path(data, "metabolomics", "metabolomics_lc_ms", run, run)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    file.path(dir, paste0("REACT_LCMS_P_", mode, "_PeakPantheR_combinedData.csv"))
  }
  ids$metab <- c(metab("RPOS", 40), metab("RNEG", 60))

  # identifiers: REACT passcodes, 2025 users, Cognitron codes, lipid and autoantibody sample names
  lipid_people <- sample(cohort, 300)
  lipid_names <- sprintf("LIPQ%06d", seq_along(lipid_people))
  aab_people <- sample(cohort, 250)
  aab_names <- sprintf("AABQ%06d", seq_along(aab_people))
  user25 <- sprintf("U25Q%06d", seq_len(n))
  cog_codes <- sprintf("COGQ%06d", seq_len(n))
  identifiers <- data.frame(subject_id = c(subject, subject, subject, lipid_people, aab_people),
    project_name = rep(c("REACT", "PHIER2025", "COGNITRON", "LIPIDS", "AAB"), c(n, n, n, length(lipid_people), length(aab_people))),
    project_identifier = c(unname(passcode), user25, cog_codes, lipid_names, aab_names), stringsAsFactors = FALSE)
  saveRDS(identifiers, file.path(saved, "subject_identifiers_gelc.rds"))
  ids$other <- c(user25, cog_codes, lipid_names, aab_names)

  # lipids with "<LLOQ" tokens, saved by stage 02 with subject_id attached
  lip <- data.frame(subject_id = lipid_people, timepoint = "t0", Sample.Name = lipid_names,
    Batch = pick(c("L1", "L2"), length(lipid_people)), stringsAsFactors = FALSE)
  for (j in 1:30) {
    v <- format(round(exp(stats::rnorm(nrow(lip), 0, 1)), 3))
    v[stats::runif(nrow(lip)) < 0.08] <- "<LLOQ"
    lip[[sprintf("Lipid_%02d", j)]] <- v
  }
  saveRDS(lip, file.path(data, "Signaling-Lipids-Analysis", "lipids_with_subject_id.rds"))

  # transcriptomics: tximport-style lists, with unmapped VITT samples at baseline
  tx <- function(barcodes) {
    genes <- sprintf("ENSG%011d", 1:300)
    mu <- stats::rlnorm(length(genes), 2, 1.5)
    counts <- matrix(stats::rpois(length(genes) * length(barcodes), rep(mu, length(barcodes))), length(genes),
      dimnames = list(genes, barcodes))
    abundance <- sweep(counts, 2, colSums(counts), "/") * 1e6
    list(abundance = abundance, counts = counts, length = counts * 0 + 1500, countsFromAbundance = "no")
  }
  tx_t0_people <- sample(cohort, 200)
  vitt <- sprintf("VITTQ%05d", 1:20)
  tx_codes <- c(unname(barcode_t0[as.character(tx_t0_people)]), unname(barcode_t1[as.character(intersect(tx_t0_people, fu))]))
  biosample <- stats::setNames(sprintf("RNAQ%06d", seq_along(tx_codes)), tx_codes)
  saveRDS(tx(c(unname(biosample[unname(barcode_t0[as.character(tx_t0_people)])]), vitt)), file.path(omics, "tx_baseline.rds"))
  saveRDS(tx(unname(biosample[unname(barcode_t1[as.character(intersect(tx_t0_people, fu))])])), file.path(omics, "tx_recall.rds"))
  ids$other <- c(ids$other, vitt, unname(biosample))

  # genomics: psam sample wells and the well-to-barcode lookup
  wells <- sprintf("WQ%05d", seq_len(400))
  geno_people <- sample(cohort, 400)
  writeLines(c("#IID\tSEX", paste(wells, pick(1:2, 400), sep = "\t")), file.path(data, "genomics", "REACT_PGEN", "react_chr1.psam"))
  writeLines("not a real pgen", file.path(data, "genomics", "REACT_PGEN", "react_chr1.pgen"))
  lpid <- sprintf("LPQ%07d", seq_len(400))
  utils::write.table(data.frame(`Sample Well` = wells, PatientId = unname(barcode_t0[as.character(geno_people)]),
    LPID = lpid, check.names = FALSE), file.path(data, "genomics", "genomics", "data", "REACT_Covid19_DNA_Sample_LPIDs-1.tsv"),
    sep = "\t", row.names = FALSE, quote = FALSE)
  ids$other <- c(ids$other, wells, lpid)

  # a folder with more than 25 files, listed by extension
  for (i in 1:28) writeLines("invented", file.path(data, "proteomics", "olink", sprintf("run_%02d.parquet", i)))
  for (i in 1:2) writeLines("invented", file.path(data, "proteomics", "olink", sprintf("notes_%d.csv", i)))

  # APOE4 carrier status by genotyping sample well
  dir.create(file.path(data, "genomics", "APOE4"), showWarnings = FALSE)
  apoe <- data.frame(IID = wells[1:350], apoe_genotype = pick(c("e3/e3", "e3/e4", "e2/e3", "e4/e4", "e2/e4"), 350,
    prob = c(0.6, 0.23, 0.12, 0.03, 0.02)), stringsAsFactors = FALSE)
  apoe$apoe4_carrier <- as.integer(grepl("e4", apoe$apoe_genotype))
  utils::write.csv(apoe, file.path(data, "genomics", "APOE4", "apoe4_carrier.csv"), row.names = FALSE)
  writeLines("invented", file.path(data, "genomics", "APOE4", "README.txt"))

  # untargeted XCMS intensities: samples in rows, no sample IDs
  xcms <- data.frame(batch = pick(1:4, 420), run_order = seq_len(420))
  z <- stats::rnorm(420)
  for (j in 1:80) xcms[[sprintf("M%.4fT%.1f", 100 + j * 3.1, 20 + j)]] <- round(exp(10 + 0.4 * z + stats::rnorm(420)), 1)
  utils::write.csv(xcms, file.path(data, "metabolomics", "metabolomics_lc_ms",
    "XCMS_metabolomics_intensity_data_2_noSampleID_CL.csv"), row.names = FALSE)

  # later questionnaires and Cognitron, including people outside the cohort
  outsiders <- sprintf("PX%06d", 1:300)
  f22 <- data.frame(U_PASSCODE = c(unname(passcode[seq_len(round(n / 2))]), outsiders),
    lc_now = pick(c("Yes", "No"), round(n / 2) + 300), q_fatigue = pick(c(1:5, -77), round(n / 2) + 300),
    date_completed = as.Date("2022-05-01") + pick(0:90, round(n / 2) + 300), stringsAsFactors = FALSE)
  saveRDS(f22, file.path(saved, "react_long_covid_2022_data.rds"))
  ids$other <- c(ids$other, outsiders)
  utils::write.csv(data.frame(user = user25[seq_len(round(n * 0.6))], eq5d_index = round(stats::runif(round(n * 0.6), 0.2, 1), 3),
    recovered = pick(c("Yes", "No", "Not sure"), round(n * 0.6))), file.path(saved, "phier_react_data_2025", "questionnaires.csv"),
    row.names = FALSE)
  utils::write.csv(data.frame(subject_id = c(subject, 7200000 + 1:400), task = pick(c("rs_digitSpan", "rs_motorControl"), n + 400),
    score = round(stats::rnorm(n + 400), 3), test_date = as.character(as.Date("2022-01-01") + pick(0:300, n + 400))),
    file.path(data, "cog_data", "cog_data_148k.csv"), row.names = FALSE)
  utils::write.csv(data.frame(user_code = cog_codes[seq_len(round(n * 0.5))], global_score = round(stats::rnorm(round(n * 0.5)), 3),
    timepoint = pick(1:2, round(n * 0.5))), file.path(data, "cog_data", "merged_cognitive_final.csv"), row.names = FALSE)
  ids$subject <- c(ids$subject, sprintf("%.0f", 7200000 + 1:400))

  # NHS extracts, linkage ID sets and invitations
  hes <- data.frame(SUBJECT_ID = pick(subject, 3000), ADMIDATE = as.Date("2016-01-01") + pick(0:2500, 3000),
    DIAG_4_01 = pick(sprintf("%s%02d%d", pick(LETTERS[1:20], 200), pick(0:99, 200), pick(0:9, 200)), 3000),
    ADMIMETH = pick(c("11", "12", "21", "22"), 3000))
  saveRDS(hes, file.path(saved, "gelc_hes_data.rds"))
  gp <- data.frame(SUBJECT_ID = pick(subject, 8000), RECORD_DATE = as.Date("2018-06-01") + pick(0:2000, 8000),
    CODE = pick(sprintf("%d", 100000 + 1:400), 8000), VALUE1_CONDITION = round(stats::rnorm(8000, 25, 5), 1))
  saveRDS(gp, file.path(saved, "gelc_gpes_data.rds"))
  saveRDS(list(ids_HES_out = unique(hes$SUBJECT_ID), ids_HES_IN = sample(subject, 200), ids_HES_critical = sample(subject, 5),
    ids_GDPPR = unique(gp$SUBJECT_ID), ids_vaccine = unique(vaccines$subject_id), ids_pilar_ii = unique(pillar2$subject_id)),
    file.path(saved, "subject_ids_all_nhs_linked_data.rds"))
  saveRDS(data.frame(subject_id = c(subject, 7300000 + 1:1400), invite_date = as.Date("2021-01-06") + pick(0:30, n + 1400),
    study = pick(1:2, n + 1400)), file.path(saved, "ge_lc_invites.rds"))
  ids$subject <- c(ids$subject, sprintf("%.0f", 7300000 + 1:1400))

  # workbooks need openxlsx; without it these two sources are reported missing
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    man <- data.frame(`Patient ID` = c(tx_codes, "Empty/buffer"), `BioSample Name` = c(unname(biosample), "EMPTY"),
      Gender = c(pick(c("M", "F"), length(tx_codes)), NA), `RNA Quality number` = c(round(stats::runif(length(tx_codes), 6, 10), 1), NA),
      `Sample Origin Location` = c(pick(c("Prescot", "Luton"), length(tx_codes)), NA), check.names = FALSE)
    half <- seq_len(round(nrow(man) / 2))
    openxlsx::write.xlsx(man[half, ], file.path(data, "transcriptomics", "manifests", "batch_1.xlsx"))
    openxlsx::write.xlsx(man[-half, ], file.path(data, "transcriptomics", "manifests", "batch_2.xlsx"))
    aab <- function(k, who) data.frame(`Sample ID` = aab_names[who], Code = seq_along(who),
      ANA = pick(c("POS", "NEG", "NEG", "INSUF"), k), ASMA = pick(c("POS", "NEG", "NEG"), k),
      Titre = pick(c("1:40", "1:80", "1:160", "No FU sample"), k), check.names = FALSE)
    wb <- openxlsx::createWorkbook()
    for (sh in c("notes", "key", "summary")) {
      openxlsx::addWorksheet(wb, sh)
      openxlsx::writeData(wb, sh, data.frame(note = "invented"))
    }
    controls <- aab(100, 1:100)
    header <- as.data.frame(as.list(c(names(controls))), stringsAsFactors = FALSE)
    names(header) <- names(controls)
    names(controls) <- c("X1", "X2", "X3", "X4", "X5")
    names(header) <- names(controls)
    controls_sheet <- rbind(header, data.frame(lapply(controls, as.character)))
    controls_sheet$X6 <- c("Panel", pick(c("A", "B"), 100))
    openxlsx::addWorksheet(wb, "controls")
    openxlsx::writeData(wb, "controls", controls_sheet)
    openxlsx::addWorksheet(wb, "cases")
    openxlsx::writeData(wb, "cases", aab(150, 101:250))
    openxlsx::saveWorkbook(wb, file.path(data, "autoantibodies", "REACT LC Aab data.xlsx"))
  }
  ids$other <- c(ids$other, sprintf("UPW%06d", seq_len(n)))
  ids <- unique(unlist(ids, use.names = FALSE))
  list(saved_root = saved, data_root = data, project_root = project, ids = ids[!is.na(ids) & nchar(ids) >= 5L])
}
