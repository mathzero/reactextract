# HES profile loading and cached sampling weights --------------------------------

# Checksum of the HES profile; its approval is recorded in
# inst/extdata/hes-profile-approval.csv
.hes_profile_sha256 <- "de180761b4c05d25fc26fe6e716bdefbdae61c1c8562ab1516c87a2a4549daaa"

# The aggregate profile behind synthetic HES: column layout of REACT's linked
# HES tables, admission-rate parameters calibrated to REACT's aggregate HES
# profile, national diagnosis and procedure frequencies (HES 2023-24) and value
# frequencies from NHS England's artificial HES. See inst/HES_SYNTHETIC.md.
.hes_profile <- function() {
  if (is.null(.reactextract_env$hes_profile)) {
    path <- system.file("extdata", "hes-profile.rds", package = "reactextract")
    if (!nzchar(path) || !identical(.sha256_file(path), .hes_profile_sha256)) {
      stop("HES profile checksum mismatch.", call. = FALSE)
    }
    .reactextract_env$hes_profile <- readRDS(path)
  }
  .reactextract_env$hes_profile
}

.hes_cached <- function(key, make) {
  key <- paste0("hes|", key)
  value <- .reactextract_env[[key]]
  if (is.null(value)) {
    value <- make()
    assign(key, value, envir = .reactextract_env)
  }
  value
}

# share of a code's episodes that are female, as a weight for one sex
.hes_sex_weight <- function(female_share, female) {
  w <- if (female) female_share else 1 - female_share
  w[is.na(w)] <- 0.5
  w
}

# Primary diagnosis weights for one age band and sex. U codes (COVID-19 and
# other emergency-use codes) come only from the COVID-19 process, which
# follows REACT's monthly counts, so they are excluded here.
.hes_diagnosis_prob <- function(profile, band, female) {
  .hes_cached(paste(profile$version, "diagnosis", band, female, sep = "|"), function() {
    tab <- profile$diagnosis
    p <- tab$age[, band] * .hes_sex_weight(tab$female_share, female)
    p[grepl("^U", tab$codes)] <- 0
    p / sum(p)
  })
}

# Procedure weights for one age band, sex and admission type; "-" means no
# procedure and its odds are shifted to match REACT's share of episodes with one
.hes_procedure_prob <- function(profile, band, female, admission) {
  .hes_cached(paste(profile$version, "procedure", band, female, admission, sep = "|"), function() {
    tab <- profile$procedure
    overall <- colSums(tab$admission_share * tab$fce) / sum(tab$fce)
    p <- tab$age[, band] * .hes_sex_weight(tab$female_share, female) *
      tab$admission_share[, admission] / overall[[admission]]
    p <- p / sum(p)
    none <- tab$codes == "-"
    odds <- p[none] / (1 - p[none]) / profile$procedure_multiplier
    p_none <- odds / (1 + odds)
    p[none] <- 0
    p <- p / sum(p) * (1 - p_none)
    p[none] <- p_none
    p
  })
}

# Secondary diagnoses: national frequencies, tilted towards the age band and
# sex using each code's profile as a primary diagnosis where that is reliable
.hes_secondary_prob <- function(profile, band, female) {
  .hes_cached(paste(profile$version, "secondary", band, female, sep = "|"), function() {
    tab <- profile$diagnosis
    sec <- profile$secondary
    i <- match(sec$codes, tab$codes)
    reliable <- !is.na(i) & tab$fce[pmax(i, 1)] >= 200
    all_age <- colSums(tab$age)
    age_tilt <- rep(1, length(i))
    age_tilt[reliable] <- (tab$age[i[reliable], band] / tab$fce[i[reliable]]) /
      (all_age[band] / sum(all_age))
    sex_tilt <- rep(1, length(i))
    female_overall <- sum(tab$female_share * tab$fce, na.rm = TRUE) / sum(tab$fce[!is.na(tab$female_share)])
    sex_tilt[reliable] <- .hes_sex_weight(tab$female_share[i[reliable]], female) /
      (if (female) female_overall else 1 - female_overall)
    p <- sec$prob * pmin(pmax(age_tilt, 0.02), 50) * pmin(pmax(sex_tilt, 0.02), 50)
    p[grepl("^U", sec$codes)] <- 0
    p / sum(p)
  })
}
