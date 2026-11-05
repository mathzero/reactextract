# 01_build_clinical_profile.R --------------------------------------------------
# Build inst/extdata/clinical-profile.rds, the profile behind synthetic
# react_clinical() and react_clinical_omics().
#
# Input (provenance in scripts/clinical/README.md):
# - REACT_CLINICAL_CANDIDATE: the folder of the clinical inventory kit's v1.2
#   candidate (holding inventory/ and profile/), returned from the enclave
#   through output checking. It stays local and is never committed; the
#   profile records the checksums of its files.
#
# Run from the package root, without the renv profile:
#   REACT_CLINICAL_CANDIDATE=/path/to/clinical-inventory-v1.2-candidate \
#     Rscript --no-init-file scripts/clinical/01_build_clinical_profile.R
# Then pin the printed SHA-256 as .clinical_profile_sha256 in
# R/clinical_profile.R, review it (scripts/clinical/DISCLOSURE_REVIEW.md) and
# record approval in inst/extdata/clinical-profile-approval.csv.

rm(list = ls(all = TRUE))

candidate <- Sys.getenv("REACT_CLINICAL_CANDIDATE")
if (!nzchar(candidate) || !dir.exists(candidate)) {
  stop("Set REACT_CLINICAL_CANDIDATE to the clinical inventory v1.2 candidate folder.", call. = FALSE)
}
out_path <- "inst/extdata/clinical-profile.rds"
release <- "clinical-profile-v1"
kit <- "clinical-inventory-v1.2"

suppressMessages(pkgload::load_all(".", quiet = TRUE))


# 1. Read and check the candidate ------------------------------------------------

# reading verifies every file against the candidate's manifest.csv
profile <- react_clinical_profile(candidate)
if (!identical(profile$kit, kit)) stop("Expected a ", kit, " candidate; found ", profile$kit, ".", call. = FALSE)
if (any(profile$source_status$status != "ok")) {
  stop("Every source must have status ok: ",
       paste(profile$source_status$source[profile$source_status$status != "ok"], collapse = ", "), call. = FALSE)
}


# 2. The release profile --------------------------------------------------------

files <- c("manifest.csv", profile$sources$file)
sources <- data.frame(
  file = files,
  origin = paste(kit, "candidate (REACT aggregates, output-checked; local only)"),
  sha256 = vapply(file.path(candidate, files), .sha256_file, character(1L), USE.NAMES = FALSE),
  stringsAsFactors = FALSE
)
out <- .clinical_profile_release(profile, release, sources)

# Labels coarsened after the disclosure review (5 November 2026): the clinic
# sites at baseline and follow-up and the vaccination providers' organisation
# codes are geography, and the Olink kit lot number isn't needed. They become
# neutral labels numbered by count; no mapping is kept, and the check below
# stops the build if any original label survives anywhere in the profile.
neutral <- c(site_name = "Site %02d", clinic_location = "LC site %02d", sending_org_code = "ORG%02d",
             panel_lot_nr = "LOT%02d")
original <- unique(out$categorical$level[out$categorical$column %in% names(neutral)])
out <- .clinical_neutral_levels(out, neutral)
values <- unlist(lapply(out, function(x) {
  if (is.data.frame(x)) unlist(x[vapply(x, is.character, logical(1L))], use.names = FALSE) else
    as.character(unlist(x, use.names = FALSE))
}), use.names = FALSE)
if (any(values %in% original)) stop("A coarsened label survives in the profile.", call. = FALSE)
rm(original, values)


# 3. Disclosure checks (H09: no counts below 10) ---------------------------------

# Counts of people or rows must be missing (suppressed), zero (suppressed and
# treated as zero) or at least 10. Feature, column and level counts (such as
# n_features or n_distinct) aren't counts of people.
count_columns <- list(
  structure = "n_rows",
  tables = c("n_rows", "n_persons", "n_persons_main", "rpp_1", "rpp_2", "rpp_3_5", "rpp_6_10", "rpp_11_plus"),
  columns = c("n_nonmissing", "n_missing"),
  numeric = c("n", grep("^n_code_", names(out$numeric), value = TRUE)),
  categorical = "n_rows",
  availability_patterns = "n_rows",
  date_summary = c("n", "n_placeholder"),
  date_months = "n_rows",
  id_shapes = "n_rows",
  omics_platform = c("n_samples_source", "n_samples_profiled", "n_samples_t0", "n_samples_t1")
)
problems <- character()
for (name in names(count_columns)) {
  for (column in intersect(count_columns[[name]], names(out[[name]]))) {
    x <- out[[name]][[column]]
    small <- !is.na(x) & x > 0 & x < 10
    if (any(small)) problems <- c(problems, sprintf("%s$%s: %d value(s) between 1 and 9", name, column, sum(small)))
  }
}
if (length(problems) > 0L) stop("Counts below 10 in the release profile:\n", paste(problems, collapse = "\n"),
                                call. = FALSE)


# 4. Write ------------------------------------------------------------------------

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(out, out_path, compress = "xz", version = 3)
message("Wrote ", out_path, " (", round(file.size(out_path) / 1e6, 1), " MB), SHA-256 ", .sha256_file(out_path))
message("Rows by table: ", paste(sprintf("%s %d", names(out)[vapply(out, is.data.frame, TRUE)],
                                          vapply(out[vapply(out, is.data.frame, TRUE)], nrow, 1L)),
                                  collapse = ", "))
