args <- commandArgs(trailingOnly = TRUE)
project <- normalizePath(if (length(args) > 0L) args[[1]] else ".", mustWork = TRUE)
old <- setwd(project)
on.exit(setwd(old), add = TRUE)

status <- system2(file.path(R.home("bin"), "R"), c("CMD", "build", "--no-manual", "."))
if (!identical(status, 0L)) {
  stop("R CMD build failed; offline bundle was not created.", call. = FALSE)
}

description <- read.dcf("DESCRIPTION")
tarball <- paste0(description[1, "Package"], "_", description[1, "Version"], ".tar.gz")
if (!file.exists(tarball)) stop("Built source tarball is missing.", call. = FALSE)

output <- file.path("dist", paste0("reactextract-", description[1, "Version"], "-offline"))
if (dir.exists(output)) {
  stop("Offline output already exists; choose a fresh release/version. Existing enclave results were not touched.")
}
dir.create(output, recursive = TRUE, showWarnings = FALSE)
copied <- c(
  file.copy(tarball, output, overwrite = TRUE),
  file.copy("renv.lock", output, overwrite = TRUE),
  file.copy("inst/ENCLAVE_INSTALL.md", output, overwrite = TRUE),
  file.copy("inst/DERIVED_VARIABLES.md", output, overwrite = TRUE),
  file.copy("inst/PARTICIPATION_V6_DECISIONS.md", output, overwrite = TRUE),
  file.copy("inst/extdata/dependency-models-v1.rds", output, overwrite = TRUE),
  file.copy("inst/extdata/dependency-models-v1-approval.csv", output, overwrite = TRUE),
  file.copy("inst/enclave", output, recursive = TRUE, overwrite = TRUE)
)
if (!all(copied)) {
  stop("Failed to assemble one or more offline bundle files.", call. = FALSE)
}
baseline_profile <- file.path(output, "react-synthetic-profile-v5")
dir.create(baseline_profile, recursive = TRUE, showWarnings = FALSE)
utils::untar("inst/extdata/synthetic-profile.tar.gz", exdir = baseline_profile)
if (!file.exists(file.path(baseline_profile, "manifest.csv"))) {
  stop("Failed to include the approved v5 synthetic profile.", call. = FALSE)
}
participation <- file.path(output,"react-participation-conditional-v2-approved")
dir.create(participation)
utils::untar("inst/extdata/participation-profile.tar.gz",exdir=participation)
if(!file.exists(file.path(participation,"approval.csv"))) stop("Participation approval record missing.")
rules <- file.path(output,"synthetic-rules-v1")
dir.create(rules)
utils::untar("inst/extdata/synthetic-rules.tar.gz",exdir=rules)
if(!file.exists(file.path(rules,"approvals.csv"))) stop("Questionnaire rule approval records missing.")

source("R/dictionary.R", local = TRUE)
files <- sort(list.files(output, recursive = TRUE, full.names = TRUE))
files <- files[!file.info(files)$isdir]
relative_files <- substring(files, nchar(output) + 2L)
relative_files <- gsub("\\\\", "/", relative_files)
manifest <- data.frame(
  file = relative_files,
  sha256 = vapply(files, .sha256_file, character(1)),
  byte_size = as.numeric(file.info(files)$size),
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, file.path(output, "manifest.csv"), row.names = FALSE, na = "")
message("Offline bundle written to ", normalizePath(output, mustWork = TRUE))
