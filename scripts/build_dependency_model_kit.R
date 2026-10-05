# Run from reactextract: Rscript --vanilla scripts/build_dependency_model_kit.R [fresh-output]
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("Supply at most one fresh output directory.")
kit <- "inst/enclave/dependency-model-v1"
wiki <- normalizePath("../react_wiki", mustWork = TRUE)
copy <- function(from, to) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(from, to, overwrite = TRUE)) stop("Copy failed: ", from)
}
contract <- "inst/enclave/participation-v6-reexport/contract"
for (f in list.files(contract)) copy(file.path(contract, f), file.path(kit, "contract", f))
rules <- file.path(wiki, "metadata/synthetic-rules-v1")
for (f in list.files(rules)) copy(file.path(rules, f), file.path(kit, "rules", f))
copy("inst/enclave/participation-v6-reexport/profile.R", file.path(kit, "vendor/profile.R"))
for (f in c("synthetic.R", "eligibility_context.R", "response_options.R", "synthetic_rules.R")) copy(file.path("R", f), file.path(kit, "vendor", f))
copy(file.path(wiki, "docs/SYNTHETIC_DEPENDENCY_MODEL_STEP1.md"), file.path(kit, "specification.md"))
files <- sort(setdiff(list.files(kit, recursive = TRUE), "kit-checksums.csv"))
if (any(grepl("[.]rds$|INTERNAL|candidate", files))) stop("Unexpected results in code-only kit.")
hashes <- data.frame(file = files, sha256 = vapply(file.path(kit, files), function(f) digest::digest(file = f, algo = "sha256"), character(1)))
write.csv(hashes, file.path(kit, "kit-checksums.csv"), row.names = FALSE)
if (length(args) && args[1] == "--prepare-only") {
  message("Prepared source kit checksums only.")
  quit(status = 0L)
}
dest <- if (length(args)) args[1] else "dist/reactextract-dependency-model-kit-v1"
if (file.exists(dest) || dir.exists(dest) || file.exists(paste0(dest, ".zip"))) stop("Choose a fresh destination; existing kits are not overwritten.")
dir.create(dest, recursive = TRUE)
for (f in c(files, "kit-checksums.csv")) copy(file.path(kit, f), file.path(dest, f))
dest <- normalizePath(dest)
old <- setwd(dirname(dest))
status <- utils::zip(paste0(basename(dest), ".zip"), file.path(basename(dest), c(files, "kit-checksums.csv")))
setwd(old)
if (status != 0L) stop("Zip creation failed.")
digest <- digest::digest(file = paste0(dest, ".zip"), algo = "sha256")
writeLines(paste(digest, basename(paste0(dest, ".zip"))), paste0(dest, ".zip.sha256"))
message("Code-only kit: ", dest, ".zip")
