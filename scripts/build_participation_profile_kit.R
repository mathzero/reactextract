# Rscript --vanilla scripts/build_participation_profile_kit.R contract-directory [fresh-output-directory]
# Run at the reactextract root. Copies code/public metadata only, never dist results.
args <- commandArgs(trailingOnly=TRUE)
if (!length(args) || length(args)>2L) stop("Supply the wiki-generated contract directory and optionally a fresh output directory.")
source("R/dictionary.R")
kit <- "inst/enclave/participation-v6-profile"
contract_source <- normalizePath(args[1],mustWork=TRUE)
contract_files <- c("contract.csv","occurrence_participation.csv","age_context.csv","routing_capture.csv",
  "routing_conditions.csv","routing_targets.csv","pending_context_instructions.csv","PARTICIPATION_V6_DECISIONS.md")
hashes <- .read_literal_csv(file.path(contract_source,"checksums.csv"))
if (!setequal(hashes$file,contract_files) || anyDuplicated(hashes$file)) stop("Unexpected contract file list.")
for (i in seq_len(nrow(hashes))) if (!identical(.sha256_file(file.path(contract_source,hashes$file[i])),hashes$sha256[i])) stop("Contract checksum mismatch.")
dest <- if (length(args)>1L) args[2] else "dist/reactextract-participation-v6-profile-kit-v1"
if (file.exists(dest) || dir.exists(dest) || file.exists(paste0(dest,".zip"))) stop("Choose a new output directory; existing kits are not overwritten.")
dir.create(file.path(kit,"contract"),recursive=TRUE,showWarnings=FALSE)
if (!all(file.copy(file.path(contract_source,c(contract_files,"checksums.csv")),file.path(kit,"contract"),overwrite=TRUE))) stop("Contract copy failed.")
files <- c("00_load.R","01_run_profile.R","02_prepare_export.R","profile.R","followup-policy.R",
  "README.md","REVIEWER.md","LICENSE.md","contract/checksums.csv")
if (!all(file.exists(file.path(kit,files)))) stop("Incomplete code kit.")
utils::write.csv(data.frame(file=files,sha256=vapply(file.path(kit,files),.sha256_file,character(1))),file.path(kit,"kit-checksums.csv"),row.names=FALSE)
all_files <- c(files,"kit-checksums.csv",file.path("contract",contract_files))
dir.create(file.path(dest,"contract"),recursive=TRUE)
for (f in all_files) if (!file.copy(file.path(kit,f),file.path(dest,f))) stop("Kit copy failed.")
dest <- normalizePath(dest,mustWork=TRUE)
old <- setwd(dirname(dest))
status <- utils::zip(paste0(basename(dest),".zip"),file.path(basename(dest),all_files))
setwd(old)
if (!identical(status,0L)) stop("Could not create zip.")
cat("Code-only kit:",paste0(dest,".zip"),"\n")
