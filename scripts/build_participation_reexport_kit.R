# Rscript --vanilla scripts/build_participation_reexport_kit.R [fresh-output-directory]
# Code/public metadata whitelist only; never copy an entire existing dist kit.
args <- commandArgs(trailingOnly=TRUE)
if(length(args)>1L) stop("Supply at most one fresh output directory.")
source("R/dictionary.R")
kit <- "inst/enclave/participation-v6-reexport"
original <- "inst/enclave/participation-v6-profile"
contract_files <- c("contract.csv","occurrence_participation.csv","age_context.csv","routing_capture.csv",
  "routing_conditions.csv","routing_targets.csv","pending_context_instructions.csv","PARTICIPATION_V6_DECISIONS.md")
dest <- if(length(args)) args[1] else "dist/reactextract-participation-v6-reexport-kit-v2"
if(file.exists(dest)||dir.exists(dest)||file.exists(paste0(dest,".zip"))) stop("Choose a fresh output directory. No kit will be overwritten.")
h <- .read_literal_csv(file.path(original,"contract/checksums.csv"))
if(!setequal(h$file,contract_files)||anyDuplicated(h$file)) stop("Unexpected public contract.")
for(i in seq_len(nrow(h))) if(!identical(.sha256_file(file.path(original,"contract",h$file[i])),h$sha256[i])) stop("Contract checksum mismatch.")
dir.create(file.path(kit,"contract"),recursive=TRUE,showWarnings=FALSE)
if(!all(file.copy(file.path(original,"contract",c(contract_files,"checksums.csv")),file.path(kit,"contract"),overwrite=TRUE))) stop("Contract copy failed.")
for(f in c("profile.R","followup-policy.R")) if(!file.copy(file.path(original,f),file.path(kit,f),overwrite=TRUE)) stop("Capture code copy failed.")
files <- c("00_load.R","01_reexport.R","reexport.R","profile.R","followup-policy.R","README.md","REVIEWER.md","LICENSE.md","contract/checksums.csv")
if(!all(file.exists(file.path(kit,files)))) stop("Incomplete re-export code kit.")
utils::write.csv(data.frame(file=files,sha256=vapply(file.path(kit,files),.sha256_file,character(1))),file.path(kit,"kit-checksums.csv"),row.names=FALSE)
all_files <- c(files,"kit-checksums.csv",file.path("contract",contract_files))
dir.create(file.path(dest,"contract"),recursive=TRUE)
for(f in all_files) if(!file.copy(file.path(kit,f),file.path(dest,f))) stop("Kit copy failed.")
dest <- normalizePath(dest,mustWork=TRUE)
old <- setwd(dirname(dest))
status <- utils::zip(paste0(basename(dest),".zip"),file.path(basename(dest),all_files))
setwd(old)
if(!identical(status,0L)) stop("Zip creation failed.")
cat("Code-only re-export kit:",paste0(dest,".zip"),"\n")
