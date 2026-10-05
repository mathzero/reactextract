# Build only from the exact approved return. No database queries or hidden counts.
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=2L) stop("Supply the re-export kit and a fresh bundle directory.")
kit <- normalizePath(args[1],mustWork=TRUE)
out <- args[2]
if(file.exists(out)) stop("Choose a fresh output directory.")
sha <- function(f) digest::digest(file=f,algo="sha256",serialize=FALSE)
read <- function(f) utils::read.csv(f,colClasses="character",na.strings=character(),check.names=FALSE)
candidate <- file.path(kit,"react-participation-conditional-v2-candidate")
stopifnot(sha(file.path(candidate,"manifest.csv"))=="bf0ed133ad95d6b34aa8203f79d25575d3632480c9d8331aa579e5819dc10e49",
 sha(file.path(candidate,"checksums.csv"))=="5a1b2a5a34a20cd82e4bb249481801f2bfdfc1fcf7efc0d4ecf4e54230a3f5d5")
for(dir in c(candidate,file.path(kit,"contract"))) {
  h <- read(file.path(dir,"checksums.csv"))
  stopifnot(all(!grepl("[/\\\\]|[.][.]",h$file)),!anyDuplicated(h$file))
  stopifnot(identical(unname(vapply(file.path(dir,h$file),sha,character(1))),h$sha256))
}
dir.create(out,recursive=TRUE)
dir.create(file.path(out,"candidate"));dir.create(file.path(out,"contract"))
for(pair in list(c(candidate,"candidate"),c(file.path(kit,"contract"),"contract"))) {
  h <- read(file.path(pair[1],"checksums.csv"))
  files <- c(h$file,"checksums.csv")
  stopifnot(all(file.copy(file.path(pair[1],files),file.path(out,pair[2]))))
}
approval <- data.frame(key=c("status","approval_date","approved_by","candidate_manifest_sha256","candidate_checksums_sha256","scope","generator_status"),
 value=c("formally_approved","2026-09-24","mathzero",sha(file.path(candidate,"manifest.csv")),sha(file.path(candidate,"checksums.csv")),
 "exact_compact_candidate_with_documented_unestimated_fallback","preview_remaining_compound_rules_and_outcome_calibration"))
utils::write.csv(approval,file.path(out,"approval.csv"),row.names=FALSE)
files <- sort(list.files(out,recursive=TRUE))
utils::write.csv(data.frame(file=files,sha256=vapply(file.path(out,files),sha,character(1))),file.path(out,"checksums.csv"),row.names=FALSE)
old <- setwd(out);on.exit(setwd(old),add=TRUE)
utils::tar(paste0(normalizePath(out),".tar.gz"),files=c(files,"checksums.csv"),compression="gzip",tar="internal")
cat("Archive SHA-256:",sha(paste0(normalizePath(out),".tar.gz")),"\n")
