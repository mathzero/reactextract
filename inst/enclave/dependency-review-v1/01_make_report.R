# Set review_folder and report_file first. No database connection is required.
if (!exists("review_folder") || !exists("report_file"))
  stop("Set review_folder to your INTERNAL review bundle and report_file to a new HTML filename.")
if (!requireNamespace("digest",quietly=TRUE)) stop("digest is required to verify the kit (already used by reactextract).")
checks <- read.csv("checksums.csv",colClasses="character")
if (!identical(names(checks),c("file","sha256")) || anyDuplicated(checks$file) ||
    !all(c("review.R","01_make_report.R","README.md") %in% checks$file) ||
    any(grepl("[/\\\\]|[.][.]",checks$file))) stop("Invalid kit checksum manifest.")
for (i in seq_len(nrow(checks))) if (!file.exists(checks$file[i]) ||
  !identical(digest::digest(file=checks$file[i],algo="sha256"),checks$sha256[i]))
  stop("Kit checksum mismatch: ",checks$file[i])
source("review.R")
react_dependency_review(review_folder,report_file)
