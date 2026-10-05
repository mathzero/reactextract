if(!exists("review_folder") || !exists("selection_folder")) stop("Set review_folder and selection_folder first.")
if(!requireNamespace("digest",quietly=TRUE)) stop("digest is required.")
checks <- read.csv("kit-checksums.csv",colClasses="character")
if(!identical(names(checks),c("file","sha256")) || anyDuplicated(checks$file) ||
  !all(c("01_prepare.R","prepare.R","README.md") %in% checks$file) ||
  any(grepl("[/\\\\]|[.][.]",checks$file))) stop("Invalid kit manifest.")
for(i in seq_len(nrow(checks))) if(!file.exists(checks$file[i]) ||
  !identical(digest::digest(file=checks$file[i],algo="sha256"),checks$sha256[i])) stop("Kit checksum mismatch.")
source("prepare.R")
react_prepare_dependency_release_review(review_folder,selection_folder)
