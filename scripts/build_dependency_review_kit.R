# Run from the package root. Ships code/instructions only, never report outputs.
kit <- "inst/enclave/dependency-review-v1"
files <- c("review.R","01_make_report.R","README.md")
checks <- data.frame(file=files,sha256=vapply(file.path(kit,files),function(f)
  digest::digest(file=f,algo="sha256"),character(1)))
write.csv(checks,file.path(kit,"checksums.csv"),row.names=FALSE)
dest <- "dist/reactextract-dependency-review-kit-v1"
if (file.exists(dest) || file.exists(paste0(dest,".zip"))) stop("Delivery already exists; use a new version rather than overwrite.")
dir.create(dest,recursive=TRUE)
stopifnot(all(file.copy(file.path(kit,c(files,"checksums.csv")),dest)))
dest <- normalizePath(dest);old<-setwd(dirname(dest))
status <- utils::zip(paste0(basename(dest),".zip"),file.path(basename(dest),c(files,"checksums.csv")))
setwd(old);stopifnot(status==0)
archive <- paste0(dest,".zip")
writeLines(paste(digest::digest(file=archive,algo="sha256"),basename(archive)),paste0(archive,".sha256"))
message(archive)
