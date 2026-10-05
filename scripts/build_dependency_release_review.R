kit<-"inst/enclave/dependency-release-review-v1"
files<-c("01_prepare.R","prepare.R","README.md")
hash<-function(f) digest::digest(file=f,algo="sha256")
write.csv(data.frame(file=files,sha256=vapply(file.path(kit,files),hash,character(1))),file.path(kit,"kit-checksums.csv"),row.names=FALSE)
dest<-"dist/reactextract-dependency-release-review-kit-v1"
if(file.exists(dest)||file.exists(paste0(dest,".zip"))) stop("Choose a new version; existing deliverables are not overwritten.")
dir.create(dest,recursive=TRUE)
files<-c(files,"kit-checksums.csv")
stopifnot(all(file.copy(file.path(kit,files),dest)))
dest<-normalizePath(dest);old<-setwd(dirname(dest))
status<-utils::zip(paste0(basename(dest),".zip"),file.path(basename(dest),files));setwd(old);stopifnot(status==0)
writeLines(paste(hash(paste0(dest,".zip")),basename(paste0(dest,".zip"))),paste0(dest,".zip.sha256"))
message("Code-only kit: ",dest,".zip")
