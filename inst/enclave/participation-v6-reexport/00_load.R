# Run from this new re-export kit, not the original profile kit.
if(!requireNamespace("reactextract",quietly=TRUE)||!requireNamespace("digest",quietly=TRUE))
  stop("Use the existing offline reactextract 0.5.4 or later 0.5.x installation.")
required <- c("00_load.R","01_reexport.R","reexport.R","profile.R","followup-policy.R",
  "README.md","REVIEWER.md","LICENSE.md","contract/checksums.csv")
h <- utils::read.csv("kit-checksums.csv",colClasses="character",na.strings=character())
if(!identical(names(h),c("file","sha256"))||anyDuplicated(h$file)||!setequal(h$file,required)) stop("Incomplete kit. Copy the complete unedited folder.")
for(i in seq_len(nrow(h))) if(!file.exists(h$file[i])||
  !identical(getFromNamespace(".sha256_file","reactextract")(h$file[i]),h$sha256[i])) stop("Kit checksum mismatch: ",h$file[i])
source("followup-policy.R")
source("profile.R")
source("reexport.R")
invisible(pc6_contract("contract"))
message("Re-export kit verified. No database connection or package update is needed.")
