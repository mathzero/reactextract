# Run with the working directory set to this unedited kit folder.
if (!requireNamespace("reactextract", quietly = TRUE) || !requireNamespace("digest", quietly = TRUE))
  stop("Install the existing offline reactextract package first (0.5.6 or later).")
if (utils::packageVersion("reactextract") < "0.5.6") stop("This kit requires reactextract 0.5.6 or later.")
dm_root <- normalizePath(".")
dm_manifest <- read.csv("kit-checksums.csv", colClasses = "character", na.strings = character())
if (!identical(names(dm_manifest), c("file", "sha256")) || anyDuplicated(dm_manifest$file) ||
    any(grepl("(^/|[.][.]|\\\\)", dm_manifest$file))) stop("Invalid kit manifest.")
required <- c("00_load.R", "01_run.R", "02_prepare_review.R", "capture.R", "models.R", "run.R",
  "README.md", "REVIEWER.md", "LICENSE.md", "specification.md", "vendor/profile.R",
  "vendor/synthetic.R", "vendor/eligibility_context.R", "vendor/response_options.R", "vendor/synthetic_rules.R",
  "contract/checksums.csv", "rules/checksums.csv")
if (!all(required %in% dm_manifest$file)) stop("Incomplete kit.")
for (i in seq_len(nrow(dm_manifest))) if (!file.exists(dm_manifest$file[i]) ||
    !identical(digest::digest(file = dm_manifest$file[i], algo = "sha256"), dm_manifest$sha256[i]))
  stop("Kit checksum mismatch: ", dm_manifest$file[i])
dm_implementation_hash <- digest::digest(file = "kit-checksums.csv", algo = "sha256")
# Private compatibility helpers have their own environment: no installed code
# or approved profile is modified by loading this kit.
dm_rules <- new.env(parent = asNamespace("reactextract"))
for (f in c("synthetic.R", "eligibility_context.R", "response_options.R", "synthetic_rules.R"))
  sys.source(file.path("vendor", f), envir = dm_rules)
source("vendor/profile.R")
source("models.R")
source("capture.R")
source("run.R")
invisible(dm_load_contract())
message("Kit verified. No database queries have run. All outputs must stay inside the enclave pending review.")
