# Run from the kit folder. Checks the kit files and loads the functions; reads
# no data. Checksums ignore carriage returns, so a CRLF conversion in transfer
# still verifies; any other change to a kit file stops here.
if (getRversion() < "4.1.0") stop("This kit needs R 4.1.0 or later.")
ci_kit_files <- c("00_load.R", "01_run_inventory.R", "02_prepare_export.R", "03_self_test.R", "sources.R",
  "inventory.R", "export.R", "fixtures.R", "README.md", "REVIEWER.md", "LICENSE.md")
ci_md5_text <- function(path) {
  bytes <- readBin(path, "raw", file.info(path)$size)
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeBin(bytes[bytes != as.raw(13)], tmp)
  unname(tools::md5sum(tmp))
}
local({
  if (!file.exists("kit-checksums.csv")) stop("Run from the kit folder (kit-checksums.csv not found).")
  hashes <- utils::read.csv("kit-checksums.csv", colClasses = "character")
  if (!identical(names(hashes), c("file", "md5")) || !setequal(hashes$file, ci_kit_files)) {
    stop("Incomplete kit checksums. Copy the complete, unedited kit.")
  }
  found <- vapply(hashes$file, function(f) if (file.exists(f)) ci_md5_text(f) else NA_character_, character(1))
  bad <- hashes$file[is.na(found) | found != hashes$md5]
  if (length(bad)) stop("Kit file changed or missing: ", paste(bad, collapse = ", "))
})
source("sources.R")
source("inventory.R")
source("export.R")
message("Kit verified (", ci_version, "). No data have been read.")
