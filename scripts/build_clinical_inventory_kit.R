# Rscript --vanilla scripts/build_clinical_inventory_kit.R [fresh-output-directory]
# Run at the reactextract root. Writes kit-checksums.csv, then copies the
# code-only kit to dist/ and zips it. Checksums ignore carriage returns, as
# the kit's 00_load.R does.
args <- commandArgs(trailingOnly = TRUE)
kit <- "inst/enclave/clinical-inventory-v1"
files <- c("00_load.R", "01_run_inventory.R", "02_prepare_export.R", "03_self_test.R", "sources.R",
  "inventory.R", "export.R", "fixtures.R", "README.md", "REVIEWER.md", "LICENSE.md")
if (!all(file.exists(file.path(kit, files)))) stop("Incomplete kit: ", paste(files[!file.exists(file.path(kit, files))], collapse = ", "))
md5_text <- function(path) {
  bytes <- readBin(path, "raw", file.info(path)$size)
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeBin(bytes[bytes != as.raw(13)], tmp)
  unname(tools::md5sum(tmp))
}
utils::write.csv(data.frame(file = files, md5 = vapply(file.path(kit, files), md5_text, character(1))),
  file.path(kit, "kit-checksums.csv"), row.names = FALSE)
if (identical(args, "--checksums-only")) quit(save = "no")
dest <- if (length(args)) args[1] else "dist/reactextract-clinical-inventory-v1"
if (file.exists(dest) || file.exists(paste0(dest, ".zip"))) stop("Choose a new output directory; existing kits are not overwritten.")
dir.create(dest, recursive = TRUE)
if (!all(file.copy(file.path(kit, c(files, "kit-checksums.csv")), dest))) stop("Kit copy failed.")
dest <- normalizePath(dest, mustWork = TRUE)
old <- setwd(dirname(dest))
status <- utils::zip(paste0(basename(dest), ".zip"), file.path(basename(dest), c(files, "kit-checksums.csv")))
setwd(old)
if (!identical(status, 0L)) stop("Could not create zip.")
cat("Code-only kit:", paste0(dest, ".zip"), "\n")
