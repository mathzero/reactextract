# Assemble a separate diagnostic kit without rebuilding or replacing v5.
# Rscript scripts/build_participation_diagnostic_kit.R /path/to/react_wiki
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Supply the react_wiki directory.")
wiki <- normalizePath(args[[1]], mustWork = TRUE)
inventory <- file.path(wiki, "generated/review/participation-v6/occurrence_provenance.csv")
if (!file.exists(inventory)) stop("Build the participation review in react_wiki first.")
source("R/dictionary.R")
kit <- "inst/enclave/participation-v6"
if (!file.copy(inventory, file.path(kit, "occurrence_provenance.csv"), overwrite = TRUE)) stop("Could not copy provenance inventory.")
if (!file.copy("LICENSE.md", file.path(kit, "LICENSE.md"), overwrite = TRUE)) stop("Could not copy code licence.")
files <- file.path(kit, c("README.md", "run.R", "diagnostic.R", "occurrence_provenance.csv", "LICENSE.md"))
utils::write.csv(data.frame(file = basename(files), sha256 = vapply(files, .sha256_file, character(1))),
  file.path(kit, "contract-checksums.csv"), row.names = FALSE)
output <- "dist/reactextract-participation-v6-kit"
dir.create(output, recursive = TRUE, showWarnings = FALSE)
for (file in c(files, file.path(kit, "contract-checksums.csv"))) {
  if (!file.copy(file, output, overwrite = TRUE)) stop("Could not copy diagnostic kit file.")
}
archive <- paste0(output, ".zip")
if (file.exists(archive)) stop("Archive already exists; move it aside before rebuilding.")
previous <- setwd(dirname(output))
on.exit(setwd(previous), add = TRUE)
utils::zip(basename(archive), file.path(basename(output), basename(c(files, "contract-checksums.csv"))))
message("Diagnostic kit written to ", archive)
