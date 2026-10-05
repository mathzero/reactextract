# Run from this directory, with reactextract 0.5.6 installed and your usual con.
# No generator settings or approved profiles are changed by this script.
library(reactextract)
if (!exists("con")) stop("Open your usual enclave database connection as con, then rerun this script.")
if (file.exists("react-participation-diagnostic-v1")) stop("The output folder already exists. Rename it or choose a fresh working folder before rerunning.")
contract_files <- read.csv("contract-checksums.csv", colClasses = "character")
for (i in seq_len(nrow(contract_files))) {
  observed <- getFromNamespace(".sha256_file", "reactextract")(contract_files$file[i])
  if (!identical(observed, contract_files$sha256[i])) stop("Diagnostic kit checksum mismatch. Copy the complete kit again.")
}
source("diagnostic.R")
participation_diagnostic <- run_participation_diagnostic(
  react_oracle(con), contract = ".", rounds = "all", batch_size = 40L
)
# Keep the detailed aggregate object in this R session inside the enclave.
# Inspect participation_diagnostic$issues and $inventory before requesting export.
participation_export <- prepare_participation_diagnostic_export(participation_diagnostic)
write_participation_diagnostic(participation_export, "react-participation-diagnostic-v1")
message("Diagnostic finished. Keep the output inside the enclave until disclosure review is complete.")
