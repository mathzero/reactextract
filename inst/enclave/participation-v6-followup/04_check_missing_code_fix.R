# Supplement for a completed v2 follow-up: seven rounds only, no all-field rerun.
# Run from this new kit folder with the usual caller-managed connection as con.
source("00_load.R")
export_path <- "react-participation-targeted-v3-supplement"
internal_path <- "INTERNAL_ONLY/targeted-missing-code-fix-v3.rds"
if (file.exists(export_path)) stop("The supplement already exists. Keep it; do not repeat the database run.")
if (file.exists(internal_path)) stop("Saved supplement checks already exist. Run source('03_export_saved_checks.R') to finish exporting without querying again.")
if (!exists("con", inherits = TRUE)) stop("Open your usual enclave database connection as con, then run this script again.")
participation_targeted_v3 <- pf6_targeted(
  reactextract::react_oracle(con),
  rounds = c("react1.r01", sprintf("react2.r%02d", 1:6)),
  # None of the seven original spellings was accessible in v2. Avoid repeating
  # those empty probes; a data-owner-confirmed field can be checked separately.
  mail_fields = character(), progress = TRUE
)
participation_targeted_v3$manifest <- rbind(participation_targeted_v3$manifest,
  data.frame(key = c("run_scope", "mail_probe_policy"),
    value = c("seven_round_supplement_to_targeted_v2_not_a_25_round_profile",
      "no_repeat_probes_after_v2_all_named_probes_unavailable")))
dir.create("INTERNAL_ONLY", showWarnings = FALSE)
saveRDS(participation_targeted_v3, internal_path)
writeLines("ENCLAVE ONLY. Unsuppressed aggregate results, not an approved export. Never copy this folder outside the enclave.", "INTERNAL_ONLY/DO_NOT_EXPORT.txt")
print(participation_targeted_v3$issues, row.names = FALSE)
pf6_write(pf6_prepare_export(participation_targeted_v3), export_path)
message("Supplement complete. Keep INTERNAL_ONLY inside the enclave. Request normal disclosure review of the supplement against both v2 exports and earlier profiles before returning it.")
