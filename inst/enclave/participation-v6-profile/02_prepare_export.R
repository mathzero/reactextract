# No database connection needed. Uses the saved internal aggregate checkpoint.
source("00_load.R")
conditional_profile <- readRDS("participation-v6-INTERNAL/conditional-profile-INTERNAL.rds")
protected_profile <- pc6_prepare_export(conditional_profile)
pc6_write(protected_profile, "react-participation-conditional-v1-candidate")
message("Candidate written. Obtain normal enclave disclosure review before transferring this folder.")
message("Never transfer participation-v6-INTERNAL or its RDS files.")
