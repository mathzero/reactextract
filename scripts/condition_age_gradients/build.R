# Build inst/extdata/condition-age-gradients-v1.rds from the public prevalence
# figures transcribed in figures.csv, the sources described in sources.csv and
# the gradient used for each condition in conditions.csv.
#
# Run from the package root:
#   Rscript --no-init-file scripts/condition_age_gradients/build.R
# then pin the printed SHA-256 as .condition_gradients_sha256 in
# R/condition_gradients.R. The script also writes the provenance record
# inst/extdata/condition-age-gradients-v1-provenance.csv.

dir <- file.path("scripts", "condition_age_gradients")
read <- function(file) {
  utils::read.csv(file.path(dir, file), colClasses = "character", na.strings = "",
                  check.names = FALSE, stringsAsFactors = FALSE)
}
figures <- read("figures.csv")
sources <- read("sources.csv")
conditions <- read("conditions.csv")

figures$age_lower <- as.integer(figures$age_lower)
figures$age_upper <- as.integer(figures$age_upper)
figures$prevalence_percent <- as.numeric(figures$prevalence_percent)
figures <- figures[order(figures$condition, figures$sex, figures$age_lower), , drop = FALSE]
rownames(figures) <- NULL

gradients <- list(
  release = "condition-age-gradients-v1",
  conditions = conditions,
  figures = figures,
  sources = sources
)
source(file.path("R", "condition_gradients.R"))
.validate_condition_gradients(gradients)

out <- file.path("inst", "extdata", "condition-age-gradients-v1.rds")
saveRDS(gradients, out, version = 3)
sha256 <- if (requireNamespace("digest", quietly = TRUE)) {
  digest::digest(out, algo = "sha256", serialize = FALSE, file = TRUE)
} else {
  strsplit(system2("shasum", c("-a", "256", out), stdout = TRUE), " ")[[1L]][[1L]]
}

provenance <- data.frame(
  release = gradients$release,
  sha256 = sha256,
  data_origin = "public_statistics_only",
  disclosure_review = "not_required_no_react_data",
  sources = paste(sources$source_id, collapse = "|"),
  built_by = "scripts/condition_age_gradients/build.R",
  stringsAsFactors = FALSE
)
utils::write.csv(
  provenance, file.path("inst", "extdata", "condition-age-gradients-v1-provenance.csv"),
  row.names = FALSE
)
cat("SHA-256:", sha256, "\n")
