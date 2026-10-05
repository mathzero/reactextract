# Internal model review report — v1

This code-only kit creates one self-contained HTML report from the results you
have already collected. No Oracle connection, package reinstall, internet access,
or new extraction is required. It uses base R plus the already-used digest
package to verify the kit files.

## Run inside the enclave

1. Copy and unzip this kit inside the enclave. Set R's working directory to it.
2. Set the two paths below to your actual enclave locations. The first is the
   **prepared review bundle**, not the original modelling output folder.
3. Run:

```r
review_folder <- "/path/to/dependency-model-review-INTERNAL-v1-1"
report_file <- "/path/to/dependency-review-INTERNAL-v1.html"
source("01_make_report.R")
```

Open the resulting HTML file in an enclave browser. It has no external assets.
The script will not overwrite an existing report or modify the input bundle.
Use a new filename if you need to regenerate it.

## Read the report

Start with the all-round summary. Then inspect flagged rounds and their observed
(blue) versus predicted (orange) positivity plots. Counts and predicted rates are
shown by age, vaccination status, dose and participation where recorded.

Flags identify potential mismatches, small groups, large coefficients, missing
diagnostics and models that do not improve on a constant-rate baseline. They are
transparent review prompts, not automatic rejection or approval. Absent solver
diagnostics are expected for a successful fit reused from the original v1 kit.

The plots use held-out validation records; the same set was used to choose the
penalty. They do not provide independent final-test performance. They also do not
validate the full synthetic raw/cleaned output. The HTML explains which additional
checks cannot be made from the saved review bundle. In particular, source-field
failures remain in the original run report, and synthetic-marginal comparisons
were not copied into this bundle.

## What happens next

Review scientific adequacy using this report and REVIEWER.md from the original
bundle. If a model looks poor, retain its results and note the round and kind of
issue; do not repeatedly refit until a flag disappears. Assess the intended
parameter/count release separately with your normal disclosure reviewer.

**The HTML contains unsuppressed information and must remain inside the enclave.**
Do not email it, upload it to this chat, or save it in OneDrive. No numerical
results or fitted parameters are approved for export by this script. Even review
flags can reveal information about small groups; discuss any external summary
under local disclosure policy. Report only that generation succeeded initially.

Only this code-only kit is copied to the project's OneDrive delivery folder.
