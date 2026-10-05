# All-round dependency modelling kit — v1

This is an **enclave-only research kit**, not a new reactextract release. It does
not alter your database, installed package, approved profiles or default generator.

The kit checks REACT-1 Round 13 and REACT-2 Round 6 first, then continues through
the other rounds. Successful rounds are saved separately. Unexpected input or
fitting problems are reported; they are never silently called a successful model.

## 1. Copy and open

Copy the complete `reactextract-dependency-model-kit-v1` folder into the enclave.
Use your existing offline reactextract installation (0.5.6 or later, with the
rc14 dictionary). No extra fitting package is needed. Set R's working directory
to this folder. Do not edit the kit files: their checksums are checked on load.

## 2. Run once

Create your usual database connection named `con`. Then run in the R console:

```r
source("00_load.R")

source_data <- reactextract::react_oracle(con)
dm_run(
  source_data,
  source_label = "REACT historical views - my unchanged snapshot",
  output = "dependency-model-INTERNAL-v1"
)
```

Choose a non-secret snapshot label you will recognise. Do not include a password
or connection string. Keep the database snapshot unchanged during the run and
on resume. A label does not itself freeze an Oracle database: ask for a stable
historical view/snapshot if these tables can change.

All 25 rounds are requested. R13 and REACT-2 R6 run first as technical pilots.
If either pilot's outcome model cannot be fitted and sampled, processing pauses
with a clear internal report. Otherwise the remaining rounds run automatically.
Later round failures do not discard completed rounds or stop unrelated rounds.

Progress reports show the round, records, batch/field progress, fitting stage and
elapsed time. Processing is sequential. Only selected modelling fields are held
for one round; the participation evidence scan reads safe projected states in
small batches. It does not extract all-round long tables or load the subject
crosswalk.

## 3. Resume if interrupted

Run exactly the same command. Completed rounds are reused. Failed rounds without
a completed checkpoint are retried. The kit refuses to mix different settings,
package versions, code, file inputs or snapshot labels in one run folder.

After an R crash, an empty `.running` folder may remain inside the output folder.
Only remove that empty lock folder after confirming no other run is still using
the output. Do not remove completed round files. If an outcome has too few cases
for a model, the saved status is `insufficient_outcome_support`, not a fitted model;
re-running the identical data will not create evidence that is absent.

Use a fresh output folder for changed data or a revised kit. Each checkpoint is
written through a temporary file so interruptions do not masquerade as a completed
round. No database writes are performed. The kit does not close your connection.

## 4. Read the combined report

Open `dependency-model-INTERNAL-v1/RUN_STATUS.md` for the concise round-by-round
status. Open `INTERNAL_REVIEW.md` **inside the enclave** for detailed results.
`fitted_pending_review` means a technical candidate exists, not that it is a good
enough final model or authorised for release.

The first candidate fits participation-aware PCR/antibody main-effect models and
captures dose, product/timing and exact acute-symptom/Ct summaries. Product/time
effects and interaction terms remain held for the combined mapping/model review.
The synthetic test is at the modelling-state level; it does not yet replace the
researcher-facing raw/cleaned generator or validate every final questionnaire
dependency. All such limitations are recorded per round.

## 5. Prepare one internal review candidate

```r
dm_prepare_review(
  output = "dependency-model-INTERNAL-v1",
  destination = "dependency-model-review-INTERNAL-v1"
)
```

Give that folder to the enclave disclosure reviewer **inside the enclave**.
It contains unsuppressed counts and unapproved model parameters. **Do not copy
the folder out.** The kit does not apply a misleading cell-by-cell suppression
pass to overlapping tables. A reviewed release-selection/suppression step comes
after the reviewer decides what can be released.

For the next conversation, tell us whether the run completed and any fixed status
labels in `RUN_STATUS.md`. Copy only that status file if enclave policy permits.
Do not paste internal model coefficients, exact counts or database error output.

## Optional controls

`batch_size = 20L` reduces the query batch size. `synthetic_n = 10000L` is the
default modelling-state test size per round, not a source-data sample limit.
Changing either setting requires a fresh output folder. `rounds` can narrow a
diagnostic run, but the default and recommended workflow is all rounds.

Existing real-data extraction and synthetic generation are unchanged. The kit's
source, dictionary and rule hashes are recorded; there are no runtime downloads.
# Patch v1.1: recovering a failed final fit

For the simplest upgrade, set `previous_output` to the old output folder and
`output` to a new folder, create your usual `con`, then run
`source("03_resume_original.R")` from this kit. It reads the original settings
automatically. It uses the default Oracle registry; if you originally supplied
custom source configuration, use the manual call below with that same source.

Use this kit in a new folder; keep the original kit and output intact. No package
reinstallation is needed. Create your usual `con`, set the working directory to
this new kit folder, and run the following with the **same source label and
settings used originally**. Replace the two absolute paths with enclave paths.

```r
source("00_load.R")
dm_run(
  reactextract::react_oracle(con),
  source_label = "REACT historical views - unchanged snapshot",
  previous_output = "/path/to/original/dependency-model-INTERNAL-v1",
  output = "/path/to/new/dependency-model-INTERNAL-v1-1"
)
```

This copies successful original-v1 checkpoints without changing them. Failed
fits are not copied, so Round 6 will be queried and fitted again. The other rounds
follow automatically if it passes. The original output remains untouched. Source
views must still represent the same snapshot: the label alone cannot verify this.
The kit refuses migration if the saved package version, settings, dictionary or
source configuration differ. It records the origin of reused models for review.

To resume the **new** run later, use the same command and settings but omit
`previous_output`. To retry a saved `refit_failed` or `fitting_failed` checkpoint,
also supply `retry_failed = TRUE`. Previous failed checkpoints are archived before
retry; successful fits are reused. Insufficient-support holds are not bypassed.

The revised solver starts the final fit from the training coefficients and, when
necessary, allows a further 2,000 iterations at the same selected penalty. It
does not accept a failed fit or weaken the convergence checks. Failure details
are now retained in `INTERNAL_REVIEW.md`, **inside the enclave only**. No new
counts, parameters or errors enter `RUN_STATUS.md`. The cause of the original
Round 6 failure cannot be established because v1 discarded the underlying error.
