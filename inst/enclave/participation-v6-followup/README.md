# Participation follow-up: corrected missing-code check

## If you have already returned both v2 folders

Run **only this supplement**, not Steps 1 and 2 again:

1. Copy and unzip the new `reactextract-participation-v6-followup-kit-v3.zip`
   inside the enclave, into a **new folder**. Keep the old kit and all its results.
2. Set RStudio's working directory to the new folder, and open your usual
   database connection as `con`. No package reinstallation is needed.
3. Run:

   ```r
   source("04_check_missing_code_fix.R")
   ```

This repeats the short checks for REACT-1 Round 1 and the six REACT-2 rounds
only. It does not repeat the all-field diagnostic or the re-export. None of the
previous mail-group name probes succeeded, so these are not repeated either.

Why this is needed: v2 recognised `-77` in the exact-code checks only when it
appeared in that field's dictionary response list. Seventeen field/round lists
omitted it. Such values were put in the combined "other code" category, making
some `-77` comparisons unreliable. The corrected script explicitly checks this
known missing code even when that list is incomplete. Other unknown values are
still grouped; their contents are never exported. The all-field re-export used
a separate literal `-77` check and is not affected by this bug.

After normal enclave disclosure review, return only:
**`react-participation-targeted-v3-supplement`**.

Review this against both v2 exports and earlier profiles before release; extra
suppression may be needed. Keep `INTERNAL_ONLY` inside the enclave. If export
preparation stops after collection, `source("03_export_saved_checks.R")`
resumes it without more database queries. The supplement is not a complete
25-round profile and does not replace the final profile-v6 run.

We still need the data manager to confirm:

- what date and source `AGE` and `U_AGE` refer to, and which drove questionnaire
  age restrictions;
- where the original mail group is held, or whether a documented age equivalent
  is valid;
- which flags define registration and individual-questionnaire completion,
  especially for REACT-2, and how partial responses were coded in REACT-1 Round 6.

No raw records or unsuppressed counts need to be sent back for these definitions.

## Original workflow (only if not already completed)

This kit follows the first participation diagnostic. It does not change the
package, fake data, or an approved profile. No package reinstallation is needed:
reactextract 0.5.4–0.5.6 with the same rc14 dictionary is supported.

## Before you start

1. Copy and unzip this **whole kit** inside the enclave, into a new folder.
2. In RStudio, set the working directory to the unzipped folder (the one
   containing `00_load.R`). Keep the original R session open if possible.
3. Do not edit kit files: their checksums are checked before running.

You need the original `participation_diagnostic` object for Step 1 only. Step 2
can run without it. Do not repeat the previous all-field run.

## Step 1 — reuse what has already been collected

Check in the original R session:

```r
exists("participation_diagnostic")
```

If `TRUE`, run:

```r
source("01_reuse_saved_results.R")
```

If the object was saved as an RDS **inside the enclave**, restore it first:

```r
participation_diagnostic <- readRDS("/your/enclave-only/path/diagnostic.rds")
source("01_reuse_saved_results.R")
```

Use the actual path to your saved file. The script rejects protected exports;
the returned CSV folder cannot replace the original object. If the object is
lost, skip Step 1 and tell us when returning Step 2 results.

This step does **not** connect to the database. It compares each field's `-77`
status with one named reference field per round, chosen from the comparisons
already collected. That reference is not assumed to prove survey completion.
The smaller export contains counts of disagreement in each direction. Matching
counts and round totals are withheld to avoid unnecessary linked totals.

Output: **`react-participation-reexport-v2`** (requires disclosure review).

`below_10` means any count from zero to nine. It does not mean zero and does not
prove exact agreement. Exact counts can be inspected inside the enclave only.

## Step 2 — check the missing pieces

Open your usual database connection as `con`, then run:

```r
source("02_run_targeted_checks.R")
```

This queries at most 20 named dictionary fields per round, plus any of seven
mail-group spellings found by empty probes. Not every field exists in every
round. It is a much smaller query than the all-field diagnostic, although the
actual time still depends on the database. It prints the round, records,
batches and elapsed time. It does not use the subject crosswalk.

It checks:

- `REGREPORTFIG` and `SFREPORTFIG` against a few questionnaire and test fields;
- registration/symptom-file membership and stage-date presence;
- recorded versus missing laboratory values, without lab result text;
- smoking's documented answer codes against **each** available age field;
- whole versus fractional ages, using the public age boundaries without rounding;
- named mail-group fields, if accessible, without assuming their meanings.

Only a key and fixed state codes are retrieved. Batches must have exactly the
same unique keys and are matched by key, never row position. Unknown response
codes are combined into `other_code_not_released`; their contents do not leave
the database. Dates are present/missing only. Exact ages are never returned.

Output: **`react-participation-targeted-v3`** (requires disclosure review).

For questionnaire/reporting-flag pairs, this export also prioritises compact
`-77` disagreement counts. The full answer-code comparisons stay in the internal
object for the reviewer, rather than duplicating those counts in the export.

The script also saves unsuppressed aggregate results under **`INTERNAL_ONLY`**
before preparing the export. Keep that folder inside the enclave. It is not an
export, even though it contains aggregates rather than participant rows.

## If something stops

- A missing original object: skip Step 1; Step 2 still runs independently.
- A connection error: reopen `con` using your usual procedure.
- A failed field: read the `issues` table; other safely matched fields are retained.
- An interrupted export after collection: run
  `source("03_export_saved_checks.R")`. It reuses saved aggregates and does
  **not** query the database. Existing completed exports are kept unchanged.
- A partially created output folder: keep it, rename it inside the enclave,
  then run the recovery script. Do not delete or replace the saved internal RDS.
- A dictionary/checksum error: stop and send the error message, without raw data.

For a one-round trial, after `source("00_load.R")`:

```r
trial <- pf6_targeted(
  reactextract::react_oracle(con),
  rounds = "REACT1_R06"
)
View(trial$issues)
```

Do not copy `trial`, the R workspace, or `INTERNAL_ONLY` outside the enclave.

## Step 3 — review, then return only the approved folders

Follow `REVIEWER.md`. Both export folders are **candidates**, not automatically
approved. Small counts are hidden, related complementary cells are hidden, and
visible counts are rounded to five. The protection step no longer hides
unrelated cells simply because their numeric counts happen to match.

The reviewer must consider the earlier diagnostic and released profiles too;
automated controls cannot certify protection across multiple releases.

After approval, copy back only:

1. `react-participation-reexport-v2` (if Step 1 was possible);
2. `react-participation-targeted-v3`.

Put these under `reactextract-0.5.6-offline` and tell us which ones are present.
Please also ask the data manager for the definitions of `AGE`, `U_AGE`, and the
mail-group field/location. We will review the evidence before setting rules or
requesting the final v6 profile. Nothing here changes the released v5 generator.

## Attribution

Code: MIT, see `LICENSE.md`. The code uses the installed, checksum-pinned REACT
dictionary and its public response options. That metadata retains its CC BY
4.0 attribution to the REACT data dictionary and underlying questionnaire
sources. This kit does not grant permission to export enclave results.
