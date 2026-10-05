# Next participation profile: enclave run kit

This read-only job collects the information needed to align survey non-response
and age restrictions in the next synthetic-data release. It does not change your
installed package, real data, or the current synthetic generator.

## Before running

Use the existing offline installation of **reactextract 0.5.4 or later 0.5.x**
with the rc14 dictionary. No new R packages or internet access are needed beyond
that bundle. The kit checks the dictionary rather than relying on a version name.

Copy the complete kit into the enclave. In RStudio set the working directory to
this folder and open your usual read-only Oracle connection as `con`.

```r
source("00_load.R")             # checks files; no database queries
source("01_run_profile.R")      # all 25 rounds, two passes per round
source("02_prepare_export.R")   # prepares the protected candidate
```

Expect a substantial run: it reads each round twice, in batches of 30 fields.
Progress shows the round, record count, pass and batch. Exact runtime depends on
the enclave. Do not run an all-variable extraction first.

## If the connection is interrupted

Reconnect as `con`, then run `01_run_profile.R` again **against the same database
snapshot**. Finished rounds are reused. The unfinished round restarts safely.
Do not delete the saved files or transfer them out of the enclave.

If the database has changed, use a new checkpoint directory and snapshot label
in a separate call to `pc6_run()`. Do not mix completed rounds from different
snapshots. A changed key, field availability or projected source values between
the two passes stops the affected round without saving it.

## What to return

Request normal disclosure review for **react-participation-conditional-v1-candidate**.
The reviewer should read `REVIEWER.md` and consider previous profile/diagnostic
releases alongside it. Copy that folder back only when the enclave permits it.

**Never transfer `participation-v6-INTERNAL`, its RDS files, or an R workspace.**
They contain unsuppressed aggregates (not respondent records).

## What this does and does not establish

- Uses NHS-derived `U_AGE` and the public boundaries 5, 12, 13, 16, 18 and 55.
- Keeps questionnaire `AGE` separate, including the Rounds 4–7 ask-all exception.
- Measures individual-questionnaire participation using the documented working
  decisions. A substantive answer vetoes shared non-response. Missing evidence
  prevents a blanket non-response inference.
- Keeps registration separate: agreement to take part is not completion.
- Retains unexplained `-77` values in each age/participation group.
- Preserves independent fields rather than treating them as questionnaire answers.
- Returns only public categories, fixed bins, missing-code counts and text-presence
  states. Unknown raw values and text content are never returned by Oracle.

This is a **conditional capture candidate, not the final profile v6**. It captures
age and participation groups without pretending that every compound question
route has been resolved. The shared contract lists pending instructions. Remaining
parent/carer and option-level routing, and participation-conditioned outcome
relationships, must be completed before releasing 0.6.0. This run does not replace
that work or the existing approved v5 outcome profiles. Further narrowly targeted
profiles may be needed for conditions not recoverable from these age/stage groups.

No new scientific interpretation of the original missing codes is implied.
