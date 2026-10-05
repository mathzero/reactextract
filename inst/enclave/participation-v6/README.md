# Check survey participation and questionnaire eligibility

This is the first enclave check for the proposed 0.6.0 upgrade. It runs with
the existing `reactextract` 0.5.6 package and its rc14 dictionary. No new package
installation or full data export is needed. It does not change fake data yet.

## Run the check

1. Copy this entire folder into the enclave.
2. In RStudio, set the working directory to this folder.
3. Open your usual database connection as `con`.
4. Run:

   ```r
   source("run.R")
   ```

The script checks all 25 rounds, one round at a time in batches of 40 fields.
It reports progress. Leave the connection open until it finishes; the script
does not open or close connections or use the subject crosswalk.

For a smaller first check, source `diagnostic.R` and call:

```r
check <- run_participation_diagnostic(
  reactextract::react_oracle(con), ".", rounds = "REACT1_R06"
)
View(check$issues)
View(check$inventory)
```

## What it checks

- How `-77` agrees between fields and several named candidate markers.
  Marker codes 0–3 and -77 are counted exactly without assigning meanings;
  all other marker values are combined without releasing their contents.
- Whether provisional registration/individual field groups have all `-77`,
  a mixture of `-77` and recorded answers, or no `-77`.
- Separate counts of database missingness, `-77`, `-91`, `-92`, `-66`, `-99`,
  `-555`, other negative codes, and recorded answers.
- Field states by public age boundaries: 5, 12, 13, 16, 18 and 55.
- Whether named mail-group or completion fields are accessible upstream;
  mail-group agreement with `U_AGE` where available.
  This includes `U_MAIL_GRP`/`u_mail_grp`, the upstream spelling explicitly
  mentioned in the Round 2 registration questionnaire, and common variants.
- Agreement between `U_AGE` and `AGE`, without treating them as interchangeable.

The source/stage inventory is provisional. A field containing `-77` is **not**
assumed to prove whole-survey non-response. All available answers remain intact.
Gender, deprivation, geography and laboratory fields are not blanked or changed.
Mixed types and dates are handled as types, not by converting dates to `-77`.
The “all fields -77” check uses only fields capable of storing that code.
Dates are excluded from that count, but a recorded date alongside `-77` still
flags a mixed/partial pattern for review.

Oracle returns a key and small fixed state codes, never raw response text or
exact ages. Keys are checked for uniqueness and each batch is matched by key.
A missing field or failed batch is reported, never joined by row position.
Fields that cannot be inspected remain unavailable, not zero non-response.

## Read the result and send it for disclosure review

`participation_diagnostic` contains **internal aggregate counts** for the enclave
reviewer. It contains no respondent rows, keys, response text, or exact ages.
It remains inside the enclave. Do not use `saveRDS()` to export this object.

The `react-participation-diagnostic-v1` folder contains a protected candidate
export. Small counts, related copies and complementary cells are hidden;
remaining counts are rounded to five. This is deliberately conservative, so
some comparisons may not be releasable. It does not constitute disclosure
approval; the reviewer should also consider differencing against existing
released profiles. Do not copy this folder outside before approval.

If any upstream completion indicators are listed as available, ask the data
manager for their definitions and code meanings. The script does not guess
those meanings or select unknown answer contents. Unlisted administrative fields
may still exist: this is a targeted probe, not an exhaustive database catalogue.

After disclosure approval, copy the protected folder back to the project.
We will use it to settle the participation rules and any round-specific
exceptions, then supply the second enclave script for profile v6. The present
v5 profile and generator remain unchanged until that evidence is reviewed.

## Attribution

The diagnostic code uses reactextract's MIT licence (included as `LICENSE.md`).
The proposed source/stage inventory derives from the
[REACT data dictionary](https://github.com/mathzero/react_wiki) and its public
Imperial College London questionnaire evidence. This metadata retains
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) attribution; it is
not an approved replacement dictionary or routing release.
