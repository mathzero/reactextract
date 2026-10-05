# Enclave-only review checklist

The follow-up produces aggregate candidate exports, not disclosure approval.
Review them against the first diagnostic **and all previously released REACT
profiles**. In particular, known round totals or earlier marginals must not
allow small suppressed counts to be recovered by subtraction. Additional
suppression, coarsening or withholding of whole tables may be needed.

The v3 supplement must also be reviewed against BOTH returned v2 folders.
It repeats seven rounds to correct a known-code classification bug. Do not
combine or difference v2/v3 cells to recover hidden values. v2 exact-code
comparisons involving the 17 fields whose response lists omitted `-77` are
not evidence of absence of that missing code. The supplement identifies literal
`-77` without approving a participation interpretation for the field. Its
internal object is `participation_targeted_v3`; it must stay in the enclave.

## Keep inside the enclave

- `participation_diagnostic`: original unsuppressed aggregates.
- `participation_reused` and `participation_targeted`: follow-up aggregates.
- Everything under `INTERNAL_ONLY`, RDS files, and the R workspace.

Only the two named candidate export folders are intended for review for release.
No participant identifiers, row-level patterns, text contents, exact ages or
dates should be present in them. The source-query key is used transiently for
alignment, not returned in any aggregate object.

## Interpreting the checks

- `REGREPORTFIG = 2` is documented as agreement to swab/survey, **not** necessarily
  completion of the later questionnaire. Other codes include break-off and
  screening outcomes; exact support comes from each round's dictionary.
- `SFREPORTFIG = 1` is documented as survey completed; `0` as break-off; `-77` as
  non-response. Check the round-specific `-555` and other missing codes too.
- File membership and date presence are supporting evidence, not automatically
  authoritative completion status. A last-access date can exist for a partial
  respondent. Missing dates have no invented `-77` encoding.
- `substantive` in the general state projection means non-missing/non-negative;
  for laboratory text this includes values that need not be clinically evaluable.
  Do not treat this as PCR positivity or proof of a valid result.
- Exact categorical projections expose only public integer codes. Unknown codes
  are combined without revealing their values. `SMOKENOW` codes 1, 2 and 3 mean
  yes, no and prefer not to say, respectively; negative codes remain distinct.
- In v3, the known missing code `-77` is checked even if the occurrence's list
  of public answer options omits it. This is a diagnostic probe, not a change to
  the published dictionary, generation support or approved field semantics.
- `not77` in the reused comparisons includes other missing codes. A matching
  `-77` status does not mean two answers match or both are present.
- Age bands are [0,5), [5,12), [12,13), [13,16), [16,18), [18,55), [55,121).
  Fractional years are allowed and not rounded across a boundary. The separate
  age-quality marginals are enclave-only. Confirm the meanings/units of `AGE`
  and `U_AGE`; neither is automatically a substitute for the original mail group.
- Source/stage classifications remain provisional. No new completion indicator,
  field exception or mail-group mapping is approved by running this script.

## Useful internal views (never copy these tables out unprotected)

```r
View(participation_targeted$issues)
View(participation_targeted$inventory)

# Exact reporting outcomes and age quality, for local interpretation only
local_checks <- subset(
  participation_targeted$counts,
  table == "source_marginal" &
    (field %in% c("REGREPORTFIG", "SFREPORTFIG") | field_kind == "age_quality")
)
View(local_checks)
```

Check Round 6 especially, partial respondents, registration/individual stage
differences, and REACT-2 test-result presence where questionnaire answers are
missing. Please confirm the authoritative completion fields and mail-group/age
definitions through the data owner. Do not infer completion from a single
ordinary questionnaire item.

## Export controls

- Below-ten counts, including zero, are masked; suppressed is not zero.
- Reused comparisons release only disagreement directions: the matching
  remainder is always withheld, as are round totals.
- Targeted exports omit overlapping one-way marginals and age-quality tables.
- Where a compact `-77` disagreement partition is included, its full
  answer-code-by-reporting-flag table is also kept enclave-only, not duplicated
  in the export. Other stage/date, laboratory-presence and age comparisons remain.
- Protection follows actual table, row/column and duplicated-cell relationships,
  never a round-wide match on a coincidentally equal numeric count.
- Positive complementary cells are masked where needed; released counts are
  rounded to the nearest five. Some highly consistent matrices can legitimately
  be largely suppressed. Use the internal evidence to review proposed rules,
  rather than reconstructing hidden counts from candidate exports.
- No cross-release safety claim is made. If the reviewer changes an export,
  regenerate its checksums using the enclave's approved process and record the
  change with the approval. The supplied folder manifest starts as pending.

Record explicit release approval and permitted use for the exact folders before
they leave the enclave. This approval is separate from later approval of the
new generator rules and final profile v6.
