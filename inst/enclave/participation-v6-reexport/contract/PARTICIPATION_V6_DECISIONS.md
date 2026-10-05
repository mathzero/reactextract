## Scope and status

Working decisions for the **planned reactextract 0.6.0 / profile v6 upgrade**,
recorded on 22 September 2026. They are not yet implemented in the released v5
generator. Real-data extraction continues to preserve the source values without
applying these synthetic-data assumptions.

**Confirmed** means a source definition supplied by the study researcher or
explicit questionnaire wording. **Working assumption** is a deliberate modelling
choice. **Unresolved** means the historical explanation is unknown; the stated
fallback allows development to continue without inventing that explanation.
Permission to make working judgements is not disclosure approval for a profile.

These notes are maintained in the REACT wiki and copied unchanged into
reactextract. They concern realistic data for code development, not scientific
inference or reconstruction of individual participants.

## D01 — Keep the two age variables distinct

**Confirmed source clarification:** `U_AGE` was calculated from NHS-recorded
birthday information. `AGE` is questionnaire-reported age at the last birthday.
It was generally asked when a respondent declined to give their date of birth,
but was asked of everyone in REACT-1 Rounds 4–7. The supplied questionnaire
screenshot shows `ASK IF (DOB = 1)` before `AGE`, with "Prefer not to say" as
the DOB response labelled 1.

Use `U_AGE` as the primary synthetic age context. Do not fill missing `AGE`
from `U_AGE`, replace one raw column with the other, or automatically treat
disagreement as an error. `U_AGE` is independently available; `AGE` is governed
by questionnaire participation and round-specific administration. The exact
calculation date of NHS-derived age remains unknown.

Retain the `AGE` ask-all exception for Rounds 4–7. Elsewhere, use v6
stage-conditioned availability and age-band profiles, describing DOB refusal as
the usual questionnaire route, not an exhaustive explanation of every recorded
value. Generated `AGE` should be compatible with the respondent's age context,
not independently sampled across all ages. Do not force the two ages to match.

The questionnaire's refusal code is not necessarily an extractable value in a
database date column. Do not implement `DOB == 1` against a date, treat 1 January
as refusal, or equate every missing DOB with refusal. Until a usable refusal
indicator is mapped, exact DOB-to-AGE routing remains a stated limitation.
Do not profile or export real birthdays.

## D02 — Approximate mail-group eligibility from NHS-derived age

**Working assumption:** the original mail group was not accessible under the
tested names. Derive an internal REACT-1 eligibility group from `U_AGE` using
the public questionnaire boundaries:

| Internal eligibility group | Age in completed years |
| --- | --- |
| Adult | 18 or older |
| Teenager | 13–17 |
| Child | 5–12 |

This is an age-based proxy, **not a verified recovery of the original mail
group**. Sampling dates, birthdays, corrections and proxy responses may explain
differences in the real data. The screenshot's input-validation ranges allow
boundary discrepancies; they are not alternative mail-group definitions.

Use the intended boundaries rather than reproduce unexplained cross-boundary
answers. No substantive adult-only smoking/vaping answer should be generated
below age 18. Retain thresholds such as 12, 16 and 55 for questions/options that
require them; three mail groups do not replace all age rules. Generate context
internally even for a request containing only downstream fields.

Where no valid public age support is available, do not silently substitute
`AGE`, guess an adult group, or open an age-restricted route. Record the fallback
and generate no substantive restricted answer. Missing codes and refusals are
never numeric ages.

## D03 — Separate questionnaire stages

**Working decision supported by the diagnostics:** represent participation once
per observation **per questionnaire stage**, not once per topic or entire record.
Questionnaire location and source coding both matter. A question appearing in a
PDF does not prove that its database field was refreshed at that stage.

Only explicitly assigned questionnaire occurrences are governed by a shared
state. Linked demographic/geographical information and independently available
laboratory fields are not erased by questionnaire non-response. Uncertain
provenance remains outside a blanket overwrite until explicitly assigned.

Registration agreement is not evidence of later questionnaire completion.
Partial respondents retain their answers; remaining fields are not all
overwritten with `-77`.

## D04 — Use REACT-1 reporting flags cautiously

**Working assumption based on the codebook:** use the round-specific
`SFREPORTFIG` definition as the principal individual-questionnaire state:
`1` recorded completion, `0` recorded break-off, and `-77` recorded non-response.
Keep `-555`, database missingness and other documented states distinct.

Use `REGREPORTFIG` for its documented registration outcome only. `2` means
agreement to swab/survey, not completion of the individual questionnaire.
Screening/refusal codes retain their round-specific meanings. Do not treat
every non-2 value as a nonrespondent.

These are codebook-based modelling decisions, not newly verified administrative
definitions. If a non-response flag conflicts with a substantive answer in the
same governed stage, retain a discordant/partial-or-uncertain state. Do not let
one flag erase recorded answers or infer that the questionnaire was completed.
Measure these exceptions through disclosure-controlled aggregates.

## D05 — Infer a conservative REACT-2 response state

**Working assumption:** no universal authoritative completion indicator has been
established. Use "response evidenced", "shared non-response inferred" and
"undetermined", rather than claim to know which questionnaires were completed.

For the individual questionnaire, use `INDCONFSF`, `ABATTEMPT` and `ABCOMP` as
the initial anchor set, with exact available occurrences checked per round.
Shared non-response requires at least two distinct question anchors, all
available chosen anchors literally `-77`, and no substantive answer elsewhere
in the assigned questionnaire fields. A documented answer in the stage vetoes
a blanket non-response inference. Negative codes, database missingness and
unrecognised values are not substantive answers. Multiple checkbox columns from
one question do not count as independent anchors.

Incomplete or contradictory evidence means "undetermined", retaining observed
field-level missingness. One `-77` is never enough. REACT-2 registration has no
automatic completion assignment: apply this standard only with an explicit
round-specific registration anchor set. Otherwise keep registration undetermined
and do not introduce a shared overwrite.

`ABCOMP` describes test completion, not automatically survey completion.
`NEWRESULT`/`NEWRESULT_2`, last-access dates and file membership are not promoted
to completion indicators. Test results and attempt/completion branches retain
their own source and routing rules.

## D06 — Preserve historical missing-code exceptions

**Unresolved:** REACT-1 Round 1 smoking, many Round 6 fields and the Round 9
smoking-stage transition show exceptions. Their original recoding rationale is
unavailable. Do not automatically label them as errors, skips or nonrespondents.

Separate two components in v6:

- shared stage non-response, where supported; and
- residual `-77` among other participation/eligibility states, profiled as an
  unexplained source missing code, not another whole-survey event.

Keep the literal raw `-77`; do not relabel it `-91` to make the simulation look
cleaner. Detailed provenance should distinguish `survey_nonresponse` from
`unresolved_source_code_77`. Cleaned values retain the existing missing-value
convention in both cases. A literal code cannot resolve an overloaded meaning.

Give Round 9 smoking an occurrence-level exception rather than automatically
moving its participation scope with the questionnaire layout. Whether the data
carried earlier answers forward is unknown; the generator must not claim it did.

## D07 — Apply rules in a fixed order

For each governed questionnaire field:

1. Supported shared non-response applies that occurrence's compatible encoding.
   Numeric/text fields may retain `-77`; dates remain missing dates with the
   reason carried separately.
2. Otherwise, ineligibility applies the documented structural-skip encoding.
   If none is defensible, use database missingness with an explicit skip reason
   instead of inventing a code.
3. Otherwise, generate an eligible response or item-level missingness from the
   new conditional profile, including explicitly modelled residual `-77`.

Negative missing codes fail ordinary answer comparisons, including "not equal
to" and "not in". Participation and demographic eligibility are mandatory AND
conditions around alternative questionnaire paths. Option restrictions apply
to priors and fallbacks too. No subsequent outcome/dependency writer can
repopulate an ineligible field.

## D08 — Profile and disclose the assumptions

The final enclave run records how often the rules assign each stage state,
where they conflict, and how often a proxy or fallback is used. Profile answers
within eligible/responding groups, shared non-response separately, and residual
source codes in their relevant states. Do not carry v5's independently sampled
`-77` frequencies unchanged into shared participation.

Use transient validated keys, fixed public age boundaries and small batches.
No respondent-level missingness pattern, birthday, identifier or text content
leaves the enclave. Retain suppression, complementary suppression and rounding.
Never recover hidden counts or weaken disclosure controls to resolve uncertainty.

An unreleasable distribution remains unavailable. Explicit public-domain
fallbacks must be restricted to questionnaire-valid states, not based on hidden
counts. Record profile, dictionary and decision-document versions in result
manifests. Keep profile v5 reproducible; do not silently reinterpret it.

## Evidence and limitations

- Researcher clarification and supplied questionnaire screenshot, 22 September
  2026: NHS-derived `U_AGE`, questionnaire `AGE`, usual DOB-refusal route, and
  REACT-1 Rounds 4–7 ask-all exception.
- Public questionnaires/codebooks: intended mail-group boundaries and
  round-specific reporting-flag meanings.
- Participation diagnostics and corrected follow-ups reviewed locally: stage
  differences and exceptions, not proof of their historical causes. No returned
  diagnostic counts are reproduced in this public-facing document.
- Researcher direction, 22 September 2026: proceed with documented judgements
  because the original administrative experts are no longer available.

The next release still needs implementation, the new profile, disclosure approval
and regression testing. These are implementation decisions, not a claim that
the current generator aligns non-response or fully enforces mail-group routing.
The wiki's methods and relevant "Asked when" notes will state remaining
uncertainty. Assumptions can be revised when better evidence becomes available.
