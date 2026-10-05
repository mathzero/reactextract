## Scope and status

Working decisions for the **planned reactextract 0.6.0 / profile v6 upgrade**,
recorded from 22 September 2026 and extended through 28 September. They are not implemented in the released v5
generator. The 0.6.0.9004 development preview implements the supported participation
and adult-smoking components, 197 question-level context rules, 109 option restrictions
and the explicit small-count assumption in D13,
not the complete planned upgrade.
Real-data extraction continues to preserve the source values without
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

## D09 — An unavailable participation rate is not zero

This records the original fallback. D13 adds a narrowly defined exception for
shared-nonresponse cells explicitly labelled `below_10`; other withheld counts
remain unestimated under this decision.

**Working implementation decision, 24 September 2026:** a technically valid
export can still leave a distribution unusable because its counts are protected.
Do not repeat a database pull merely to obtain the same protected distribution,
combine earlier releases to recover hidden counts, or substitute a zero rate.

Shared non-response can be sampled from a round's approved binary distribution
only when both counts are available. Otherwise mark that shared state as
**unestimated**: do not invent a whole-questionnaire nonrespondent group, declare
every observation a completed survey, or borrow a rate from another study.
Under the conservative REACT-2 rule in D05, this means no inferred shared
overwrite where the rate is unavailable. Retain permitted field-level source
missingness and identify the limitation in the result manifest and documentation.
This is a fallback policy, not a claim that row-aligned non-response has been
reproduced for that round. An unavailable core distribution must remain visible
as a failed release-readiness check until the release explicitly accepts that
documented limitation; file validation must not relabel it as available.

The public-response-domain prior is for questionnaire answers, not for inventing
an empirical participation rate. In particular, do not use an arbitrary 50/50
split when the two participation counts are hidden.

Availability must also be checked within each conditioning group. A few visible
`AGE` cells do not establish a usable age distribution for every NHS-age band.
Use only questionnaire-valid public supports for an explicitly recorded fallback;
do not make `AGE` a copy of `U_AGE` or claim its observed response frequency was
recovered. Unknown source values remain unknown; a count labelled
`outside_public_support` is not permission to invent their contents.

These rules are implemented in the participation preview. They neither approve a
returned profile for disclosure nor change the released v5 generator. The exact
compact candidate received a separate recorded approval on 24 September 2026.

## D10. Complete question-level conditions approved on 24 September 2026

**Approved:** 197 corrections from the eligibility-context-v1 review, affecting
772 distinct field–round combinations. The exact reviewed proposal checksums
and approval are retained in the wiki; the package ships the same approved
conditions in a checksum-verified bundle. Version 0.6.0.9001 applies them only
to the participation-preview generator. The original rc14 dictionary and
explicit v5 generation remain unchanged.

Earlier-answer requirements remain mandatory on every adult/teen alternative.
Adults do not need a teenager's parent-confirmation answer for household counts.
Workplace contact questions also require leaving home for work: the raw option
is `LEAVEREASON_2` in Round 7 and `LEAVE2_1` subsequently. Rounds 8–10 retain the
old name in the contact instruction; the reasons question and codebook establish
the explicit mapping. Round 19 isolation intentions are hypothetical and do not
inherit a neighbouring previous-positive-test condition.

The NHS-age mail-group proxy remains a **working assumption**, not an observed
historical mail group. Unknown age does not open a restricted route. An unasked
field is missing with a structural-skip reason; no universal raw skip code is
invented. Shared non-response takes precedence on its governed fields. The
same full conditions are displayed by the wiki, grouped across matching rounds.

The nine held rules (three child-age and six vaccination-pathway instructions)
are outside this approval. New option-specific restrictions remain subject to
their own review. This step improves questionnaire validity; it does not recover
suppressed counts or estimate previously unavailable eligible-response rates.

## D11. Vaccination options approved on 24 September 2026

The 32 candidate field–round restrictions in response-options-v1 are enabled in
preview 0.6.0.9002. Seven held cases remain excluded: REACT-2 Round 5 provides no
explicit pregnancy-option restriction, and five options belong to the unresolved
REACT-1 Rounds 14–16 teenager-only vaccination-refusal pathway.

Pregnancy/breastfeeding options require Female code 2 and age 18–54. REACT-1's
questionnaire `GENDER` and `DAGE` are mapped to same-round `U_GENDER` and NHS age
as explicit modelling assumptions; REACT-2 Round 6 names those linked fields
directly. “Too young” options require age 13–17 and `INDCONF = 1`.

The whole question's earlier-answer conditions must also pass. Both sampling
and its public-category fallback exclude unavailable options. A final check
prevents subsequent writers from repopulating them. Unavailable checkbox fields
stay missing with `structural_skip_option`, rather than an invented historical
zero. Shared `survey_nonresponse` takes precedence for governed fields.

This approval does not change which fields are governed by shared participation.
The existing `VACCRUFUSE2` participation classification remains unresolved even
where its questionnaire alias `VACCREFUSE2` provides clear option evidence.
Questionnaire stage evidence alone does not establish the historical meaning of
all its missing codes. This is a remaining provenance follow-up, not a new rate
estimate. No eligible-response counts are inferred from the option restrictions.

The same exact-occurrence contract is shipped in the package and used for wiki
option notes. The seven held options, work/travel/contact proposals and other
unreviewed inventory entries cannot enter generation through this approval.

## D12. Compact rules and work/travel/contact options

Preview 0.6.0.9003 adds the 77 approved work, travel and contact option restrictions:
17 adult-only options, 42 age-16-plus options, and 18 workplace-contact options
available to adults or self-responding teenagers (`INDCONF = 1`, age 13–17).
The three Round 15 quarantine options remain held because their documented
response codes do not establish the selected state. Isolation restrictions are
not copied to ordinary leaving-home questions or the changed Round 19 section.

The 197 question rules and 109 option restrictions share 20 reusable condition
patterns. The package receives one small, checksum-pinned rules bundle with
exact round/field links and approval references; detailed questionnaire evidence
stays in the wiki. Both use the same reviewed source tables. No new package
dependency, expression language or researcher-facing argument is introduced.

Whole-question and option restrictions are applied in dependency order, so an
unavailable option cannot open a later follow-up. Shared non-response still
takes precedence on governed fields. Age remains an explicit proxy for mail
group, not a reconstruction of the original sampling records. These changes
do not provide new eligible-response rates or new enclave evidence. Historical
v5 behaviour remains available unchanged. Enclave helper kits are not removed.

## D13. An explicit approximation for a shared count below ten

**Approved modelling decision, 28 September 2026:** when the shared individual-
questionnaire nonresponse count is explicitly labelled `below_10`, use an
**assumed count of 5**, divided by the approved rounded whole-round total.
This is implemented in preview 0.6.0.9004. It is an assumption for fictional
data, not a recovered count, an empirical estimate or new disclosure approval.

The current approved profile has this exact label for the shared state in all
six REACT-2 rounds. Its complementary state is labelled `linked_complement`:
that count is hidden for protection and must not be assumed small. Merely seeing
`suppressed = TRUE`, or seeing a small complementary state, does not permit the
assumption. Both visible participation counts continue to use their released
proportions, so REACT-1's existing generation is unchanged.

The denominator comes from the already approved v5 `round_denominators`, not
from adding protected cells or recovering a hidden complement. Both profiling
routines enumerate the complete round view with its `U_PASSCODE` observation
key, without a respondent-only filter. They share the pinned rc14 registry.
Using the earlier rounded total assumes those historical views did not change
between captures; the exports do not prove an identical database snapshot.
That denominator assumption is recorded in the manifest. If the approved
denominator is missing, suppressed, duplicated or invalid, no rate is assigned.

The primary label discloses the integer range 0–9, including zero. For a rounded
denominator N, the assumed rate is 5/N. The recorded bounds are 0 to 9/(N-2),
allowing for rounding an integer denominator to the nearest five. These are
bounds conditional on the same-population/stable-snapshot assumption, not a
confidence interval or a bound on all true survey nonresponse.

The count of 5 refers to the **original whole-round population**, not five
records in each generated sample. With 1,000 fictional records per round, the
expected number is 1,000 × 5/N, usually far below one. Zero generated cases are
therefore normal; we do not force at least one or round the model rate to zero.
When generated, the shared state applies consistently across its governed
questionnaire fields. Independent linked/laboratory fields and documented
exceptions retain their existing scope.

The result records `assumed_below_10`, the assumed count, denominator source,
rate and bounds, and emits a `participation_rate_assumed` issue. The original
protected tables, suppression flags and empirical-availability checks remain
unchanged. No general imputation of suppressed answer counts is introduced.
The conservative REACT-2 definition detects only clear shared-nonresponse
cases; this does not classify every incomplete or uncertain respondent.
Participation/outcome calibration remains a separate unfinished task.

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

The compact participation summaries are now approved. The final release still
needs the held rules, remaining option restrictions, participation-aware outcome
integration and the full release acceptance checks. The preview aligns supported
shared states but does not fully enforce mail-group routing. Its REACT-2 shared
participation rates now use the explicit D13 assumption, not newly measured rates.
The wiki's methods and relevant "Asked when" notes will state remaining
uncertainty. Assumptions can be revised when better evidence becomes available.
