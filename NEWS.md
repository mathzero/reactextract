# reactextract 0.5.6

- Requires R 4.4.0 or later without an artificial upper-version bound. CI
  checks R 4.4.1, R 4.5.1 and the current R release (currently R 4.6.x).
- Removes the obsolete package-load ceiling that rejected otherwise compatible
  installations on R 4.5 and later.
- Tests the package on R 4.4, R 4.5 and the current R release in continuous
  integration, with the offline artifact built once on the current release.

# reactextract 0.5.5

- Bundles the formally disclosure-approved `react-synthetic-profile-v5`,
  checksum-pinned to dictionary release `v1.0.0-rc14`.
- Enables the reviewed outcome-centred dependency model by default for PCR
  positivity in REACT-1, IgG antibody positivity in REACT-2 and 22 selected
  outcome–predictor relationships.
- Generates exact occurrence-specific laboratory-result representations using
  the approved Round 2 `Rejected`, Round 11 lowercase `negative` and Round 13
  `ambiguous` decisions.
- Removes the temporary rc9/v4 compatibility exception: the approved v5
  profile must match the installed rc14 dictionary exactly.

# reactextract 0.5.4

- Pins three reviewed occurrence-specific PCR result supports without applying
  global case conversion or name-based normalisation: Round 2 `FINALRESULT =
  "Rejected"` and Round 13 `RESULT = "ambiguous"` are missing/non-evaluable,
  while Round 11 `RESULT = "negative"` is negative.
- Resolves exact laboratory-result meanings by occurrence ID. An exact value
  approved in one round remains unknown everywhere else and fails closed to a
  missing PCR outcome with an aggregate issue.
- Validates targeted enclave repairs dynamically against each occurrence's
  approved support instead of expecting the same four labels in every round.
- Preserves the released mixture of exact raw representations during synthetic
  generation and changes only the round's actual PCR outcome field (`RESULT`,
  or `FINALRESULT` in Round 5).
- Updates the documented `estbinres` recipe so unfamiliar labels and Round 13
  `ambiguous` remain missing, while exact Round 11 lowercase `negative` maps to
  zero.

# reactextract 0.5.3

- Fails closed for unfamiliar REACT-1 laboratory-result labels: only exact
  `Not Detected` is PCR-negative, while every unreviewed value remains missing
  and produces an aggregate issue record.
- Adds an enclave-only aggregate diagnostic for the three rejected round/field
  pairs. It exposes whitespace, Unicode code points, byte lengths and Oracle
  byte dumps without selecting identifiers or respondent rows.
- Writes separate enclave-only and disclosure-controlled review reports. Counts
  below 10 are suppressed in the review report and all other counts are rounded
  to the nearest 5.
- Keeps the exact rejected values subject to human review; no case conversion,
  trimming or automatic result recoding has been introduced.

# reactextract 0.5.2

- Preserves the exact single-space `RESULT`/`FINALRESULT` value documented by
  the historical REACT-1 cleaner and profiles it as coded missing. It is never
  trimmed, treated as PCR-negative, or sampled as a substantive result.
- Corrects PCR outcome profiling so `Void`, exact single space, database
  missingness and reviewed administrative missing codes remain missing.
- Recalculates the REACT-1 outcome/dependency aggregates during the targeted
  v5 repair; the first candidate's affected aggregates are no longer retained.
- Makes repair failures identify the affected round, field and aggregate count
  while continuing to reject every unreviewed exact source value.
- Preserves the round-specific raw representation of missing PCR outcomes in
  synthetic data instead of converting every missing outcome to `Void`.
- Pins dictionary release `v1.0.0-rc13`.

# reactextract 0.5.1

- Corrects the exact REACT-1 negative laboratory-result label to
  `Not Detected`; spelling and case are now preserved in synthetic raw data.
- Rejects a v5 laboratory repair if any `RESULT` or `FINALRESULT` value falls
  outside the three reviewed labels, preventing plausible outcome totals from
  masking a broken raw distribution.
- Adds a targeted enclave repair for the returned v5 candidate. It re-queries
  only the 37 laboratory-result fields.
- Pins dictionary release `v1.0.0-rc12`, which records this technical source-
  label correction without changing the PCR positivity definition.

# reactextract 0.5.0

- Pins dictionary release `v1.0.0-rc11` with two reviewed central outcomes,
  22 selected outcome–predictor relationships and categorical support for all
  REACT-1 `RESULT`/`FINALRESULT` fields.
- Adds `react_profile_dependencies_source()` and a one-command enclave v5
  upgrade that profiles only changed Ct/laboratory distributions and the
  reviewed dependency fields.
- Applies two-dimensional primary and complementary suppression to every
  outcome-by-predictor table before it can leave the enclave.
- Generates coherent REACT-1 PCR result/Ct blocks and REACT-2 `NEWRESULT` /
  `NEWRESULT_2` blocks when an approved v5 profile is supplied, then samples
  the selected predictors conditionally on the outcome.
- Records whether v5 dependencies were active in every synthetic extraction
  manifest. The existing v4 profile remains usable but is explicitly reported
  as not containing the new dependency tables.

# reactextract 0.4.1

- Pins dictionary release `v1.0.0-rc10`, which separates exact zero from
  positive Ct/Cp measurements and uses the ordered bands 1–10, 11–20, 21–30,
  31–40 and 41–50.
- Adds a targeted enclave re-profile for the 50 affected round-specific fields;
  all other approved aggregate distributions are retained unchanged.
- Profiling and synthetic generation now honour explicit open and closed
  fixed-bin boundaries, so decimal measurements cannot fall into a gap or the
  wrong adjacent band.
- The formally approved v4 profile remains usable during this targeted update;
  its exact rc9-to-rc10 compatibility is recorded and limited to this reviewed
  Ct/Cp contract transition.

# reactextract 0.4.0

- Adds a tested, documentation-only guide to optional REACT-1 derived-variable
  recipes, including the round-specific historical `estbinres` definition,
  IMD quintile, fixed age groups, household size and BMI.
- `react_extract()` now defaults to efficient wide output containing the cleaned
  `data` and exact `raw_data` tables without retaining the very large detailed
  long tables.
- The new `output` argument accepts `"wide"`, `"long"`, or `"both"`. Long values
  are processed one round at a time in wide mode and retained only when
  explicitly requested.
- Extraction results now print a compact summary, and manifests record the
  selected output mode and whether long tables were retained.
- `react_write()` writes the tables present in any output mode, while
  `react_profile()` gives a direct instruction to request long output when it is
  needed.

# reactextract 0.3.3

- Bundles the formally disclosure-approved `react-synthetic-profile-v4` and
  enables `react_synthetic()` with the protected round-specific distributions
  by default.
- Corrects the category domains for 168 field-round occurrences, including the
  affected `AGE_GROUP` rounds. Of these, 151 were recovered from previously
  approved singleton-bin aggregates and only 17 required a targeted enclave
  query.
- Pins the exact approved archive, profile manifest and rc9 dictionary hashes
  before synthetic values can be generated.
- Updates the README so real enclave extraction and fictional-data development
  have parallel, directly runnable examples.

# reactextract 0.3.2

- Fixes inferred public response domains when a round's local metadata contains
  administrative missing codes but omits the ordinary categories.
- Adds a small correction workflow: 151 category distributions are recovered
  exactly from already approved singleton bins and only 17 fields are queried
  again inside the enclave.
- Withdraws the technically incomplete v3 profile before package release; it
  remains disclosure-safe but is not used for generation.
- Presents real-data extraction and synthetic code development as equal entry
  points in the package README, with transferable examples for both.

# reactextract 0.3.1

- Pins dictionary release `v1.0.0-rc9`, which corrects 168 round-specific
  fields that were treated as generic numbers despite having one unambiguous
  same-study public category definition.
- Adds a targeted enclave re-profile that replaces only those 168 aggregate
  distributions while retaining the approved routing and unaffected profile
  rows.
- Resolves inferred category domains during both enclave profiling and the
  public-domain development generator without altering literal source metadata.

# reactextract 0.3.0

- Bundles the exact checksum-fixed `react-synthetic-profile-v2-sanitised`
  aggregate profile following formal enclave disclosure approval.
- Uses approved, round-specific aggregate distributions by default while
  retaining an explicit `development = TRUE` public-domain fallback.
- Verifies the profile archive, table manifest, dictionary pin and separate
  approval record before any synthetic values are generated.

# reactextract 0.2.3

- Applies suppression and rounding to the affected-record counts in profile
  issue reports.
- Represents values outside reviewed public response domains or fixed ranges as
  an explicit synthetic missingness state, preventing the generator from
  inventing ordinary values for unsupported source records.
- Adds `react_sanitise_profile()` and an offline script to upgrade an existing
  v2 profile locally without querying Oracle again.

# reactextract 0.2.2

- Pins the checksum-approved `react_wiki` dictionary release `v1.0.0-rc8`,
  containing the corrected profiling policy approved by `mathzero`.
- Safely rebases an existing disclosure-controlled profile when routing and the
  included occurrence set are unchanged, and requires targeted replacement of
  every affected empirical field.
- Treats documented administrative Date offsets as missingness codes rather
  than calendar values.
- Adds fixed public supports for Long COVID, vaccination, deprivation, height
  components and historical dates; identifiers and operational timestamps are
  not learned.

# reactextract 0.2.1

- Correctly separates documented administrative missing codes (`-91`, `-92`,
  `-77`, `-66`, `-99`, and `-555`) from ordinary negative measurements when
  building generation profiles.
- Profiles free-text fields through present/absent database expressions so raw
  respondent text is never returned to R.
- Skips fields already confirmed unavailable in Oracle and supports targeted
  occurrence-level repairs without repeating the full profile run.
- Full future profile runs contain separately disclosure-controlled all-round
  distributions. Targeted repairs never approximate totals from suppressed
  public round tables.

# reactextract 0.2.0

- Added `react_synthetic()` as a third source for the existing extraction
  interface, with deterministic field streams and fictional round-specific IDs.
- Added schema-2, exact-occurrence enclave profiling with fixed public bins,
  disclosure preparation, checksum manifests, and profile read/write helpers.
- Pinned dictionary schema 6, including immutable questionnaire provenance,
  1,989 human-approved routing rules, 15,093 approved profiling dispositions,
  and reviewed safe bins.
- The bundled development profile uses only public response domains and is
  explicitly unsuitable for scientific inference while the enclave profile
  awaits disclosure approval.

# reactextract 0.1.7

- Applies 31 reviewed Long COVID symptom-duration decisions from the shared
  wiki dictionary. Earlier continuous DAYS/WEEKS answers are binned to the
  later categorical response format; categorical answers remain authoritative.
- Correctly links the 13 symptom fields whose number changed between REACT-1
  and REACT-2 round 6, and records every contributing raw name on the cleaned
  output.
- Leaves a cleaned value unresolved and reports an issue if both DAYS and WEEKS
  are positive for the same symptom and observation.
- Bundles the shared harmonisation notes and exposes both the human-readable
  notes and exact reviewed decision tables from R.
- Uses the full topic-prefixed concept ID for every one-column harmonised output.
  Genuine multi-item columns use the consistent
  `concept_id__field__EXACT_RAW_FIELD` form; contributing names remain in
  `column_dictionary` instead of making harmonised names unwieldy.

# reactextract 0.1.6

- Populates cleaned output for all 478 reviewed concepts and every asked
  occurrence across all feature topics.
- Keeps the 18 pre-existing-condition concepts on their approved scientific
  transforms; other concepts use transparent source-preserving coding from the
  shared wiki dictionary.
- Uses one concept column when there is at most one field per round. Genuine
  multi-item concepts retain separate `concept__field__EXACT_RAW_FIELD` component
  columns so simultaneous answers are never overwritten or arbitrarily
  coalesced.
- Adds `column_dictionary` to explain every cleaned output column and its
  source fields, rounds, and cleaning strategy.

# reactextract 0.1.5

- Replaces the percentage-only bar with round and named-stage updates showing
  requested fields, records received, and elapsed time.
- Records extraction and post-processing stage timings in the result manifest.
- Vectorises repeat-visit numbering, limits harmonisation to selected mappings,
  and indexes long tables once while producing the simple wide outputs.
- Adds the pinned, offline source-coding lookup used by the REACT wiki, with
  explicit tests for acute symptoms and previous-testing history.
- Expands enclave acceptance beyond pre-existing conditions to representative
  acute-symptom and testing-history concepts.

# reactextract 0.1.4

- Moves the confirmed Oracle field-availability notes into the pinned
  `react_wiki` dictionary bundle, alongside concept links and harmonisation
  rules, so study-specific decisions have one source of truth.

# reactextract 0.1.3

- Fixes Oracle extraction for requested rounds where the selected concept was
  not collected. Participant identifiers are retained and the simple concept
  column is left missing, as intended.

# reactextract 0.1.2

- Adds `data`, a simple table with one cleaned concept per column, and
  `raw_data`, a simple table with one exact source field per column.
- Keeps all requested round participants in the simple output and uses missing
  values when a requested concept was not present in a round.
- Adds optional round-by-round extraction progress.

# reactextract 0.1.1

- Records four `PREVREACT` fields as confirmed unavailable from the REACT-1
  round 19 enclave view, without removing them from the public dictionary.
- Allows enclave acceptance to continue when every absent field has been
  explicitly checked and recorded.

# reactextract 0.1.0

- Adds dictionary-pinned extraction for REACT-1 rounds 1–19 and REACT-2
  rounds 1–6.
- Adds Oracle and RDS/CSV/Parquet round-file sources.
- Returns long observation, raw-value, harmonised-value, issue, and manifest
  tables.
- Adds aggregate profiling and disclosure-preparation tools for Phase 3.
- Adds a metadata-only Oracle preflight and a credential-safe enclave acceptance
  runner for all 25 views and the subject crosswalk.
