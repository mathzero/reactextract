# reactextract 0.7.0

The first release since 0.5.6. It brings together development versions
0.6.0.9003 to 0.6.0.9011. The approved `v5` profile remains the default for
REACT-1/2.

## Synthetic REACT Clinical Study

- Adds `react_clinical()`, which returns the Clinical Study's wrangled release,
  the nested list saved as `ge_lc_wrangled.rds`: synthetic tables from
  `react_synthetic()`, or the real list from the new `react_wrangled()` source,
  which can also be checked against a profile's inventory. Synthetic tables
  copy the inventory's table paths, column names, order and classes, including
  tables covering the whole REACT study (generated for every synthetic
  person), a branch for registrants who weren't REACT participants, tables
  whose row count is suppressed (no rows) and date limits. The cohort is drawn
  from the source's synthetic people, and each `subject_id` is a nine-digit
  number made from the person's `SUBJECT_ID`. Values follow the profile's
  distributions within sex, age group, long COVID group, main-cohort membership
  and the other anchors; columns of one table follow its rank correlations;
  follow-up values are correlated with baseline values; dates in different
  tables follow the profiled intervals; tables of one visit nest; and
  placeholder dates are kept.
- Adds `react_clinical_omics()`, which returns synthetic omics files for the
  same cohort, each shaped like the object read from the real file (proteomics
  tables in wide, long and stacked layouts, metabolomics tables, tximport lists
  of RNA-seq matrices, genotype and autoantibody tables). Samples go to people
  with the matching availability flag; values follow each platform's low-rank
  structure and feature quantiles, with tables of the same measurements
  sharing values.
- Bundles `clinical-profile-v1`, formally approved for release on 5 November
  2026. `react_clinical_profile()` returns it by default, after checking its
  approval record and pinned SHA-256, so `react_clinical()` and
  `react_clinical_omics()` need no local files. A checked candidate of the
  clinical inventory kit (v1.2) can still be read from a local folder
  (`REACTEXTRACT_CLINICAL_PROFILE`) for development, and the kit version that
  wrote it is recorded. Long proteomics tables take the source files' column
  order, proteomics reference tables (name lookup, summaries) hold the
  generated protein names, and placeholder dates appear at the counted rates.
- `scripts/clinical/` builds the profile from the kit's v1.2 candidate (only
  what the generator reads, no counts below 10), validates synthetic output
  against it and lists what becomes public for the disclosure review. Clinic
  sites, vaccination providers' organisation codes and a kit lot number are
  replaced by neutral labels. Tests use an invented candidate.
  `inst/CLINICAL_SYNTHETIC.md` sets out the design and what is still open.
- Fidelity fixes found by the validation: availability flags are read from the
  kit's availability-pattern file; omics feature quantiles are used on their
  raw scale; numeric anchor shifts are mapped back to the overall quantiles; a
  blank cell within an anchor level keeps the level's overall share instead of
  counting as zero (complementary suppression can blank a common level);
  follow-up values link to the baseline value's rank; and people outside the
  cohort have missing anchors in population tables.

## Synthetic linked HES

- Adds `react_hes()`, which returns REACT's linked Hospital Episode Statistics
  (admitted patient care and outpatients) for a set of participants. With
  `react_synthetic()` it generates fictional records with the real tables'
  columns and types. They are calibrated to aggregates of REACT's linked HES,
  with diagnoses and procedures by age and sex from national HES statistics.
  With `react_oracle()` it queries the real `NHSE_HES_*` tables for the
  participants' `SUBJECT_ID`s. The profile (`hes-profile-v1`) is
  checksum-pinned and formally approved (29 October 2026); it is not loaded
  without its approval record. See `inst/HES_SYNTHETIC.md`; the build scripts
  are in `scripts/hes/`.
- The advanced guide covers `react_hes()` with real data and the HES profile;
  the enclave installation guide adds a one-off check that real HES column
  types match the synthetic tables. The package description mentions linked
  HES.

## Condition age and sex gradients

- Synthetic pre-existing conditions (`health.preexisting.*`) now depend on age
  and, for diabetes, heart disease, high blood pressure, stroke, cancer,
  anaemia and anxiety, on sex. Before, every condition was equally common at
  every age: diabetes and heart disease were reported by about 6% and 5% of
  every age band from under 18 to 75+. The shape of each gradient comes from
  public statistics for England (the Health Survey for England, the Adult
  Psychiatric Morbidity Survey 2023/4, the Mental Health of Children and Young
  People survey 2017 and NDRS cancer prevalence), not from REACT, and needed no
  disclosure review. Each round's prevalence of every condition still matches
  the approved `v5` profile: per round, the condition's logit in each age-sex
  cell is solved against that round's synthetic age and sex distribution so
  that the published odds ratios are kept. A shared tendency to report
  conditions keeps the profile's share ticking "None of these", which is now
  ticked only by people who report no condition. Someone who did not answer the
  question now has the same missing code in every condition box. Weakened
  immune system and organ transplant stay equally common at every age because
  no suitable source was found. Requesting any condition generates the whole
  block with age and gender, so values do not depend on which families or
  concepts are extracted, and age and gender are unchanged. The gradients are
  bundled as the checksum-pinned `condition-age-gradients-v1` with a provenance
  record and are built by `scripts/condition_age_gradients/`; see
  `inst/CONDITION_AGE_GRADIENTS.md`. The extraction manifest records each
  round's calibration. The development profile and the experimental `v6`
  preview are unchanged.
- Documents how the synthetic relationships combine. Synthetic hospital records
  depend only on age and sex, so test results, symptoms and reported conditions
  have no effect on them. Because conditions, outcomes and hospital records all
  depend on age, crude comparisons between them differ (people reporting
  diabetes have more admissions) until age and sex are allowed for. The README,
  `inst/HES_SYNTHETIC.md` and `inst/CONDITION_AGE_GRADIENTS.md` give an
  example, and the README no longer says conditions and test results show only
  chance differences. The README and advanced guide credit the Open Government
  Licence sources of the gradients.

## Student-release changes

- `react_synthetic_profile()` and `react_synthetic()` now default to the
  approved `v5` profile. The `v6` participation preview is opt-in via
  `version = "v6"`, shows a one-time notice and is labelled experimental: it
  produces implausible distributions for several key variables (for example
  ethnicity, deprivation quintile, smoking, country of birth and gender).
- Lowers the minimum R version from 4.4.0 to 4.1.0, so the package installs on
  older R installations such as university HPC environments. No code needed
  R 4.4; the earlier floor dated from the original pin to R 4.4.x. CI now also
  checks R 4.1.3.
- Adds `react_search()`, a keyword search over concept names, raw field names
  and question wording. It returns the concept IDs to pass to `react_extract()`,
  with each concept's raw fields, rounds and typical question.
- Exports the derived-variable recipes as functions: `react_derive_estbinres()`,
  `react_derive_age_group()`, `react_derive_imd_quintile()`,
  `react_derive_household_size()`, `react_derive_bmi()` and
  `react_bmi_category()`. Adds `react_derive_igg()` for REACT-2 IgG positivity,
  following the reviewed `NEWRESULT` outcome definition. Each accepts
  `result$raw_data` or the whole extraction result.
- Adds `react_short_names()`, which drops topic prefixes from cleaned column
  names (for example `people_households.demographics.gender` becomes `gender`).
- Rewrites the README for students and researchers new to REACT: the
  synthetic-then-real workflow, the REACT studies, installation, a quick start,
  choosing variables, reading the result, derived variables, moving an analysis
  to the real data and what synthetic data do and do not capture. Real-data
  extraction, file sources, output options and harmonisation move to
  `inst/ADVANCED_GUIDE.md`, which is also added to the offline bundle.
- `react_extract()` on a synthetic source created for only some rounds now
  returns those rounds by default, instead of failing with "subscript out of
  bounds". Explicitly requesting a round without records gives a clear error.
- Fixes silent truncation of the bundled dictionary and synthetic-rule tables in
  a C/ASCII locale, which is common in batch jobs. Previously only a handful of
  rows were read, with a warning but no error.
- Printing a `v5` extraction result no longer fails; the summary names the
  synthetic model in use.
- `react_families()` no longer returns a duplicated `family` column.
- Fixes integer bins that hold a single value (for example each IMD decile),
  which drew from 1 to that value instead of returning it. Generated IMD deciles
  now follow the approved counts instead of falling from about 24% (decile 1) to
  about 1% (decile 10).
- An outcome-conditional table with a suppressed predictor category no longer
  erases that category. When the approved marginal implies the hidden category
  is far larger than primary suppression allows (complementary suppression),
  those rows keep their marginal draw. This restores, for example, the most
  deprived IMD quintile and the North East region, which were almost absent.
  No suppressed count is estimated. The approved REACT-1 age marginals withhold
  ages 85 and over, so those ages remain almost absent from REACT-1 rounds.
- Adds plausibility tests for key synthetic distributions (gender, ethnicity,
  country of birth, smoking, deprivation, PCR and IgG positivity), so that a
  profile with permuted or collapsed categories fails the test suite.

## Enclave kit: clinical inventory

- Adds an enclave kit, `inst/enclave/clinical-inventory-v1`, that inventories
  and profiles the REACT Clinical Study: every table in the wrangled release
  from the clinical pipeline, the proteomics, metabolomics, lipid,
  transcriptomics, autoantibody and genomics sample files, the later
  questionnaires, Cognitron and the linked NHS extracts. It reads files only,
  with no database connection. It writes an inventory (structure, sizes,
  completeness) and a profile (quantiles, category counts, anchored summaries,
  rank correlations, timing, availability patterns and omics structure). Both
  are disclosure-controlled for output checking: counts below 10 suppressed
  with complementary suppression, rounding to 5, no minima, maxima, means or
  SDs, and labels only when shared by at least 10 people. A self-test runs the
  kit on invented data and checks that no invented identifier, free text,
  rare level or extreme value reaches the output.
  `scripts/build_clinical_inventory_kit.R` writes the checksums and the
  transfer zip.
- Kit v1.1 repaired the seven sources that stopped in the first enclave run
  and added two. Windows-1252 free text is converted on reading. An error in
  one column, table or step is recorded and the rest is still profiled.
  Proteomics tables use the column layouts declared in `sources.R`, with raw
  and normalised values profiled separately. CSV column names are repaired as
  `read_csv()` repairs them. RNA-seq samples are linked to people through the
  manifests' BioSample names. The untargeted XCMS metabolomics table and the
  APOE4 carrier file are new sources. A partial run keeps every other
  source's checkpoint and profiles failed sources again.
- The kit in this release is v1.2, which profiles every source afresh and
  writes one candidate, replacing the first candidate and the v1.1 supplement.
  Checkpoints from earlier kit versions are never reused. Placeholder dates
  are any date up to 1900-01-01 or in the year 9999, since the enclave's SPSS
  zero dates didn't match the exact days v1.1 listed. The proteomics lookup
  and QC tables are profiled as ordinary tables, and the column order of long
  omics tables is recorded for the generator. Its candidate is the source of
  `clinical-profile-v1`.

## Experimental `v6` participation preview

These changes affect only `version = "v6"`, which stays experimental.

- Stores 197 question rules and 109 option restrictions as 20 reusable
  condition patterns in one checksum-pinned bundle shared with the REACT wiki,
  including the 77 approved work, travel and contact restrictions. Three held
  Round 15 quarantine options remain excluded; no restrictions are copied to
  unrelated ordinary leaving-home questions or the changed Round 19 section.
  One controlled condition evaluator applies option restrictions before their
  downstream questions.
- Adds the approved D13 modelling assumption for shared questionnaire
  nonresponse labelled `below_10`: five cases divided by the approved rounded
  whole-round population, not five cases in each synthetic sample. Primary
  small-cell suppression is distinguished from complementary withholding, and
  an unavailable or invalid denominator leaves the rate unestimated. Assumed
  rates, count bounds and denominator provenance are recorded in the manifest.
- Integrates checksum-pinned, user-approved models for all 25 rounds.
  Suppressed cells are kept as unknown, using explicit baseline fallbacks, not
  zero counts or reconstruction; published supports receive a 1% public prior.
  Participation and eligibility are reapplied after writes, and Ct consistency
  repairs and fallback counts are recorded.
- Held rules, product and timing effects, joint symptom patterns, REACT-2
  shared participation and outcome calibration remain unresolved.

## Known issues

- Synthetic weights recorded in stones and pounds (`WEIGHT_S`, `WEIGHT_P`) are
  implausible: they are not whole numbers, pounds are not limited to 0–13 and
  a few values run to tens of thousands. BMI from `react_derive_bmi()` is
  therefore implausible for the rows that use them. Use synthetic BMI only to
  test code.
- `react_derive_household_size()` warns on synthetic data that some rows
  populate both routed household-count alternatives (`NADULTS` and
  `NADULTS1`, or `NCHILD` and `NCHILD1`), because the generator fills both
  routes for a few per cent of REACT-1 rows. On synthetic data the warning
  can be ignored; on real data, check those rows before analysis.

# reactextract 0.5.6

- Requires R 4.4.0 or later without an artificial upper-version bound. CI
  checks R 4.4.1, R 4.5.1 and the current R release (currently R 4.6.x).
- Removes the obsolete package-load ceiling that rejected otherwise compatible
  installations on R 4.5 and later.
- Uses the checksum-fixed dictionary source files for routing and dependency
  contract hashes, avoiding R-version-specific serialized-object hashes while
  retaining explicitly reviewed compatibility with earlier profile releases.
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
