# reactextract 0.4.0

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
