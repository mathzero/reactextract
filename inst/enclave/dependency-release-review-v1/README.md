# Dependency release selection — internal review kit v1

This kit reduces the already-prepared model bundle to a proposed release
selection. It does not query Oracle, change models, install a package, contact
the internet or authorise export. It requires base R and the existing digest
package. It leaves the original bundle untouched.

## Run inside the enclave

Unzip this code-only kit and set R's working directory to it. Then run:

```r
review_folder <- "/path/to/dependency-model-review-INTERNAL-v1-1"
selection_folder <- "/path/to/dependency-release-review-INTERNAL-v1"
source("01_prepare.R")
```

Use an existing prepared review bundle and a new destination folder. No database
connection is needed. Do not put the destination in a cloud-synchronised folder.

## What is selected

- The coefficients, factor levels and selected penalty needed for each outcome
  model, with attached solver diagnostics removed.
- The most detailed context, exact-symptom and Ct count tables. Redundant
  lower-detail fallback tables are omitted; the original bundle remains intact.
- Component-level review decisions, initially all pending, and integrity hashes.

Exact validation results, calibration counts, source configurations, vaccine
conflict counts, error logs and fitted R objects are not copied. No participant
records are required. This is a reduced review proposal, not a final generator
profile. Omitting fallback marginals may reduce generation utility; this must be
assessed after suppression rather than silently reconstructing hidden counts.

## Preliminary count protections

Any count below 10 triggers suppression of its whole conditional distribution
(all target answers for that parent combination). This deliberately withholds
more than a minimal complementary suppression scheme. Remaining counts are
rounded to the nearest five. No row totals, percentages or fallback marginals
are included to reveal the suppressed counts within that table.

This is **not** a proof of safety across overlapping tables, rounds or previously
released profiles. Observed combinations and factor-level support can themselves
reveal information. The disclosure reviewer must check these jointly, decide
whether more suppression or coarsening is needed, and assess model parameters
separately. Ridge penalties and removal of participant rows do not guarantee
privacy. There is no automatic approval or automatic export step.

## Scientific decision to retain

The project owner accepts limited precision in low-prevalence rounds for code
development. Round 3's baseline warning remains a limitation: we must not claim
that it reliably reproduces exposure–outcome relationships. Acceptance of the
other supported-group mismatch flags has not been inferred. These still need a
recorded judgement for REACT-1 rounds 6, 8, 16, 17 and 18, and REACT-2 rounds 1–3.
The flags are triage prompts, not statistical significance tests. No automatic
refitting is proposed. The missing PREVREACT field in Round 19 is not used by the
current model or participation classification; retain that source limitation.

## Give the reviewer access inside the enclave

Review the proposed `SELECTION-NOT-APPROVED.rds` together with:

- the HTML performance report and original bundle's REVIEWER.md;
- previous approved profiles (linked-table and differencing risk);
- `REVIEW_DECISIONS.csv`, listing every proposed component;
- the original bundle if the reviewer needs unsuppressed evidence.

Suggested questions for the reviewer:

1. Which model parameters and public-domain factor supports may be released?
2. Which protected tables can be released together with prior profiles? What
   additional coarsening, suppression, or exclusions are required?
3. Do any sparse combinations or large coefficients need removal or refitting?
4. What are the permitted release terms and intended uses?

Record decisions inside the enclave. Editing the decisions file does not change
or approve the payload, and invalidates its original checksum. After decisions,
we will build the final authorised selection, re-hash it, and obtain approval of
those exact files before export. Nothing here is ready to load into reactextract.

**Keep the entire generated selection inside the enclave. Do not copy it to
OneDrive or this conversation.** Initially report only whether preparation
succeeded. Numerical results and parameters require normal formal disclosure
approval. The code-only kit, not its outputs, is delivered via OneDrive.
