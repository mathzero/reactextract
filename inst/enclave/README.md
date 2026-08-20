# Enclave acceptance workflow

This folder contains the first scripts to run after installing `reactextract`
inside the REACT enclave.

1. Copy `connection.R.example` to a private working location as `connection.R`.
2. Supply `REACT_DB_USER` and `REACT_DB_PASSWORD` through the enclave's approved
   environment or secret mechanism. Do not type credentials into a tracked file.
3. Run:

   ```sh
   Rscript run_acceptance.R connection.R reactextract-acceptance
   ```

The runner first checks all 25 views, all 15,093 included dictionary fields and
the subject crosswalk. Field checks use `WHERE 1 = 0`; key checks return only
aggregate counts. If preflight passes, it performs a representative extraction
from REACT-1 round 10 and REACT-2 round 1 covering an approved pre-existing
condition, acute symptoms, and previous-testing history.
The smoke summary also confirms that all three areas produce cleaned output,
not only raw values.

Normal extraction now returns wide cleaned and raw tables without retaining the
large long tables. Request `output = "long"` or `output = "both"` explicitly
only when detailed typed-value provenance is needed.

Only metadata, aggregate counts, manifests and issue records are written. The
row-level smoke result is discarded. Nevertheless, keep the report inside the
enclave unless it has passed the normal disclosure-review process.

Four questionnaire fields (`PREVREACT` and `PREVREACTID1`–`3`) are documented
as absent from the REACT-1 round 19 enclave view following a check by
`mathzero`. They remain in the public dictionary, are reported as confirmed
unavailable, and do not prevent the smoke test from running.

## Targeted Long COVID duration check

To decide how the paired DAYS/WEEKS fields should be converted to the later
categorical duration questions, run:

```sh
Rscript check_long_covid_durations.R connection.R reactextract-long-covid-duration-check
```

This selects only the Long COVID duration fields. It does not select participant
identifiers and does not write respondent-level rows. The output contains
aggregate value counts, completion patterns for each DAYS/WEEKS pair, three
candidate interpretations, and a check of the categorical codes (including the
otherwise undocumented REACT-2 round 6 fields).

The aggregate counts are not automatically safe to export: keep the result
folder inside the enclave until it has passed the normal disclosure review.

## Synthetic generation profile

### Approved categorical profile

Version 0.4.0 includes the formally approved `react-synthetic-profile-v4`.
The completed correction recovered 151 exact category distributions from
previously approved singleton bins and queried only the remaining 17 fields.
The correction script remains in the release for provenance, but it does not
need to be rerun to use the bundled generator.

### Complete profile build

The schema-6 routing and profiling decisions are approved and pinned in this
package. To create the aggregate profile, run:

```sh
Rscript build_synthetic_profile.R connection.R react-synthetic-profile-v1
```

This processes exact raw fields sequentially and writes aggregate counts only.
It never writes participant identifiers or respondent text. The output already
has mechanical suppression, complementary suppression and count rounding, but it
must remain inside the enclave until it receives normal disclosure approval.

When the command finishes, check that `manifest.csv` and `metadata.csv` are in
the output folder. Then submit the whole folder through the normal disclosure
process. Do not copy it out before approval. Once approved, copy the folder back
beside this offline bundle so it can be checksum-pinned into the public
synthetic-data release.

### Repairing the version-1 profile

The first complete run exposed safe-profile policy problems as well as the
administrative-code and free-text handling issues corrected in 0.2.1. Version
0.2.2 applies the corrected, human-approved field treatments and fixed ranges
without repeating unaffected fields. If the repaired 0.2.1 profile folder is
beside this bundle, run:

```sh
Rscript enclave/repair_synthetic_profile.R connection.R react-synthetic-profile-v1-repaired react-synthetic-profile-v2
```

The repair revisits corrected-policy fields and any remaining fields named in
the earlier issue report. It skips fields already confirmed unavailable, asks
Oracle only whether free text is present, retains the existing routing checks,
and replaces only the affected aggregate rows. Submit the complete
`react-synthetic-profile-v2` folder for normal disclosure approval; do not edit
its CSV files by hand.

Before disclosure submission, protect the issue counts and represent values
outside public support as synthetic missingness. This is a local operation and
does not query Oracle:

```sh
Rscript enclave/sanitise_synthetic_profile.R react-synthetic-profile-v2 react-synthetic-profile-v2-sanitised
```

Submit `react-synthetic-profile-v2-sanitised`, not the unsanitised v2 folder.

All-round distributions are calculated only during a complete unsuppressed
profile run. They are deliberately not estimated by adding already-suppressed
round tables during a repair.
