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
aggregate counts. If preflight passes, it performs a small extraction of
pre-existing-condition fields from REACT-1 round 1 and REACT-2 round 1.

Only metadata, aggregate counts, manifests and issue records are written. The
row-level smoke result is discarded. Nevertheless, keep the report inside the
enclave unless it has passed the normal disclosure-review process.

Four questionnaire fields (`PREVREACT` and `PREVREACTID1`–`3`) are documented
as absent from the REACT-1 round 19 enclave view following a check by
`mathzero`. They remain in the public dictionary, are reported as confirmed
unavailable, and do not prevent the smoke test from running.
