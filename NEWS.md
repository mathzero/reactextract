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
