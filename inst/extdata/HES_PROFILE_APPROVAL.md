# Approval: hes-profile-v1

Status: **pending owner approval.** Record the decision in
`hes-profile-approval.csv` (status, date, who confirmed it).

- Profile file: `hes-profile.rds`
- SHA-256: `de180761b4c05d25fc26fe6e716bdefbdae61c1c8562ab1516c87a2a4549daaa`
- Built: 29 October 2026 by `scripts/hes/01_build_hes_profile.R`
- Scope requested: release of these exact profile bytes in the public package.

What the profile contains:

- **From REACT's linked HES tables** (schema profile run 22 October 2026,
  returned through output checking):
  - column names, order and Oracle types;
  - value frequencies from counts of at least 10, rounded to 5;
  - completeness shares and 5th–95th percentiles of numeric columns;
  - whole-table row and person counts (Oracle optimiser statistics);
  - yearly counts from a 1% block sample;
  - monthly COVID-19 admission counts (rounded, at least 10).

  Suppressed counts are treated as zero and are not reconstructed. The REACT
  input files are not distributed; their SHA-256 checksums are in
  `profile$sources`.
- **Public NHS England data** (Open Government Licence v3): artificial HES
  value frequencies, and HES 2023-24 diagnosis and procedure counts by age
  band and sex.
- **Fitted parameters:** admission and appointment rates by age and sex,
  calendar factors, frailty, day-case, procedure and death scaling.

It contains no records, identifiers or counts below 10.
