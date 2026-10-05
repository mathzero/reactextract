# Disclosure review required

The candidate contains conditional count tables, public domain labels, a fixed
list of unavailable fields, and version/checksum metadata. It does not contain
respondent identifiers, keys, birthdays, free text, exact numeric observations,
minima/maxima or crosswalks. Both source passes use validated keys transiently.

Counts below ten, including zeros, are suppressed. Complementary suppression is
applied to table partitions and both row and column margins, linked copies share
suppression, and remaining counts are rounded to five. No separate exact totals
or unsuppressed issue counts are exported. Administrative source codes remain
separate from database missingness. All domains are fixed before reading data.

These controls are **not disclosure approval** and are not a proof against
arbitrary differencing attacks. Review the linked age/participation matrices
together and against earlier v1–v5 profiles and diagnostic exports. Pay particular
attention to small age/stage groups and rare questionnaire answers. Additional
suppression or withholding whole tables may be necessary.

The public contract's remaining unresolved questionnaire conditions are not
silently interpreted as complete eligibility. This candidate cannot yet be
distributed as a generator-ready v6 profile. Approval of this candidate does not
approve later joins or derived/recombined tables automatically.

Only the fixed CSV/TXT files and their checksums in the candidate directory are
proposed for transfer. The INTERNAL checkpoint directory must stay in the enclave.
