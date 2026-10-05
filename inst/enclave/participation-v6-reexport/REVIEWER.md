# Compact conditional export: disclosure review required

This is an aggregate-only re-export of the existing internal conditional capture.
It uses a fixed, public coarsening plan, not a choice of whichever cells happen
to escape suppression. No respondent rows or new database queries are involved.

## Proposed files

- `counts.csv`: protected counts, using public response states only.
- `domains.csv`: exact pinned public labels and response-domain definitions.
- `plan.csv`: public occurrence-level conditioning and pooling decisions.
- `issues.csv`: named unavailable fields, without failure counts or raw errors.
- `availability.csv` and `AVAILABILITY.md`: visibility/utility information
  calculated only from already protected counts.
- `manifest.csv`, `README.txt`, `checksums.csv`: fixed status and provenance.

No source path, snapshot label, source timing, key, subject crosswalk, identifier,
birthday, free-text content or unrecognised raw value is exported. The original
RDS, unsuppressed tables, per-round checkpoints and R workspace must remain inside
the enclave. The source domains are regenerated from the pinned dictionary and
compared exactly before export; arbitrary text domains are rejected.

## Same protection policy, less redundant detail

The re-export calls the **unchanged** `pf6_prepare_export()` implementation used
by the original kit. Counts below ten, including zeros, are hidden. Positive
complements are hidden in table partitions and matrix row/column margins; linked
copies share protection. Visible counts are rounded to five. There is no option
to disable or reduce these settings. No separate exact round totals are written.

Binary participation and age distributions have implicit margins, and answer
profiles can share participant subsets. **Do not interpret the absence of a
separate total as the absence of inferable margins.** Review all outputs jointly,
including prior approved v1–v5 profiles, participation diagnostics and the first
conditional export. The automated policy is not a proof against arbitrary
differencing or reconstruction. Additional withholding may be required.

Aggregation is performed on the saved unsuppressed internal counts, never on the
suppressed candidate. No hidden values are estimated or filled in. Complete
public state grids, within-round totals and shared context margins are validated
before any coarsening. Availability reports do not expose private counts.

## Statistical limitations

Participation is binary for the specific purpose of a shared `-77` gate. Its
complement includes completion, break-off and uncertainty; it is not called
“completed”. Fine participation details and the empirical participation–age
relationship are not in this proposed release. Most answer distributions pool
age within their defined participation subset; the adult smoking/vaping mask
and conditional questionnaire `AGE` are explicit exceptions. Independent fields
and the Round 9 smoking exception retain their contract treatment.

Unknown response codes stay in `outside_public_support`; their contents are
never collected. Entirely or partly hidden distributions remain unavailable or
require a public prior, not reconstruction of suppressed frequencies. Other
parent/carer and response-option conditions and refreshed outcome relationships
remain to be completed. `generator_ready` is always `FALSE` in this candidate.

The files require normal enclave disclosure approval and recorded permitted
release terms before they can be bundled, published or used outside the enclave.
An availability report is not approval and does not certify synthetic fidelity.
