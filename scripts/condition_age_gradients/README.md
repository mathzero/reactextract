# Building the condition age gradients

`inst/extdata/condition-age-gradients-v1.rds` holds the age and sex gradients
for synthetic pre-existing conditions. It is built only from public prevalence
statistics for England. No REACT data or aggregates are used, so it needs no
disclosure review.

```sh
Rscript --no-init-file scripts/condition_age_gradients/build.R
# pin the printed SHA-256 as .condition_gradients_sha256 in R/condition_gradients.R
```

The script also writes `inst/extdata/condition-age-gradients-v1-provenance.csv`,
which the package checks against the pinned checksum before loading the table.
The build is byte-for-byte reproducible.

## Files

| File | Contents |
|---|---|
| `figures.csv` | One row per condition, sex and age band: the published prevalence used (`prevalence_percent`), the value as printed, the source, the measure and the file, sheet and cell (`table_ref`) |
| `sources.csv` | Title, publisher, publication date, landing page, file, tables used and licence of each source |
| `conditions.csv` | For each condition: whether its gradient is by age and sex, by age only, or absent, and why |

Health Survey for England tables print whole percentages, so `figures.csv`
holds the unrounded values stored in the published spreadsheet cells, to 2
decimal places, with the printed value alongside. NDRS cancer prevalence is
printed per 100,000 and is divided by 1,000. Other values are as printed. A
printed `-` (no observations) is 0. Where a source's age band overlaps the next
source's, `table_ref` says which ages it is used for.

The figures were transcribed from the files listed in `sources.csv`, downloaded
on 29 October 2026. Spreadsheet values were read cell by cell with their row
and column labels checked; the Health Survey for England 2010 values come from
the table in its PDF report. A sample was checked again independently. See
`inst/CONDITION_AGE_GRADIENTS.md` for the method, the sources and the
limitations.
