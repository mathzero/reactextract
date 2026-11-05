# Disclosure review: clinical-profile-v1

Written for the approver; `clinical-profile-v1` was formally approved for
release by `mathzero` on 5 November 2026, with the three changes below. The
profile passed output checking as part of the kit's v1.2 candidate, but
bundling it in reactextract makes it public, which is a separate decision
(H09). This note lists what became public, in descriptions rather than
values. The full listing, with every label, is in
`dist/clinical-profile-review/` (written by `03_disclosure_inventory.R`; git
ignores it).

- Profile: `inst/extdata/clinical-profile.rds` (rebuilt after the coarsening),
  1.4 MB, SHA-256
  `ac78e27a67c33630d14c28a1f5e29ae94efb18ced711af0821db45233d03eaf0`
- Built from: `clinical-inventory-v1.2` candidate (output-checked 5 November
  2026); the build found no count of people or rows between 1 and 9.

## Coarsened after review

The approver accepted these three changes on 5 November 2026. The build
(`01_build_clinical_profile.R`) now applies them on every rebuild, keeps no
mapping, and stops if any original label survives anywhere in the profile; a
test checks the four columns hold only neutral labels.

1. **Clinic sites.** Baseline clinical tests, `site_name` (8 named venues),
   became `Site 01` to `Site 08`; follow-up bloods, `clinic_location` (23 named
   locations), became `LC site 01` to `LC site 23`. Each set is numbered by
   descending count, so no order from the names survives; code that groups by
   site still works.
2. **Vaccination providers.** NHS vaccine records, `sending_org_code` (7 NHS
   organisation codes), became `ORG01` to `ORG07`.
3. **Olink kit lot number.** Olink samples, `panel_lot_nr` (one vendor lot),
   became `LOT01`.

The other suggestion, dropping the plate-barcode and sample-file-name
identifier shapes, was not taken: they stay as they are.

## Category labels

3,432 label sets (each column's released levels, overall): 2,561 are numeric
codes, 428 standard words (yes/no, frequency scales, sex and similar), and 443
other labels (161 distinct sets). All 443 were read, and the approver scanned
all 3,432 independently for postcodes, emails, URLs, titles, exact dates,
phone numbers and place words; the only real hits were the three coarsened
above. The rest:

- **Geography:** English region labels with their ONS codes (public), one
  region code as numbers 1–9, and a survey-area code 1–3. Keep.
- **Testing:** Pillar 2 test reasons (34 national programme codes and answer
  options; one names a city-level testing pilot), test locations (drive-in,
  walk-in, home), result and target labels, and SNOMED codes for test results
  and vaccination sites. Keep.
- **Questionnaire answer options:** long labels come from standard instruments
  and the survey's own wording (fatigue, SARC-F, GAD-7, WEMWBS, ONS4, IPAQ,
  diet, alcohol, smoking, menstrual history). They are the questions' fixed
  options, not answers in free text. The WEMWBS scale is licensed; only its
  short response labels appear. Keep.
- **Ethnicity:** ONS 18/19-category and 5-group labels. Keep.
- **Laboratory and assay vocabulary:** platform and panel names, units, QC
  flags and vendor QC notes, sample matrix and type, project stage, batch
  numbers 1–22, plate rows A–H and columns 1–12, positions 1–11, block A–D.
  All are numbers or fixed vocabulary; none holds a date, name or barcode.
  Keep.
- **Other:** availability-flag names (combinations of `has_*` flags), the
  name of a commercial testing laboratory (named in REACT's papers), vaccine
  products, REACT round labels, one questionnaire version date (a single
  level, shared by the whole wave), and one placeholder date used as a level.
  Keep.

No label looks like a person's name, staff member, address, postcode, plate or
sample barcode, or a free-text answer. The kit withholds free-text columns and
labels shared by fewer than 10 people.

## Identifier shapes

169 identifier and high-cardinality columns keep up to 10 character shapes
each (letters as `A`, digits as `9`, with counts of at least 10): participant
passcodes and REACT IDs, clinic and sample barcodes, genotyping plate barcodes
and wells, sample file names, protein and gene name shapes, and a few dates
stored as text (`9999-99-99`). They hold no values; they show formats (for
example, that a passcode is eight letters with at most one digit). The
generator uses them to make realistic identifiers. Kept as they are (the
plate-barcode and sample-file-name shapes show laboratory naming conventions;
the approver chose to keep them).

## Dates

Month counts for 132 date columns, from 1980 (smoking start dates) to
November 2024, each released month holding at least 10 people; placeholder
months (1582-10, 1900-01, 9999) are kept, as in the data. Clinic and sample
dates run month by month through 2021 and 2022. No exact dates, and no month
singles anyone out. Keep.

## Numbers, dependencies and omics

- Numeric quantiles: p05–p95 overall (p01 and p99 only from 1,000 values),
  p10–p90 within anchor levels (only from 30 values with 10 either side);
  missing-code counts; decimals and the share of whole numbers. No minima,
  maxima, means or SDs.
- Rank correlations within tables (26,310 pairs, 2 decimals, from at least 50
  pairs), baseline to follow-up rank correlations (400), and quantiles of six
  intervals between dates in whole days.
- 64 availability patterns among main-cohort members, each with at least 10
  people.
- Table sizes and completeness counts (rounded to 5, at least 10).
- Omics: 15 platforms' sample counts by visit; 67,499 feature names with
  quantiles (public protein names and UniProt IDs, Ensembl-style gene IDs,
  metabolite feature indices, XCMS m/z and retention-time labels); eigenvalues
  and loadings of up to ten components on rank-based normal scores (these cap
  each sample's influence); per-feature visit stability; library-size
  quantiles; the column order of the four long proteomics tables.

## Coverage

| Generated | Not covered yet |
|---|---|
| The 31 leaves of the wrangled release (registration, clinic tests, bloods and sequencing status at both visits, health surveys and their raw versions, the data dictionary, REACT-1/2 records, infection dates, the non-REACT registrant branch, linked vaccination and Pillar 2 records, NHS demographics, QC and date-rule leaves) | Later questionnaires (2022, 2025) and Cognitron |
| Proteomics: the platform tables (Olink, Alamar, LC-MS wide and long, all platforms stacked) and the name lookup, protein summary and detectability tables; two QC tables as structure only | The cohort's HES and GP extracts |
| PeakPantheR (positive and negative) and XCMS metabolomics, lipids | Linkage ID sets, identifier crosswalk, invitations |
| RNA-seq at both visits (profiled genes only) | Sample lookup, availability sets, provenance |
| Autoantibody cases and controls, genotyping sample lists, APOE4 carriers | `combined_data_set.rds` (only its anchors and availability patterns are used) and the RNA-seq manifests |

`scripts/clinical/README.md` says what a later profile version would need for
each source not covered.
