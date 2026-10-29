# Age and sex gradients for pre-existing conditions

In synthetic data from the default `v5` profile, the reported pre-existing
conditions (`health.preexisting.*`, the `HEALTHA` boxes of "Do you currently
have any of the following?") depend on age and, where the source gives it, on
sex. Older synthetic participants report diabetes, high blood pressure, heart
disease and cancer far more often than younger ones; anxiety and depression are
most common in young and middle-aged adults.

**The age and sex gradients come from published public statistics for England,
not from REACT.** No REACT data or aggregates were used to build them, so they
needed no disclosure review. They set only the shape of each gradient: the odds
of reporting a condition at one age and sex, relative to another. Each round's
overall prevalence of every condition still comes from the approved `v5`
profile.

> Synthetic data are for learning and writing code, not for findings. The
> gradients make age a realistic confounder for code that uses these
> conditions; they are not estimates of how these conditions vary among REACT
> participants.

## Conditions

| Condition | Gradient | Source | Measure | Note |
|---|---|---|---|---|
| Diabetes | Age and sex | HSE 2019 | Doctor-diagnosed diabetes |  |
| Heart disease or heart problems | Age and sex | HSE 2017 | Doctor-diagnosed cardiovascular disease (heart attack, angina, heart murmur, abnormal heart rhythm, stroke or other heart condition) | Any cardiovascular disease, which also includes stroke |
| High blood pressure | Age and sex | HSE 2022 | Doctor-diagnosed high blood pressure |  |
| Stroke | Age and sex | HSE 2017 | Doctor-diagnosed stroke |  |
| Kidney disease | Age | HSE 2016 | Doctor-diagnosed chronic kidney disease | All adults: no men aged 16-24 reported it |
| Liver disease | Age | HSE 2016 | Doctor-diagnosed chronic liver disease | All adults: figures by sex rest on 158-509 people per cell, with zeros |
| Asthma | Age | HSE 2018 | Current asthma: diagnosed, with symptoms or medication in the last 12 months | All persons: child figures by sex rest on fewer than 270 children per cell |
| Other lung condition | Age | HSE 2010 | Doctor-diagnosed chronic bronchitis, emphysema or COPD | No all-adults figure is printed; the mean of men and women is used. No women aged 16-24 reported it |
| Cancer | Age and sex | NDRS 2022 | Cancer registered in the past 5 years, alive at the end of 2022 | Registry data, not self-report, used as a proxy for currently having cancer |
| Condition affecting the brain and nerves | Age | HSE 2017–18 | Long-standing condition of the nervous system | All persons: child figures by sex are small. Dementia may be coded elsewhere |
| Anaemia | Age and sex | HSE 2017–18 | Long-standing condition of the blood and related organs | 'Blood and related organs' also includes other blood and immune conditions |
| Anxiety | Age and sex | MHCYP 2017 + APMS 2023/4 | Children: any anxiety disorder; adults: generalised anxiety disorder in the past week | The 11-16 figure is used for ages 11-15 |
| Depression | Age | MHCYP 2017 + APMS 2023/4 | Children: any depressive disorder; adults: depressive episode in the past week | All persons: the APMS 2023/4 figure for men aged 16-24 (0.4%, 145 men) is unstable. The 11-16 figure is used for ages 11-15 |
| Psychiatric disorder | Age | APMS 2023/4 | Psychotic disorder in the past year (2007, 2014 and 2023/4 pooled) | Taken as severe mental illness; psychotic disorder is the APMS measure. No child figure is used |
| Weakened immune system | None | — | — | No public prevalence by age found; independent of age |
| Organ transplant recipient | None | — | — | No openly licensed prevalence by age found; independent of age |

Two conditions have no age gradient because no suitable public source was
found. Weakened immune system: no public prevalence by age exists (the Shielded
Patient List was checked; it does not cross age with condition). Organ
transplant recipient: the UK Renal Registry gives the age distribution of
kidney transplant patients only, with no reuse licence, and NHS Blood and
Transplant publishes totals only. The `overweight` box, asked in a few rounds,
also keeps the same probability at every age.

## How the conditions are generated

For each round:

1. **Who answered.** Whether a person answered the question, and if not which
   missing code they have, is generated from the profile for "None of these" as
   before. Every condition box takes the same state: a person who did not
   answer (for example `-77`, survey non-response) has the same missing value
   or code in every box. Missing values are never filled in.
2. **Conditions.** Each person who answered has a value *z* from a standard
   normal distribution: a shared tendency to report conditions, so that
   conditions occur together. Given *z*, the person reports condition *k* with
   probability

   logit P = *η*<sub>*k*</sub>(age band, sex) + *σ* *z*

   *η* is solved per round so that, averaged over *z*:
   - the odds of reporting *k* in each age band (and sex) relative to any other
     band are exactly the published odds ratios; and
   - the prevalence of *k* over this round's synthetic people who answered
     (with their own ages and sexes) equals the profile's prevalence. The
     profile, not the public figure, sets the level.
3. **Shared tendency.** *σ* is solved per round so that the share of people
   reporting no condition equals the profile's share ticking "None of these".
   Without it, conditions that are independent apart from age would leave too
   few people with no condition. In the approved profile *σ* is between 1.15 and
   1.35 on the logit scale.
4. **None of these** is ticked by exactly the people who report no condition.
   If, even with *σ* = 0, more people report no condition than the profile's
   "None of these" share, a matching share of them leave it unticked.

Ages below a source's youngest band take that band's figure. People with
unknown age take the round's average. For an age and sex gradient, people whose
recorded gender is not male or female take the all-persons figure. Conditions
with no source have the same probability at every age; they still share *z*.
Published figures below 0.05%, including printed zeros, are treated as 0.05%.

The calibration for each round is recorded in the extraction manifest
(`synthetic_condition_calibration`), with the checksum of the gradient table
(`synthetic_condition_gradients_sha256`).

## Reproducibility

The same synthetic source and seed give the same people and values. Random
draws are keyed by seed, round and field. Requesting any condition generates
the whole block of condition boxes with age (`U_AGE`) and recorded gender
(`U_GENDER`), so a person's values do not depend on which families or concepts
are extracted. This step does not change age or gender, so people match across
separate extractions and `react_hes()`.

## Limitations

- The public figures use different questions, definitions and years from
  REACT's checklist; for example, heart disease uses any cardiovascular disease
  (which includes stroke), cancer uses registrations in the past 5 years, and
  psychiatric disorder uses psychotic disorder. Only their shape is used.
- Where child and adult figures come from different surveys (anxiety and
  depression), the step between them reflects the surveys' different measures
  as well as age. Conditions without child figures give children the youngest
  adult band's figure, which overstates conditions that are rare in childhood,
  such as type 2 diabetes.
- Small survey samples make some shapes noisy. Liver disease and psychiatric
  disorder, for example, peak at 55–64 and 35–44 and fall at older ages.
- Conditions are related to age, sex and each other, but not to the outcomes
  (PCR or antibody positivity) or to other variables such as smoking,
  deprivation, ethnicity or BMI.
- The shared tendency *z* is a modelling device chosen to keep the profile's
  "None of these" share. It is not estimated from data on which conditions occur
  together, and it gives every pair of conditions the same kind of association.
- The development profile (`react_synthetic_profile(development = TRUE)`) and
  the experimental `v6` preview are unchanged: their conditions remain
  independent of age.

## Sources

| Source | Publisher, date | Tables used | Licence |
|---|---|---|---|
| [Health Survey for England 2019: Adults' health, data tables](https://digital.nhs.uk/data-and-information/publications/statistical/health-survey-for-england/2019) ([file](https://files.digital.nhs.uk/AB/BE7EBB/HSE19-Adult-health-tab.xlsx)) | NHS Digital (now NHS England), 2020-12-15 | Table 2 (doctor-diagnosed diabetes, by age and sex; 2019 column) | Open Government Licence v3.0. digital.nhs.uk terms and conditions s.8.1 (NHS England content released under the current OGL except where specified); workbook requires attribution to the Health and Social Care Information Centre |
| [Health Survey for England 2022, Part 2: Adult health, data tables](https://digital.nhs.uk/data-and-information/publications/statistical/health-survey-for-england/2022-part-2) ([file](https://files.digital.nhs.uk/34/0BBE3A/HSE-2022-Adult-health-tables.xlsx)) | NHS England, 2024-09-24 | Table 16 (doctor-diagnosed hypertension, by age and sex) | Open Government Licence v3.0. digital.nhs.uk terms and conditions s.8.1 (NHS England content released under the current OGL except where specified) |
| [Health Survey for England 2017: Cardiovascular diseases, tables (version 2)](https://digital.nhs.uk/data-and-information/publications/statistical/health-survey-for-england/2017) ([file](https://files.digital.nhs.uk/EF/5430E9/HSE2017-CVD-tab-v2.xlsx)) | NHS Digital (now NHS England), 2018-12-04 | Table 1 (rows 'Any CVD' and 'Stroke', by age and sex) | Open Government Licence v3.0. digital.nhs.uk terms and conditions s.8.1 (NHS England content released under the current OGL except where specified); workbook requires attribution to the Health and Social Care Information Centre |
| [Health Survey for England 2016: Kidney and liver disease, tables](https://digital.nhs.uk/data-and-information/publications/statistical/health-survey-for-england/health-survey-for-england-2016) ([file](https://files.digital.nhs.uk/publication/m/9/hse2016-adult-kid-liv.xlsx)) | NHS Digital (now NHS England), 2017-12-13 | Table 1 (doctor-diagnosed chronic kidney disease) and Table 11 (doctor-diagnosed chronic liver disease), by age; all adults | Open Government Licence v3.0. Workbook 'Title Sheet' rows 49-51: re-use under the Open Government Licence v3.0 |
| [Health Survey for England 2018: Asthma, data tables](https://digital.nhs.uk/data-and-information/publications/statistical/health-survey-for-england/2018/health-survey-for-england-2018-data-tables) ([file](https://files.digital.nhs.uk/62/E9B7A9/HSE18-Asthma-tab.xlsx)) | NHS Digital (now NHS England), 2019-12-03 | Table 2 (adults) and Table 10 (children), row 'Current asthma: with symptoms of asthma or taking medication'; all persons | Open Government Licence v3.0. Workbook 'Contents' sheet rows 54-56: re-use under the Open Government Licence v3.0 |
| [Health Survey for England 2018: Longstanding conditions, data tables (HSE 2017 and 2018 combined)](https://digital.nhs.uk/data-and-information/publications/statistical/health-survey-for-england/2018/health-survey-for-england-2018-data-tables) ([file](https://files.digital.nhs.uk/47/924178/HSE18-Longstanding-conditions-tab.xlsx)) | NHS Digital (now NHS England), 2019-12-03 | Table 2 (adults) and Table 9 (children), rows 'Nervous system' and 'Blood and related organs' | Open Government Licence v3.0. Workbook 'Title Sheet' rows 48-50: re-use under the Open Government Licence v3.0 |
| [Health Survey for England 2010: Respiratory health, chapter 2 (Respiratory symptoms and disease in adults)](https://digital.nhs.uk/data-and-information/publications/statistical/health-survey-for-england/health-survey-for-england-2010-respiratory-health) ([file](https://files.digital.nhs.uk/publicationimport/pub03xxx/pub03023/heal-surv-eng-2010-resp-heal-ch2-symp-adul.pdf)) | The NHS Information Centre for health and social care (now NHS England), 2011-12-15 | Table 2.11 (row 'Self-reported doctor-diagnosed chronic bronchitis, emphysema or COPD', men and women) | Not stated as OGL in the document. The PDF says 'All rights reserved' and asks for explicit reference to HSCIC ownership; digital.nhs.uk terms and conditions s.8.1 (NHS England content released under the current OGL except where specified). Only 14 published percentages are used, with attribution |
| [Cancer Prevalence Statistics England, 2022](https://digital.nhs.uk/ndrs/data/data-outputs/cancer-data-hub/cancer-prevalence) ([file](https://digital.nhs.uk/binaries/content/assets/website-assets/national-disease-registration-service/ncras/data-outputs/cancer_prevalence_england_2022_data_download.zip)) | NHS England, National Disease Registration Service (NDRS), 2025-08-28 | Sheet 'Table_2', England, 5-year prevalence (diagnosed 2018-2022, alive at 31 December 2022), crude rate per 100,000 by 5-year age band and sex | Open Government Licence v3.0. Workbook 'Title_sheet': re-use under the Open Government Licence v3.0 |
| [Adult Psychiatric Morbidity Survey: Survey of Mental Health and Wellbeing, England, 2023/4](https://digital.nhs.uk/data-and-information/publications/statistical/adult-psychiatric-morbidity-survey/survey-of-mental-health-and-wellbeing-england-2023-24) ([file 1](https://files.digital.nhs.uk/CC/E6A57B/APMS23-24-Common-mental-health-conditions-data-tables.xlsx), [file 2](https://files.digital.nhs.uk/29/D78DA0/APMS23-24-Psychotic-disorder-data-tables.xlsx)) | NHS England, 2025-11-27 | Common mental health conditions Table 1.4 (generalised anxiety disorder and depressive episode in the past week, by age and sex, 2023/4); Psychotic disorder Table 12.2 (psychotic disorder in the past year, 2007, 2014 and 2023/4 pooled, all adults) | Open Government Licence v3.0. Workbook 'Contents' sheets (common mental health conditions A47, psychotic disorder A45): re-use under the Open Government Licence v3.0 |
| [Mental Health of Children and Young People in England, 2017: Emotional disorders, tables](https://digital.nhs.uk/data-and-information/publications/statistical/mental-health-of-children-and-young-people-in-england/2017/2017) ([file](https://files.digital.nhs.uk/2C/6C0530/MHCYP%202017%20Emotional%20Disorders%20Tables.xlsx)) | NHS Digital (now NHS England), 2018-11-22 | Sheet '1' (any anxiety disorder and any depressive disorder, ages 5-10 and 11-16, by sex) | Open Government Licence v3.0. Workbook 'List of tables' sheet B50: re-use under the Open Government Licence v3.0 |

HSE is the Health Survey for England; NDRS the National Disease Registration Service; APMS the Adult Psychiatric Morbidity Survey; MHCYP the Mental Health of Children and Young People survey. All are statistics for England. Reuse is credited as: *Information from NHS England, licensed under the current version of the Open Government Licence*, with the Health Survey for England, Adult Psychiatric Morbidity Survey and Mental Health of Children and Young People survey credited to NHS Digital (formerly the Health and Social Care Information Centre) where the workbooks ask for it.

The Health Survey for England 2010 respiratory report is the only source not
explicitly released under the Open Government Licence: it predates the
statement, and its PDF reserves rights while the publisher's current terms
release its content under the licence except where specified. It is the only
public source of chronic bronchitis, emphysema or COPD by age for England; only
the 14 percentages in its Table 2.11 are used, with attribution.

## Figures used

Percent reporting the condition, by age band. Health Survey for England tables print whole percentages; the values below are the unrounded values stored in the published spreadsheets, to 2 decimal places. NDRS rates per 100,000 are shown as percentages. Other values are as printed. `scripts/condition_age_gradients/figures.csv` in the source repository gives the sheet and cell of every value. For an age-only gradient, the all-persons row is used.

**Diabetes** (HSE 2019)

| Sex | 16–24 | 25–34 | 35–44 | 45–54 | 55–64 | 65–74 | 75+ |
|---|---:|---:|---:|---:|---:|---:|---:|
| Men | 0.99 | 1.32 | 3.3 | 9.16 | 12.92 | 20.52 | 18.2 |
| Women | 0.34 | 0.77 | 2.63 | 3.61 | 9.06 | 11.64 | 15.32 |
| All | 0.67 | 1.04 | 2.96 | 6.35 | 10.96 | 15.91 | 16.59 |

**Heart disease or heart problems** (HSE 2017)

| Sex | 16–34 | 35–44 | 45–54 | 55–64 | 65–74 | 75+ |
|---|---:|---:|---:|---:|---:|---:|
| Men | 4.51 | 6.65 | 9.1 | 19.55 | 34.01 | 42.15 |
| Women | 5.09 | 6.22 | 12.38 | 12.59 | 20.9 | 33.17 |
| All | 4.8 | 6.43 | 10.75 | 16.02 | 27.21 | 37.11 |

**High blood pressure** (HSE 2022)

| Sex | 16–34 | 35–44 | 45–54 | 55–64 | 65–74 | 75+ |
|---|---:|---:|---:|---:|---:|---:|
| Men | 2.96 | 14.82 | 23.19 | 33.37 | 47.81 | 55.39 |
| Women | 2.98 | 6.61 | 13.41 | 21.28 | 43.49 | 47.41 |
| All | 2.97 | 10.66 | 18.43 | 27.27 | 45.52 | 50.94 |

**Stroke** (HSE 2017)

| Sex | 16–34 | 35–44 | 45–54 | 55–64 | 65–74 | 75+ |
|---|---:|---:|---:|---:|---:|---:|
| Men | 0.29 | 0.25 | 2.48 | 3.62 | 6.04 | 10.39 |
| Women | 0.2 | 0.27 | 1.88 | 1.96 | 4.14 | 7.51 |
| All | 0.25 | 0.26 | 2.18 | 2.78 | 5.05 | 8.77 |

**Kidney disease** (HSE 2016)

| Sex | 16–24 | 25–34 | 35–44 | 45–54 | 55–64 | 65–74 | 75+ |
|---|---:|---:|---:|---:|---:|---:|---:|
| All | 0.34 | 0.73 | 1.15 | 1.73 | 2.56 | 4.13 | 4.93 |

**Liver disease** (HSE 2016)

| Sex | 16–24 | 25–34 | 35–44 | 45–54 | 55–64 | 65–74 | 75+ |
|---|---:|---:|---:|---:|---:|---:|---:|
| All | 0.34 | 0.45 | 1.29 | 1.24 | 2.55 | 0.72 | 0.6 |

**Asthma** (HSE 2018)

| Sex | 0–3 | 4–6 | 7–9 | 10–12 | 13–15 | 16–24 | 25–34 | 35–44 | 45–54 | 55–64 | 65–74 | 75+ |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| All | 2.39 | 6.71 | 5.16 | 10.09 | 8.21 | 8.78 | 8.41 | 10.3 | 11.55 | 10.47 | 11.65 | 9.15 |

**Other lung condition** (HSE 2010)

| Sex | 16–24 | 25–34 | 35–44 | 45–54 | 55–64 | 65–74 | 75+ |
|---|---:|---:|---:|---:|---:|---:|---:|
| Men | 1 | 2 | 2 | 4 | 6 | 11 | 9 |
| Women | 0 | 2 | 4 | 5 | 8 | 10 | 8 |

**Cancer** (NDRS 2022)

| Sex | 0–4 | 5–9 | 10–14 | 15–19 | 20–24 | 25–29 | 30–34 | 35–39 | 40–44 | 45–49 | 50–54 | 55–59 | 60–64 | 65–69 | 70–74 | 75–79 | 80–84 | 85–89 | 90+ |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Men | 0.052 | 0.079 | 0.055 | 0.071 | 0.10397 | 0.16621 | 0.24205 | 0.32466 | 0.45195 | 0.62865 | 1.0907 | 1.99273 | 3.40317 | 5.20184 | 6.85035 | 8.49159 | 8.46889 | 7.78023 | 6.11309 |
| Women | 0.044 | 0.058 | 0.045 | 0.069 | 0.10928 | 0.20488 | 0.39334 | 0.64614 | 0.97042 | 1.38672 | 2.09066 | 2.61732 | 3.29205 | 4.055 | 4.52149 | 5.10846 | 5.38083 | 5.13969 | 3.83553 |
| All | 0.048 | 0.069 | 0.050 | 0.070 | 0.10659 | 0.1858 | 0.32006 | 0.49075 | 0.71748 | 1.01329 | 1.59901 | 2.31059 | 3.34666 | 4.61192 | 5.62926 | 6.68494 | 6.74339 | 6.21345 | 4.58386 |

**Condition affecting the brain and nerves** (HSE 2017–18)

| Sex | 0–1 | 2–5 | 6–9 | 10–15 | 16–24 | 25–34 | 35–44 | 45–54 | 55–64 | 65–74 | 75–84 | 85+ |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| All | 0.73 | 0.53 | 0.99 | 1.78 | 2.53 | 3.75 | 3.67 | 4.34 | 4.52 | 4.97 | 7.68 | 9.28 |

**Anaemia** (HSE 2017–18)

| Sex | 0–1 | 2–5 | 6–9 | 10–15 | 16–24 | 25–34 | 35–44 | 45–54 | 55–64 | 65–74 | 75–84 | 85+ |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Men | 0.34 | 0.49 | 0.56 | 0.34 | 0.13 | 0.14 | 0.31 | 1.08 | 0.95 | 1.98 | 1.04 | 1.49 |
| Women | 0 | 0.23 | 0.16 | 0.16 | 1.56 | 0.97 | 1.52 | 1.62 | 1.07 | 1.33 | 2.53 | 1.9 |
| All | 0.17 | 0.36 | 0.37 | 0.25 | 0.83 | 0.56 | 0.91 | 1.35 | 1.01 | 1.64 | 1.87 | 1.73 |

**Anxiety** (MHCYP 2017 + APMS 2023/4)

| Sex | 5–10 | 11–15 | 16–24 | 25–34 | 35–44 | 45–54 | 55–64 | 65–74 | 75+ |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Men | 4.4 | 6.2 | 3.6 | 8.5 | 6 | 7.1 | 6.6 | 5 | 3 |
| Women | 3.4 | 9.7 | 12 | 11.3 | 9.6 | 11.7 | 8 | 4.8 | 3.8 |
| All | 3.9 | 7.9 | 7.6 | 10.1 | 7.8 | 9.4 | 7.3 | 4.9 | 3.5 |

**Depression** (MHCYP 2017 + APMS 2023/4)

| Sex | 5–10 | 11–15 | 16–24 | 25–34 | 35–44 | 45–54 | 55–64 | 65–74 | 75+ |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| All | 0.3 | 2.7 | 3.8 | 5.3 | 3.7 | 4.5 | 4.6 | 1.8 | 1.7 |

**Psychiatric disorder** (APMS 2023/4)

| Sex | 16–24 | 25–34 | 35–44 | 45–54 | 55–64 | 65–74 | 75+ |
|---|---:|---:|---:|---:|---:|---:|---:|
| All | 0.3 | 0.6 | 0.9 | 0.4 | 0.8 | 0.2 | 0.1 |

## Maintenance

The table is built by `scripts/condition_age_gradients/build.R` from
`figures.csv`, `sources.csv` and `conditions.csv` in the same folder of the
source repository. After a rebuild, pin the printed SHA-256 as
`.condition_gradients_sha256` in `R/condition_gradients.R`. The bundled file is
`inst/extdata/condition-age-gradients-v1.rds`, with its provenance record in
`inst/extdata/condition-age-gradients-v1-provenance.csv`.
