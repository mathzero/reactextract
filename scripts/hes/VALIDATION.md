# Synthetic HES validation

Generated 29 October 2026 with reactextract 0.6.0.9006 (hes-profile-v1) for 25,000 synthetic REACT participants (1,000 per round, seed 20261029), HES seed 1. Generation took 6 seconds.

REACT figures come from the disclosure-controlled schema profile of REACT's linked HES tables (22 October 2026): whole-table volumes are Oracle optimiser statistics, divided by the number of people with any GDPPR record; other figures are from a 1% block sample. Suppressed counts (1-9) count as zero.

| Table | Measure | REACT | Synthetic |
|---|---|---|---|
| APC | Episodes per person | 2.67 | 2.68 |
| APC | Share of people with any episode | 0.553 | 0.555 |
| APC | STARTAGE 5th/25th/50th/75th/95th percentiles | 20 / 44 / 59 / 70 / 81 | 20 / 43 / 59 / 69 / 81 |
| APC | Female share of episodes (SEX 2) | 0.578 | 0.580 |
| APC | Day cases (CLASSPAT 2) | 0.487 | 0.488 |
| APC | Emergency admissions (ADMIMETH 2x) | 0.332 | 0.373 |
| APC | Waiting-list admissions (ADMIMETH 11) | 0.285 | 0.259 |
| APC | First episode of spell (EPIORDER 1) | 0.894 | 0.894 |
| APC | Spell not ended (SPELEND N) | 0.085 | 0.106 |
| APC | Died in hospital (DISMETH 4) | 0.002 | 0.002 |
| APC | Delivery episodes (EPITYPE 2) | 0.032 | 0.012 |
| APC | One diagnosis only (DIAG_COUNT 1) | 0.159 | 0.180 |
| APC | Ten or more diagnoses (DIAG_COUNT >= 10) | 0.119 | 0.095 |
| APC | Episodes with a procedure | 0.702 | 0.718 |
| APC | Ethnicity White British (ETHNOS A) | 0.773 | 0.710 |
| OP | Appointments per person | 21.98 | 20.81 |
| OP | Share of people with any appointment | 0.843 | 0.844 |
| OP | ACTIVAGE 5th/25th/50th/75th/95th percentiles | 18 / 41 / 58 / 69 / 80 | 16 / 41 / 58 / 69 / 80 |
| OP | First attendances (FIRSTATT 1 or 3) | 0.309 | 0.324 |
| OP | No diagnosis recorded (NODIAGS 0) | 0.945 | 0.944 |
| OP | No procedure recorded (NOPROCS 0) | 0.771 | 0.770 |

## COVID-19 episodes per 100,000 people (U07.1/U07.2, any position)

| Month | REACT | Synthetic |
|---|---|---|
| 2020-04 | 132 | 92 |
| 2020-11 | 104 | 156 |
| 2021-01 | 274 | 356 |
| 2021-07 | 48 | 32 |
| 2022-01 | 80 | 80 |
| 2022-07 | 153 | 132 |
| 2023-01 | 74 | 48 |

## Episodes per 1,000 people by calendar year

2015 covers April to December and 2023 January to March.

| 2015 | 2016 | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 |
|---|---|---|---|---|---|---|---|---|
| 180 | 242 | 259 | 314 | 363 | 283 | 416 | 484 | 137 |

