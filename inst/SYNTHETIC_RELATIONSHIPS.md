# Relationships represented in synthetic REACT data

Applies to the default **reactextract 0.6.0.9005 development preview**, covering
REACT-1 rounds 1–19 and REACT-2 rounds 1–6. The explicitly selected `v5` profile
retains its earlier generator.

The publications below motivate the choice of relationships. The generator does
**not** copy published odds ratios or reproduce the papers' analyses. It uses
separately fitted, disclosure-approved round-specific models and protected
aggregate distributions. Generated data are for developing analysis code, not
prevalence estimation, hypothesis testing, power calculations or causal inference.

## Outcomes and research links

“Captured” means represented by a model or conditional sampling step. It does not
guarantee a significant association, its direction, or exact agreement with every
published or observed subgroup estimate. Terms can contain explicit missing states.

| Relationship represented | Current implementation and coverage | Relevant published REACT research |
|---|---|---|
| Age, recorded gender, broad ethnicity, region, deprivation, economic activity and prior COVID history → PCR positivity | Separate multivariable models for every REACT-1 round; associations may differ across rounds. | Chadeau-Hyam et al. [1] investigate infection prevalence and associated demographic/exposure factors in the Delta period. This supports the subject matter, not extrapolating their estimates to every round. |
| Household size, confirmed contact and care/key-worker role → PCR positivity | Contact: R1–19. Household size: R1 and R4–19. Care/key-worker role: R2–3 and R5–19. Missing mappings are not invented. | [1] examines household, contact and occupational context alongside infection. |
| Reported vaccination status and dose → PCR positivity | REACT-1 R8–19; age is included in the same model. Main effects only: no explicit age-by-dose interaction, product effect or time-since-dose effect. | [1] investigates vaccination and infection; its product-specific effectiveness estimates are **not** implemented as generator coefficients. |
| Demographics, deprivation, household size, economic activity and prior COVID history → IgG antibody positivity | Separate models for all six REACT-2 rounds. Care/key-worker role additionally enters R2–4. IgG positivity is not a measure of protective immunity. | Ward et al. [2] investigate antibody prevalence and social/demographic variation after the first wave. |
| Reported vaccination status/dose and age → IgG antibody positivity | REACT-2 R5–6. Product, elapsed time and detailed clinical factors are not additional modelled effects. | Ward et al. [3] investigate antibody responses following vaccination, including age, dose and other factors. Only the stated subset is captured here. |
| PCR outcome, age and participation → individual acute-symptom answers | Protected conditional tables for selected exact symptom fields in REACT-1 R2–19, where available. No new symptom tables in R1 or REACT-2. Symptoms are not used to determine questionnaire eligibility from PCR status. | Whitaker et al. [4] investigate symptoms across variant periods, age/vaccination context and Ct. The generator represents round-specific symptom–outcome associations, **not** individual variants or complete joint symptom patterns. |
| PCR outcome → Ct values | Applicable REACT-1 laboratory fields use outcome-conditioned bins: zero separately, then (0,10], (10,20], …, (50,60], with missing/outside-support separate. Draws inconsistent with the reviewed PCR definition revert to an outcome-consistent encoding. | [4] connects symptom/infection characteristics with Ct. The implemented relationship is narrower: outcome-conditioned Ct, not a fitted symptom–Ct or joint gene model. |

The outcome definitions use the package's reviewed raw-field mappings: applicable
PCR result/laboratory/Ct fields for REACT-1 and `NEWRESULT`/`NEWRESULT_2` for
REACT-2. Missing or non-evaluable results are not treated as negative.

## Dependencies between background variables

Before drawing the outcome, the generator samples these small conditional
distributions when released counts and compatible raw-field mappings exist:

| Generated state | Conditioning states |
|---|---|
| Recorded gender; region | Age band |
| Broad ethnicity; IMD quintile | Region |
| Household size; economic activity; prior COVID history; vaccination status | Participation and age band |
| Care/key-worker role | Economic activity and age band |
| Vaccine dose | Vaccination status and age band |
| Confirmed contact | Participation and household size |
| Outcome availability | Participation and age band |

These are pragmatic representations of the extract, not causal claims or a claim
that every arrow was estimated in the cited papers. Coverage follows available
round-specific definitions. Unsupported encodings retain the earlier draw and
are counted in diagnostics.

Age and shared participation remain governed by the earlier approved context
model. The new bundle's age/participation/registration tables do not replace it.
Participation is an outcome-model predictor in all REACT-1 rounds; registration
response evidence is a predictor in REACT-2 R1 and R3. Other REACT-2 models do not
therefore imply a separately estimated participation effect.

## Questionnaire and missingness constraints

These are data-collection rules, distinct from published statistical associations:

- Shared questionnaire non-response aligns `-77` across governed fields; linked
  demographics and independently available laboratory values are preserved.
- NHS-derived age context, reviewed parent/carer rules and response-option
  restrictions determine which questions/answers are available. Questionnaire
  `AGE` is not simply a duplicate of `U_AGE`.
- REACT-2 test attempt, completion and result states are generated together and
  checked against participation and questionnaire rules.
- Final guards prevent later model steps from filling in ineligible answers.

See [participation decisions](PARTICIPATION_V6_DECISIONS.md) for exceptions and
assumptions, including the explicitly assumed REACT-2 small non-response rate.

## Method and limitations

The central models are round-specific ridge-penalised logistic models of positivity
among evaluable observations. Runtime evaluation needs no fitting library.
Background, availability, symptom and Ct steps use protected conditional tables.
Usable distributions combine 99% released frequencies with a 1% prior over the
allowed public domain. A withheld conditional distribution retains the baseline
draw; suppressed counts are never interpreted as zero or reconstructed.

Questionnaire guards and laboratory consistency checks can change sampled
distributions. Fallback/guard counts appear in `issues` and `manifest`; exact
reproduction of every marginal or relationship is not promised. Sparse early
PCR rounds are imprecise, and some supported-group calibration differences remain.

Not additionally modelled here: vaccine product/timing effects, general interactions,
joint symptom vectors, individual viral lineage, Long COVID severity/duration
relationships, longitudinal reinfection, linked hospitalisation or mortality.
Some relevant fields and questionnaire routes may exist without those statistical
relationships being implemented. Respondents are independent fictional people
within each round, not a longitudinal cohort.

## Publications

1. Chadeau-Hyam M et al. (2022). *SARS-CoV-2 infection and vaccine effectiveness
   in England (REACT-1): a series of cross-sectional random community surveys.*
   The Lancet Respiratory Medicine, 10:355–366.
   [Publication](https://doi.org/10.1016/S2213-2600(21)00542-7).
2. Ward H et al. (2021). *SARS-CoV-2 antibody prevalence in England following the
   first peak of the pandemic.* Nature Communications, 12:905.
   [Publication](https://www.nature.com/articles/s41467-021-21237-w).
3. Ward H et al. (2022). *Population antibody responses following COVID-19
   vaccination in 212,102 individuals.* Nature Communications, 13:907.
   [Publication](https://www.nature.com/articles/s41467-022-28527-x).
4. Whitaker M et al. (2022). *Variant-specific symptoms of COVID-19 in a study
   of 1,542,510 adults in England.* Nature Communications, 13:6856.
   [Publication](https://www.nature.com/articles/s41467-022-34244-2).

The [Imperial REACT publication index](https://www.imperial.ac.uk/medicine/research-and-impact/groups/react-study/publications-/)
provides the wider research context; the four sources above are selected links
to relationships actually represented in this preview, not an exhaustive review.
