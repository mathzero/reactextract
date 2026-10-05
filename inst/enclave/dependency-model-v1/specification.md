---
title: "Synthetic dependencies — step 1: model specification"
date: "2026-09-28"
lang: en-GB
---

## Where we are

The sequential modelling approach has been approved. This document makes its first implementation choices explicit. It is a **draft fitting specification**, not a fitted model, disclosure approval or a change to the released generator.

**Updated delivery approach:** one run requests all 25 rounds. It starts with **REACT-1 Round 13** for PCR and **REACT-2 Round 6** for antibodies as automatic technical checks, then continues through the remaining rounds. Both have vaccination questions. They are not assumed to represent every round. Completed rounds are saved; later round-specific failures are reported without discarding unrelated work. Review the combined exceptions rather than approve 25 separate reports.

The first all-round candidate covers participation-aware central outcome models, vaccination dose and exact-occurrence acute-symptom/Ct summaries. Product and timing effects are captured for review but held out of the initial main-effect models. Interactions and a joint symptom/raw-laboratory generator remain subsequent validation steps, not silently assumed complete. Long COVID, behaviours and later linked outcomes are outside this first fitting run.

## What the pilot must establish

1. Participation, eligibility and outcome generation can coexist without overwriting each other.
2. A compact model can represent age–vaccination–outcome together, rather than three independently generated columns.
3. The generated raw laboratory fields reproduce the existing reviewed outcome definition.
4. The model can be checked inside the enclave and packaged without releasing participant records.

## Models and order

| Step | Generated state or field | Method | Important boundary |
|---|---|---|---|
| A | NHS age context and selected linked demographics | Existing public-support distributions, with approved small joint tables where available | Keep `U_AGE` distinct from questionnaire `AGE`; never infer unavailable original mail group as a known field. |
| B | Participation state | Existing stage definitions, conditional on available context where released evidence supports this | Preserve D01–D13 decisions, partial/undetermined states and historical exceptions. The small-count assumption is not evidence for subgroup effects. |
| C | Eligible background/history fields, including vaccination | Small conditional tables in an explicit order | Generate observed response states, including missingness. Do not impute an unobserved substantive answer for a nonrespondent. |
| D | Evaluable versus non-evaluable outcome state | Small reviewed conditional table, or a compact categorical model if necessary | Missing and invalid results are not negative results. Questionnaire completion does not determine independently available PCR. |
| E | Positive versus negative among evaluable outcomes | Separate ridge-penalised logistic models for each pilot study-round | Use observed predictors and explicit missing/skip states; never discard all incomplete records through complete-case fitting. |
| F | Exact result and laboratory fields | Existing round-specific encoders plus compatible conditional distributions | Check the reviewed outcome function on the resulting raw fields; stop/report contradictions, rather than rewriting eligibility. |
| G | Acute symptom state and answers | Initially reuse the approved definitions and add compatible conditional tables in the next increment | Do not use current PCR as an eligibility gate for symptoms. Preserve symptomatic negatives and positives without recent symptoms. |
| H | Cleaned data | Existing harmonisation | No separate synthetic harmonisation logic. |

Each generated field has one owner. A later step can read an earlier field, but cannot overwrite it to force a different association. Eligibility is checked before sampling; a final check verifies invariants rather than silently repairing a poor model by deleting answers. An impossible combination stops that block and reports an issue.

The order is a factorisation of the observed distribution, not a causal claim. In particular, some survey answers are recorded after the swab or test. They must not be described as prospectively measured risk factors merely because they appear before the outcome in a sampling sequence.

## First outcome specifications

| Component | REACT-1 pilot | REACT-2 pilot |
|---|---|---|
| Round | `react1.r13` | `react2.r06` |
| Outcome | Existing `.dependency_react1_outcome()` definition; applicable result/laboratory/Ct fields | Existing `.dependency_react2_outcome()` definition; applicable `NEWRESULT` / `NEWRESULT_2` |
| Initial substantive predictors | NHS age band, recorded gender, broad ethnicity, region, IMD quintile, prior COVID history, vaccination status; household/contact/role where their source mappings and gates pass checks | NHS age band, recorded gender, broad ethnicity, region, IMD quintile, prior COVID history and vaccination status; household/activity where supported |
| Vaccination extension | Dose count first; product and elapsed time only after temporal/source checks | Dose count first; product and elapsed time only after temporal/source checks |
| First interaction | Age band × vaccination status/dose, only if the pilot data support it | Age band × vaccination status/dose, only if the pilot data support it |
| Initial exclusions | Acute symptoms as predictors of the central outcome: they are generated downstream, avoiding a cycle | Test attempt/completion must be resolved in the availability branch, not treated as ordinary risk-factor effects |
| Deferred effects | General high-order interactions, individual lineage, linked admissions/death | Detailed immunosuppression, latent protective immunity, linked admissions/death |

The current 22-link contract remains the source of established broad predictor definitions. This pilot does not claim all 22 multivariable associations can already be fitted from released pairwise tables. It also does not remove their round-specific deferrals.

### Fitting choices

- Fit within the enclave using validated aligned observation keys. Identifiers are only transient join keys, never predictors or model outputs.
- Fit separate study-round pilot models; do not pool REACT-1 and REACT-2 or borrow across pandemic periods yet.
- Use ridge shrinkage for stability. Select its strength with an enclave-only training/validation split, not repeated manual searching against exported results. Record seed, formula, factor levels, scaling and selected penalty.
- Do not use class balancing or oversampling that changes baseline positivity. Fit the extract's unweighted distribution; published population-weighted prevalence is not the target for fictional extract rows.
- Require both outcome classes and adequate internal support. Unsupported terms are removed according to a recorded fallback order; no effect is manufactured from a paper or suppressed table. Interaction first, then newly added product/time detail; keep simpler approved relationships as a declared fallback.
- Check calibration on held-out records, overall and in sufficiently supported age/vaccine/participation groups. Once the specification is frozen, refit for the release candidate and retain a separate validation report. The eventual synthetic baseline must match approved round-level targets within simulation/disclosure-rounding tolerance.
- Fitting may use an enclave-only modelling dependency. The public package only evaluates released coefficients and samples values; it need not ship a fitting library. Dependency availability will be checked before issuing the runnable kit.

## Vaccination source decisions to resolve before fitting

The dictionary contains registration and individual-questionnaire variants, including `VACCINE3` / `VACCINE3SYM`, `VACCDOSE` / `VACCDOSESYM`, dose-date variants and single/multiple-column product answers. Their presence is not proof that they are interchangeable.

For each pilot round, the kit preparation must:

1. Link the exact occurrences to questionnaire stage, wording, response codes and participation scope.
2. Identify the relevant observation date: swab collection for PCR; antibody-test date for REACT-2 where available. Do not use an arbitrary mid-round date as an exact observation date.
3. Distinguish vaccination reported at registration from changes by the individual questionnaire. Generate a consistent progression, not independently sampled dose counts at both stages.
4. Determine whether dose dates are dates or month/year text. Do not parse ambiguous month values as exact days.
5. Exclude genuinely post-test doses from a pre-test vaccination-history predictor. For imprecise dates, use a documented interval/unknown group rather than guessing which came first.
6. Keep product unknown, multiple-product and trial responses distinct until a reviewed grouping is agreed.

If timing cannot be established, fit reported vaccination status/dose at its documented survey stage and label it accordingly. Product/time-since effects wait; they do not block the simpler pilot.

### Concrete differences found in the pilot inventory

- REACT-1 R13 has both registration and later-questionnaire status, dose and date fields; REACT-2 R6's included dose/status fields use the later-questionnaire variants. A single global rule choosing the same raw name will not work.
- REACT-1 R13 dose dates have database type `DATE`, whereas REACT-2 R6 dose dates are `VARCHAR2` with DD/MM/YYYY in the question label. The latter need explicit parsing and invalid-value checks inside the enclave; the label alone does not establish that every stored value follows it.
- In REACT-1 R13, product checkbox suffixes 5 and 6 have different meanings in the two question stages: `VACCINETYPE_5` is Janssen and `_6` is Other, while `VACCINETYPESYM_5` is Other and `_6` is Janssen. Product mapping must use exact response definitions, not matching suffixes.

These are reasons to inspect mappings before writing a model-fitting script, not to silently change the established harmonisation.

## Missingness and participation

Use an observed-state model, not an implicit reconstruction of what nonrespondents would have answered. An ineligible or unreported vaccination answer is not an unvaccinated person. The same applies to employment, ethnicity, prior COVID and symptoms.

For evaluable laboratory outcomes whose questionnaire covariates are unavailable, use the available-context branch and explicitly coded missing/participation states. Sparse nonresponse strata may not support a separate effect. The fallback is a documented simpler model, not a claimed estimate of the missing association. Keep aggregate calibration checks across the full extract population.

REACT-2 needs special treatment: the current conservative participation definition uses test-process anchors. Do not independently sample a participation classification and a contradictory `ABATTEMPT`/`ABCOMP`/result combination. Treat their allowed combinations as one small process block, with result availability resolved before IgG positivity. Reuse exact existing provenance rules; don't assume all result fields are either wholly independent or wholly questionnaire-derived.

## What existing exports can and cannot supply

| Existing asset | Reuse | Still needed |
|---|---|---|
| Approved v5 marginal and outcome–predictor profiles | Public supports, round outcomes, selected pairwise checks, laboratory encoders | Joint predictor relationships and multivariable fitting |
| Approved participation conditional export | Stage/age definitions and field-by-participation/age summaries where released | Outcome constructed from several raw fields jointly with participation; vaccination/context joints |
| Compact eligibility/option bundle | Hard question/option constraints | No statistical effects are implied by those constraints |
| Publication review | Choice of relationships and important limitations | No coefficients or conditional probabilities can be copied wholesale |

The capture script conditions each field on its assigned stage (or `not_assigned`). Consequently, a laboratory field's appearance in the export does not establish a participation-conditioned PCR outcome table. Several raw laboratory fields also have to be interpreted together to derive the reviewed PCR outcome. Reconstructing those joints from separate marginals is not valid.

## Enclave kit to prepare next

The next deliverable is an all-round kit with an automatic two-round technical check and three clear steps:

1. **Check inputs:** verify source columns, key uniqueness, outcome coding, questionnaire-stage mappings and vaccination timing. Save a local preflight report; stop unsupported components with a plain-language reason.
2. **Fit and test inside the enclave:** create the small modelling dataset in memory/bounded round files inside the enclave, fit the models and generate a synthetic comparison sample. Save detailed diagnostics internally only.
3. **Prepare a review candidate:** export only approved-format aggregate diagnostics and deliberately selected compact model objects. Nothing is automatically published, copied to the public repository or promoted into the default profile.

This is a new fitting workflow, not an instruction to rerun the old whole-dictionary profile script. Existing profile exports remain immutable.

### Disclosure boundary

Coefficients and predicted probabilities are new disclosure objects; count suppression does not automatically make them safe. Candidate review must consider rare factor levels, unstable/extreme coefficients, linked releases, small strata and the model's predictions. No row-level residuals, fitted values tied to people, training records, IDs, text, exact dates or data-derived extrema leave the enclave. Metadata and clean numeric parameter arrays replace full fitted R objects, which may retain training data or environments. Reject unsupported model terms before proposing export; all models still need normal disclosure approval.

Protected aggregate tables retain the existing small-cell, complementary-suppression and rounding policy. Unknown/complementarily suppressed cells are not zeros. The D13 small-count assumption remains confined to its already approved participation scope.

## Review checkpoint

Recommended decisions for this step:

- All-round run, with REACT-1 R13 and REACT-2 R6 as automatic initial checks.
- Unweighted extract realism, not population-weighted epidemiological inference.
- Existing observed outcome definitions; missing/non-evaluable outcomes remain separate.
- Simple missing-state handling, no invented questionnaire answers for nonrespondents.
- Dose detail before product/time-since detail; temporal ambiguity remains visible.
- No public parameter release or default-generator replacement until enclave validation and disclosure review.

The accompanying generated review adds an exact public-dictionary input inventory for the two initial check rounds. Those entries are **candidates to inspect**, not a newly approved mapping based on similar variable names. The all-round kit's README supplies the execution instructions. Its internal review candidate is not an authorised export.
