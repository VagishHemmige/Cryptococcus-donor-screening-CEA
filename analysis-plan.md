# Health Economic Analysis Plan: Donor Serum Cryptococcal Antigen Screening in US Deceased-Donor Kidney Transplantation

**Version:** 0.1 (draft) · **Date:** 2026-10-25 · **Author:** Vagish Hemmige

This plan states the intended design of the cost-effectiveness analysis. It does not describe the current code: `ARCHITECTURE.md` describes what is implemented, and `TODO.md` lists where the code does not yet match this plan. Items marked **Decision D*n*** are intended choices not yet made; they are collected in [Open decisions](#14-open-decisions).

Reporting will follow the Consolidated Health Economic Evaluation Reporting Standards 2022 (CHEERS 2022) [@husereau2022], and methods will follow the recommendations of the Second Panel on Cost-Effectiveness in Health and Medicine [@sanders2016] and the ISPOR-SMDM Modeling Good Research Practices Task Force. This document is the health economic analysis plan referred to in CHEERS 2022 item 4. The [reporting map](#13-reporting-map) links each CHEERS item to a section of this plan.

The evidence for each input is summarized in `references/literature-review.md`. This plan defines what each input means and how its uncertainty is represented; the numbers themselves live in `R/setup.R` and are justified on that page.

---

## 1. Decision problem and objective

**Decision.** Should US organ procurement organizations (OPOs) test deceased donors for serum cryptococcal antigen (CrAg) before kidney allocation, and act on a positive result?

**Primary objective.** To estimate the incremental cost, incremental quality-adjusted life years (QALYs) and net monetary benefit (NMB) of donor serum CrAg screening, compared with no routine screening, from the perspective in [Section 4](#4-perspective-time-horizon-discounting-and-currency).

**Secondary objectives**

- Identify the inputs that drive the decision, and the threshold values at which the preferred strategy changes, for the inputs without direct evidence: donor CrAg prevalence, transmission from a screen-detected donor, and what happens to a kidney from a CrAg-positive donor.
- Estimate the expected value of further research on those inputs ([Section 9.6](#96-value-of-information)).
- Compare universal screening with screening restricted to donors at higher risk (Decision D2).

## 2. Comparators

1. **No routine screening (current practice).** Donors are not tested for serum CrAg unless clinically indicated. Donor-derived cryptococcosis is recognized, if at all, after transplantation, usually from illness in a recipient or a post-mortem finding. Recipients of other organs from that donor may then receive antifungal prophylaxis. Whether this recognition-driven prophylaxis is represented in the no-screening arm is part of Decision D10.
2. **Universal donor serum CrAg screening.** Every donor in the screened population ([Section 3](#3-population-and-setting)) has a serum CrAg lateral flow assay before allocation, and a positive result is managed by the protocol in Decision D8.
3. **Targeted screening (optional).** Screening only donors with signs of central nervous system (CNS) disease or unexplained neurological illness, the group in which most recognized US transmissions occurred [@penumarthi2021]. **Decision D2:** whether to include this strategy.

With more than two strategies, results are reported as a fully incremental analysis: strategies are ordered by cost, dominated and extendedly dominated strategies are removed, and ICERs are computed between adjacent non-dominated strategies.

## 3. Population and setting

- **Setting:** US deceased-donor kidney transplantation, current allocation era. Only kidneys are modeled; other organs from the same donor are outside the model, although they share the donor's test result.
- **Screened population.** **Decision D2:** all deceased donors, or only potential donors who die of non-traumatic illness (the population of the original 2019 analysis [@santos2019atc]), or only donors with CNS signs (the targeted strategy).
- **Unit of analysis.** **Decision D1:** the unit to which results are expressed:
  - (a) a potential donor entering OPO evaluation, whose kidneys may never be recovered or used;
  - (b) a deceased donor with at least one kidney recovered for transplant; or
  - (c) a single kidney offer.

  The unit fixes what the organ-use probability means, where the test cost is charged, and how the two recipients of one donor are counted ([Section 5.2](#52-required-structural-features)). Whichever unit is chosen is stated in every table, figure and page, and any aggregate result (for example per 10,000 donors, or per US donors in a year) is computed by explicit multiplication from the per-unit result.
- **Recipients:** adult deceased-donor kidney recipients. Pediatric recipients and multi-organ recipients are excluded.

## 4. Perspective, time horizon, discounting and currency

- **Perspective.** **Decision D3.** Proposed: US health care sector perspective for the reference case, with an impact inventory listing the societal-perspective items that are excluded or not quantified, as the Second Panel recommends [@sanders2016].
- **Time horizon.** **Decision D4.** The horizon must cover every important difference between strategies. For an accept-now-versus-decline decision this extends to the life of the graft or recipient. One horizon applies to every input: background cryptococcosis risk, the cost and QALY consequences of transplant or decline, and the consequences of cryptococcosis. An input that refers to a different period is converted to the model horizon before use, and the conversion is documented.
- **Discounting.** Costs and QALYs are discounted at the same annual rate. Proposed: 3% in the reference case, with 0% and 5% in scenario analyses [@sanders2016]. Because the model is a decision tree without cycles, discounting is applied inside the inputs that summarize future streams (for example, the QALYs and costs of accepting now), and each such input records the rate used.
- **Currency and price year.** **Decision D5.** Proposed: 2025 US dollars, with costs adjusted by the CPI-U Medical Care index (BLS series CUUR0000SAM), matching the method of `usRds::adjust_costs_for_inflation()`. The PCE Health price index is a sensitivity analysis. Each cost input records its original value, price year, index, and whether it is a cost, payment or charge. Charges are not used as costs without a cost-to-charge conversion.

## 5. Model

### 5.1 Model type

A decision tree, evaluated by expected values, for a decision whose consequences are summarized at the terminal nodes. A tree is suitable because the screening decision and its immediate consequences (test result, organ use, prophylaxis, recipient infection) occur once, around transplantation. Longer-term consequences (survival with a graft, return to dialysis, death after cryptococcosis) are summarized as terminal-node values derived from published models and cohorts ([Section 7](#7-health-outcomes)). If the endpoint values cannot be built credibly from published sources under Decision D6, a state-transition submodel for the post-transplant period will be considered, and this plan will be amended first.

### 5.2 Required structural features

The model must have the features below, whatever the decisions in [Section 14](#14-open-decisions):

1. **Probabilities.** At every chance node, branch probabilities sum to 1, and every complement is computed (1 − *p*), never entered separately. Every pathway ends in exactly one terminal node.
2. **Shared baseline organ nonuse.** Both strategies have the same probability that a kidney is not used for reasons unrelated to screening. Only the additional nonuse caused by a positive screen is attributed to screening.
3. **Test cost.** The test cost is charged once per screened donor, including donors whose kidneys are not used, according to the unit in Decision D1.
4. **Two kidneys, one donor.** Recipients of kidneys from the same donor share the donor's infection status and test result. **Decision D1** fixes whether outcomes are summed over the expected number of kidneys transplanted per donor or expressed per kidney. Recipient outcomes are counted once each, never twice.
5. **True and false positives are separate paths.** The management of a positive result (decline, confirmatory testing, prophylaxis) may depend on information available at the time, so the model allows different probabilities on true-positive and false-positive paths when the protocol in Decision D8 includes confirmation. Without confirmation, the team cannot distinguish them and the probabilities are equal.
6. **Decline is not discard.** A declined kidney may be reallocated to another recipient or discarded. **Decision D9:** how reallocation is represented (for example, an offer that is declined for the intended recipient keeps its value for another recipient, so the net loss is the intended recipient's decline-and-wait penalty only).
7. **Timing.** A positive result that is available only after implantation cannot lead to cancellation; only the prophylaxis branch applies. **Decision D7:** the share of results available before allocation, before implantation, and after implantation.
8. **Prophylaxis against background infection.** Prophylaxis given for a positive donor screen reduces background (not donor-derived) cryptococcosis only for the part of the background risk that falls inside the prophylaxis window. **Decision D11:** set this effect to 0 in the reference case, or use efficacy × the share of the background-risk window covered.
9. **One reference endpoint.** All terminal-node QALYs and costs are measured relative to the same reference ([Section 7.1](#71-reference-endpoint)), and the cryptococcosis loss is defined against the endpoint it is subtracted from.
10. **One implementation.** The pathway table is defined once and used by the base case, scenarios, one-way analysis and PSA.

### 5.3 Target condition

The donor condition the test is meant to detect is **infection that can be transmitted to a kidney recipient**. **Decision D12:** whether the model's donor prevalence means serum CrAg positivity detectable by the assay, or transmissible infection, and how sensitivity and transmission are defined to match. The three must refer to the same condition: for example, if prevalence is defined as antigenemia, transmission is the probability that a recipient of an antigenemic donor develops disease, and sensitivity is the probability that the assay detects antigenemia. Accuracy estimates measured against cryptococcal meningitis as the reference condition are not used for specificity, because they count antigenemia without meningitis as a false positive.

**Decision D13:** whether transmission and prophylaxis efficacy depend on donor antigen titer or disease stage (for example, CNS involvement), which would require a titer-stratified tree.

## 6. Inputs

### 6.1 Definitions

Each input below has one definition, which `R/setup.R`, the tree and the website use consistently. *Basis* follows the labels on the evidence page: **E** evidence, **E+J** evidence with explicit judgment, **Elicit** needs elicitation or new data, **Policy** a practice choice analyzed by scenario.

| Input | Definition | Distribution family | Basis |
|---|---|---|---|
| `p_usage` | Probability that a kidney is used, for the unit in D1, in the absence of screening | Beta | E, once D1 is fixed |
| `p_donor_cryptococcus` | Prevalence of the target condition (D12) in the screened population (D2) | Beta, or scenario values | Elicit |
| `p_transmission` | Probability of recipient cryptococcosis, given a donor with the target condition and no prophylaxis | Beta | E+J; Elicit for screen-detected donors |
| `p_spont_cryptococcus` | Cumulative background (not donor-derived) cryptococcosis risk over the model horizon (D4) | Beta | E |
| `p_sensitivity`, `p_specificity` | Accuracy of the actionable result defined by D8, for the target condition in D12 | Joint (Section 6.3) | E+J |
| `p_cancelled` | Probability that a kidney is not transplanted into the intended recipient after an actionable positive result; split by true and false positive and by reallocate or discard (D9) | Scenario values | Policy |
| `p_prophrate` | Probability that a recipient of a kidney from a donor with an actionable positive result receives prophylaxis under the screening protocol; status-quo uptake separately (D10) | Beta, or scenario values | E+J; Policy |
| `p_prophefficacy` | Relative risk reduction in donor-derived cryptococcosis from the prophylaxis regimen (D10) | Beta, or log-normal on the relative risk | E+J; Elicit |
| `cost_test` | Cost per screened donor of the test, plus the program cost if in scope (D16) | Gamma | E (price); gap (program) |
| `cost_fluconazole` | Cost of the prophylaxis regimen (D10), including drug-level monitoring if in scope | Gamma | E, once the regimen is fixed |
| `cost_disease` | Incremental cost of one episode of cryptococcosis in a kidney recipient over the horizon, excluding costs counted elsewhere (for example graft loss) | Gamma or log-normal | E+J |
| Accept-now cost and QALYs | Discounted cost and QALYs of transplanting the kidney now, relative to the reference endpoint (D6) | Joint (Section 6.3); signed distribution for cost | E+J |
| `q_loss_cryptococcus` | Discounted QALY loss from recipient cryptococcosis, against the same endpoint (Section 7.2) | Derived from components | E+J; Elicit (utility decrement) |
| Cancellation and nonacceptance values | Cost and QALYs when a kidney is not used, relative to the reference endpoint | Fixed at 0 if decline-and-wait is the reference | — |

### 6.2 Distributions

- Probabilities use Beta distributions. Where an input comes from counts, the Beta is fitted to the counts (with a stated prior, for example Jeffreys), and the effective sample size is reduced when recipients are clustered within donors. Where it comes from a reported mean and interval, the Beta is fitted to both by the method of moments or by matching quantiles, and the fit is checked.
- Nonnegative costs use gamma or log-normal distributions fitted to a mean and a measure of spread.
- Any quantity that can be negative, such as the cost of accepting now minus declining and waiting, uses a distribution that allows negative values, or is derived from components that do.
- **Policy** inputs are not given distributions in the probabilistic analysis. They are held at their reference-case value, and their alternatives are analyzed as scenarios.
- Inputs without usable evidence (**Elicit**) are given a deliberately broad placeholder distribution, labelled as such on the website, until a structured elicitation (for example, the SHELF protocol) or new data replace it. **Decision D14:** which inputs are elicited.
- Every reported interval for an input says what it is: a confidence interval from data, a credible interval, a percentile range of a prior, or a judgment or scenario range.

### 6.3 Correlated inputs

Inputs that are linked are sampled jointly, or the plan records that they are sampled independently and why. **Decision D15** fixes the method for each pair:

- **Sensitivity and specificity** of the actionable result: jointly, for example from a bivariate meta-analysis, or through a shared protocol or titer threshold.
- **Accept-now QALY gain and cost saving:** both rise with the expected additional wait after a decline, so both are derived from one expected-wait input.
- **Cryptococcosis cost and QALY loss:** both rise with disease severity (ICU admission, acute kidney injury, meningitis), so they share a severity input or are sampled with a positive correlation.
- **Accept-now QALYs and cryptococcosis QALY loss:** loss from early death is proportional to the QALYs a recipient would otherwise have, so the loss is derived from the endpoint value rather than sampled separately.
- **Recipients of the same donor:** share the donor's infection status, test result and prophylaxis efficacy draw.

## 7. Health outcomes

### 7.1 Reference endpoint

**Decision D6:** the counterfactual against which accepting a kidney is valued:

- (a) **decline and wait**, in which the candidate remains on the list and may receive a later deceased- or living-donor transplant, die, or be removed from the list; or
- (b) **dialysis indefinitely**.

Proposed: (a), because declining an offer is the realistic alternative for the intended recipient, and (b) overstates the value of the offer. Option (b) may be reported as a scenario for comparison with earlier analyses. The decline-and-wait value is built from published post-decline outcomes (later transplant, death, removal from the list, time to a later transplant) and published accept-versus-decline models and cohorts [@bowring2018; @cohen2019; @husain2019; @mohan2025; @eckman2020; @kadatz2018].

With (a), declining has value 0 by definition, and every other terminal value is relative to it. A cancelled transplant and an unused kidney then have cost 0 and QALYs 0 for the intended recipient.

### 7.2 Loss from cryptococcosis

The QALY loss from recipient cryptococcosis is built from components, against the accept-now endpoint:

- excess mortality after cryptococcosis, multiplied by the discounted QALYs the recipient would otherwise have had;
- graft loss, multiplied by the loss from returning to dialysis;
- an acute utility decrement during induction and consolidation treatment;
- any lasting neurological deficit, if evidence or elicitation supports one.

Published mortality in kidney recipients with cryptococcosis has no uninfected comparison group, so it includes background mortality. The excess is estimated against a comparable reference and the method is documented. The loss is not truncated: if early death makes the QALYs of an infected recipient lower than those of a recipient whose transplant was declined, the model keeps that result.

### 7.3 Utilities

Utilities for a functioning graft and for dialysis come from a published meta-analysis [@wyld2012]. Where both are needed, their difference is sampled rather than each one independently.

## 8. Costs

- **Items.** Donor test (reagent, and program cost if in scope, D16), prophylaxis drug and monitoring (D10), treatment of cryptococcosis, and the difference in cost between accepting now and the reference endpoint (D6).
- **Measurement.** Each cost states its source, the quantity it measures (cost, payment or charge), its price year and its index, as in [Section 4](#4-perspective-time-horizon-discounting-and-currency).
- **No double counting.** Graft-loss and dialysis costs after cryptococcosis are counted in one place only, either in the cryptococcosis cost or in the endpoint values.
- Litigation or settlement costs are outside the health care sector perspective. **Decision D17:** keep them only as an explicitly labelled scenario, or drop them.

## 9. Analysis

### 9.1 Reference case and decision rule

- Expected cost and QALYs per unit (D1) for each strategy, with incremental cost (ΔC), incremental QALYs (ΔQ), NMB = λ × ΔQ − ΔC, and, where ΔQ ≠ 0, the ICER with its quadrant on the cost-effectiveness plane [@stinnett1998; @black1990].
- The primary decision rule is NMB at λ = $100,000 per QALY. **Decision D18:** the range of λ reported alongside it, for example $50,000 to $150,000 [@neumann2014; @vanness2021].
- A strategy that saves money and gains QALYs dominates, and an ICER is not reported for it.

### 9.2 Scenario analyses

Policy inputs and structural choices are analyzed as scenarios, each changing named inputs from the reference case:

1. Cancellation after a positive screen: 0, 0.1, 0.25, 0.5, 0.8 and 1.0, separately for true and false positives where D8 allows.
2. Positive-screen protocol (D8): single unconfirmed positive; titer threshold; confirmatory test.
3. Assay failure: specificity of about 0.90, representing a defective reagent lot or a non-IMMY assay, rather than folding it into the specificity distribution.
4. Prophylaxis uptake under a screening protocol: 0.8, 0.9, 1.0; and status-quo uptake.
5. Prophylaxis effect on background infection (D11): none; efficacy × window share.
6. Reference endpoint (D6): the option not chosen as reference.
7. Discount rate 0% and 5%; PCE Health index instead of CPI-U Medical Care.
8. Targeted screening population (D2).
9. Scenarios carried from the current model (low cancellation, high prophylaxis uptake, high disease cost) are kept only if they map to one of the above or are justified separately.

### 9.3 One-way sensitivity analysis

Each probabilistic input is varied alone across its stated range (the 2.5th to 97.5th percentile of its distribution, or a stated judgment range), with all other inputs at reference-case values. Results are shown as tornado diagrams of NMB at the reference λ, ΔC and ΔQ. Policy inputs appear in the scenario analyses, not the tornado.

### 9.4 Threshold analyses

For donor prevalence, transmission from a screen-detected donor, specificity, cancellation, and test cost, the value at which NMB at the reference λ equals 0 is reported, with two-way threshold plots for prevalence against transmission and prevalence against cancellation.

### 9.5 Probabilistic sensitivity analysis

- Monte Carlo simulation with a fixed seed and a fixed number of draws (`nsim`), chosen so that the Monte Carlo standard error of mean NMB is small relative to the mean; that error is reported.
- Draws follow [Section 6](#6-inputs), with joint sampling as in [Section 6.3](#63-correlated-inputs). Any probability that feeds a complement is drawn so that every branch stays in [0, 1].
- Reported from the same set of draws: mean and 95% percentile interval of ΔC, ΔQ and NMB; the cost-effectiveness plane; the cost-effectiveness acceptability curve and frontier [@fenwick2001; @vanhout1994]; and the probability that screening is cost-effective at each λ.
- The PSA mean is compared with the deterministic reference case, and a material difference is explained.

### 9.6 Value of information

- Expected value of perfect information per unit and, scaled by the number of donors screened per year in the US over a stated technology lifetime, at the population level [@wilson2015].
- Expected value of partial perfect information (EVPPI), estimated by a regression-based method, for these groups of inputs: donor prevalence; transmission from a screen-detected donor; test accuracy; cancellation and prophylaxis practice; the endpoint values. This identifies whether a donor screening pilot, a study of stored donor sera, or an elicitation would be worth most.

### 9.7 Heterogeneity

Results are reported for donor subgroups where inputs differ in the evidence: donors with versus without CNS signs, and kidneys with high versus low KDPI, where organ use and the value of accepting now differ markedly.

### 9.8 Software

Analyses are run in R (packages listed in `R/setup.R`). Code, inputs and results are public at https://github.com/VagishHemmige/Cryptococcus-donor-screening-CEA, and a website at https://vagishhemmige.github.io/Cryptococcus-donor-screening-CEA/ documents each step. No restricted or patient-level data are used.

## 10. Validation

Validation follows the ISPOR-SMDM guidance on model transparency and validation, and is reported with a checklist such as AdViSHE:

- **Face validity:** transplant infectious disease and OPO clinicians review the tree structure, the target condition and the positive-screen protocol.
- **Verification:**
  - branch probabilities sum to 1 at every node in every PSA draw;
  - limiting cases give the expected results: prevalence 0 (screening costs the test and harms through false positives only), sensitivity and specificity 1, test cost 0, cancellation 0 with prophylaxis efficacy 0 (screening has no health effect);
  - a small version of the tree is reproduced by hand;
  - tornado endpoints match direct reruns at the same values;
  - the PSA mean is close to the deterministic result.
- **Cross-validation:** the original 2019 analysis [@santos2019atc] is reproduced under its stated inputs (for example its break-even donor probability), so that differences in this model's results can be attributed to specific changes.
- **External validity:** predicted recognized donor-derived cryptococcosis under no screening is compared with US surveillance [@penumarthi2021].

## 11. Engagement and distributional effects

- **Engagement:** clinicians in transplant infectious diseases and OPO practice inform the structure, the positive-screen protocol and the elicitation. Patient engagement is not planned; this is stated in reporting.
- **Distributional effects:** not analyzed quantitatively. The report notes groups for whom a declined offer may cost more, such as highly sensitized candidates and candidates in regions with long waits.

## 12. Limitations to address in reporting

- No study has screened deceased donors with serum CrAg, so donor prevalence, accuracy in donors, and transmission from a screen-detected donor are transported from other populations or elicited.
- Surveillance of recognized donor-derived infection under-ascertains donor infection, and transmission estimates from recognized clusters are biased upward.
- Practice after a positive screen (decline, reallocation, prophylaxis) is not observed and is analyzed as policy scenarios.
- No comparative evidence exists for prophylaxis efficacy after donor-derived exposure in transplant recipients.
- No published utility decrement exists for cryptococcosis in transplant recipients.
- A decision tree summarizes long-term consequences in terminal values, which depend on the endpoint models they are taken from.

## 13. Reporting map

| CHEERS 2022 item | Topic | Plan section |
|---|---|---|
| 1 | Title | — |
| 2 | Abstract | — |
| 3 | Background and objectives | 1 |
| 4 | Health economic analysis plan | This document |
| 5 | Study population | 3 |
| 6 | Setting and location | 3 |
| 7 | Comparators | 2 |
| 8 | Perspective | 4 |
| 9 | Time horizon | 4 |
| 10 | Discount rate | 4 |
| 11 | Selection of outcomes | 7, 9.1 |
| 12 | Measurement of outcomes | 6, 7 |
| 13 | Valuation of outcomes | 7.3 |
| 14 | Measurement and valuation of resources and costs | 8 |
| 15 | Currency, price date and conversion | 4 |
| 16 | Rationale and description of model | 5 |
| 17 | Analytics and assumptions | 5.2, 6.2, 6.3 |
| 18 | Characterizing heterogeneity | 9.7 |
| 19 | Characterizing distributional effects | 11 |
| 20 | Characterizing uncertainty | 6.2, 9.2–9.6 |
| 21 | Approach to engagement | 11 |
| 22 | Study parameters | 6; `references/literature-review.md` |
| 23 | Summary of main results | 9.1 |
| 24 | Effect of uncertainty | 9.2–9.6 |
| 25 | Effect of engagement | 11 |
| 26 | Findings, limitations, generalizability | 12 |
| 27 | Source of funding | — |
| 28 | Conflicts of interest | — |

## 14. Open decisions

| ID | Decision | Section |
|---|---|---|
| D1 | Unit of analysis (potential donor, donor with a kidney recovered, or kidney offer), and how two recipients of one donor are counted | 3, 5.2 |
| D2 | Screened population (all donors, non-traumatic deaths, or donors with CNS signs), and whether targeted screening is a third strategy | 2, 3 |
| D3 | Perspective, and the impact inventory | 4 |
| D4 | Time horizon shared by every input | 4 |
| D5 | Price year and inflation index | 4 |
| D6 | Reference endpoint: decline and wait, or dialysis indefinitely | 7.1 |
| D7 | Timing of the result relative to allocation and implantation | 5.2 |
| D8 | Positive-screen protocol: single positive, titer threshold, or confirmatory test | 5.2, 9.2 |
| D9 | Decline versus discard, and how reallocation is valued | 5.2 |
| D10 | Prophylaxis regimen (drug, dose, duration, renal adjustment, monitoring); uptake under the protocol and under current practice | 2, 6.1, 8 |
| D11 | Effect of prophylaxis on background infection: none, or efficacy × window share | 5.2 |
| D12 | Target condition for prevalence, sensitivity and transmission | 5.3 |
| D13 | Titer- or stage-dependent transmission and prophylaxis efficacy | 5.3 |
| D14 | Inputs to be elicited, and the elicitation method | 6.2 |
| D15 | Joint sampling method for each correlated pair | 6.3 |
| D16 | Whether the OPO program cost of testing is in scope | 8 |
| D17 | Litigation cost scenario: keep as labelled scenario, or drop | 8 |
| D18 | Range of willingness-to-pay thresholds reported | 9.1 |

## References

Keys refer to `references.bib`. Items without a key are to be added there.

- Husereau D, et al. Consolidated Health Economic Evaluation Reporting Standards 2022 (CHEERS 2022) statement [@husereau2022].
- Sanders GD, et al. Recommendations for conduct, methodological practices, and reporting of cost-effectiveness analyses: Second Panel on Cost-Effectiveness in Health and Medicine [@sanders2016].
- Briggs AH, et al. Model parameter estimation and uncertainty analysis: ISPOR-SMDM Modeling Good Research Practices Task Force-6 [@briggs2012].
- Eddy DM, et al. Model transparency and validation: ISPOR-SMDM Modeling Good Research Practices Task Force-7. *Med Decis Making* 2012.
- Vemer P, et al. AdViSHE: a validation-assessment tool of health-economic models for decision makers and model users. *Pharmacoeconomics* 2016.
- Strong M, Oakley JE, Brennan A. Estimating multiparameter partial expected value of perfect information from a probabilistic sensitivity analysis sample: a nonparametric regression approach. *Med Decis Making* 2014.
- Wilson ECF. A practical guide to value of information analysis [@wilson2015].
- Fenwick E, et al. Representing uncertainty: the role of cost-effectiveness acceptability curves [@fenwick2001].
- Stinnett AA, Mullahy J. Net health benefits [@stinnett1998].
- Neumann PJ, et al. Updating cost-effectiveness: the curious resilience of the $50,000-per-QALY threshold [@neumann2014].
- Vanness DJ, et al. Health opportunity costs in the United States [@vanness2021].
- Model-specific evidence: `references/literature-review.md`.
