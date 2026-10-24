---
title: "Evidence base for model parameters"
date: 2026-10-19
date-modified: 2026-10-24
bibliography: ../references.bib
nocite: |
  @*
---

## About this page

This page summarizes the published evidence for every input in `R/setup.R`. It reports what the literature supports for each parameter and how strong that support is. It does not change the model: the values in `setup.R` are still the ones the website results use, and the table below compares them with the evidence.

- **Model version reviewed:** commit `f95f972` (19 Feb 2026). Parameter values were read from `R/setup.R`.
- **Setting:** US deceased-donor kidney transplantation. Kidney-specific figures are not generalized to other organs.
- **Evidence cutoff:** 19 Oct 2026. Searches covered PubMed (with PMC full text where available) and the web: journal sites, CDC, OPTN/SRTR, USRDS, BLS and BEA price indices, CMS fee schedules and drug-pricing sources.
- **Inclusion:** donor-derived infection surveillance, diagnostic accuracy studies, transplant outcome cohorts, economic studies and price sources. Reviews were used to find primary studies and are not counted alongside them.
- **Access level:** every figure is marked *full text*, *abstract*, or *secondary* (read through a summary, a news report or a copy of an official file). Abstract-only and secondary figures should be checked against the source before they are relied on.
- **Computed values:** where a number was derived from reported counts (for example an exact binomial interval), it is marked *computed*. The source authors did not report it.
- **Verification:** an independent re-read of the primary sources checked 17 key figures. 16 were confirmed. The 17th (Bowring 2018 hazard ratios) has no confidence intervals in the paper to check.
- **AI assistance:** searching and extraction were done with an AI assistant (Claude, Anthropic) and reviewed by the author. Items still to be checked by hand are listed in `TODO.md`.

**Basis labels.**

- **E:** derived from evidence.
- **E+J:** evidence-informed, with explicit judgment.
- **Elicit:** no usable evidence; needs expert elicitation or new data. A broad prior is a placeholder until then.
- **Policy:** a choice about practice, not an estimable quantity. Analyze as scenarios.

Ranges labeled *scenario* or *judgment* are not confidence intervals.

## Summary: current values against the evidence

*Current interval* is the 2.5th–97.5th percentile implied by the shape parameters in `setup.R`.

| Parameter | Current value (implied interval) | What it should represent | Evidence-supported value or range | Basis |
|---|---|---|---|---|
| `p_usage` | 0.43 (0.24–0.63) | Probability a screened donor's kidneys are used; depends on the unit of analysis | No published source for 0.43. Kidney use given recovery 0.71 nationally (2024 nonuse 29.3%), about 0.3 for KDPI ≥85. Conversion of potential to actual donors 0.19–0.48 at two OPOs | E, once the unit is fixed |
| `p_donor_cryptococcus` | 0.001 (0.00048–0.0017) | Prevalence of donor infection detectable by serum CrAg | No donor screening study exists. Recognized US donor events 1.2–2.3 × 10⁻⁴ per deceased donor, a lower bound. Scenario range 1 × 10⁻⁴ to 1 × 10⁻³ | Elicit |
| `p_transmission` | 0.867 (0.63–0.99) | P(recipient cryptococcosis \| infected donor, no prophylaxis) | Kidney recipients of recognized transmitting donors 13/19 = 0.68 (0.43–0.87, computed), biased upward. Range 0.35–0.75 for clinically infected donors. No data for screen-detected donors | E+J; Elicit for screen-detected donors |
| `p_spont_cryptococcus` | 0.005 (0.0024–0.0085) | Background (not donor-derived) cryptococcosis over the model horizon | 0.33% over about 3.5 years in US kidney recipients. 0.002–0.01 cumulative over a stated multi-year horizon | E; horizon must be stated |
| `p_sensitivity` | 0.901 (0.68–0.996) | Serum lateral-flow sensitivity in infected donors | Center 0.90–0.93; scenario range 0.75–0.99, driven by disease spectrum | E+J |
| `p_specificity` | 0.98 (0.85–1.0) | Specificity of one serum IMMY lateral-flow test in uninfected donors | Center 0.99–0.995; range 0.97–0.999. Defective-lot or non-IMMY scenario about 0.90. After confirmation plausibly >0.997 | E+J |
| `p_cancelled` | 0.8 (0.55–0.96) | Organ non-use after a positive screen | No data. No guideline found recommends discard. Infectious-risk labels raise discard only modestly | Policy |
| `p_prophrate` | 0.51 (0.30–0.72) | Prophylaxis given when a kidney from a CrAg-positive donor is used | Status quo (recognition-driven) 20/35 = 0.57 (0.41–0.72). Under a screening protocol 0.8–1.0 | E+J; Policy |
| `p_prophefficacy` | 0.88 (0.65–0.99) | Relative risk reduction in donor-derived disease | HIV primary prophylaxis RRR 0.71 (0.51–0.83). Transplant scenario range 0.3–0.95. Effect on late background infection should be much smaller | E+J; Elicit |
| `cost_test` | $2 ($0.54–4.38) | Cost per donor screen | $13.42 Medicare reimbursement (2025); about $31 list acquisition per test. Program cost (staff, 24/7 availability, confirmation) unknown | E (price); gap (program) |
| `cost_fluconazole` | $6,660 ($1,815–14,598) | Cost of a prophylaxis course | About $154 for 400 mg/day × 6 months at 2025 acquisition cost; about $312 for 12 months. Monitoring not costed | E; regimen must be specified |
| `cost_disease` | $110,945 ($30,229–243,171) | Incremental cost of one cryptococcosis episode in a kidney recipient | Payments per hospitalization $63,650 (CPI) / $61,991 (PCE), national mean; $84,923 / $82,710 private payer. No attributable, transplant-specific estimate | E+J |
| `cost_nocryptococcus` | −$10 (fixed) | Cost of accepting now, relative to the reference | Accept-now minus decline-and-wait is usually negative over multi-year horizons; needs a signed distribution | E+J; structural |
| `cost_cancellation` | $0 (fixed) | Cost of a cancelled transplant, relative to the reference | 0 if decline-and-wait is the reference | — |
| `cost_nonacceptance` | $0 (fixed) | Cost when an organ is not used | 0 if decline-and-wait is the reference | — |
| `q_nocryptococcus` | 5.5 (1.50–12.06) | QALY gain from accepting now | Versus dialysis forever: +1.17 to +2.04 over 10 years. Versus decline-and-wait: about 0.1–1.2 per offer | E+J; structural |
| `q_loss_cryptococcus` | 2.5 (0.68–5.48) | Additional QALY loss from cryptococcosis | Build from components: 6-month mortality 22%; 12-month 26–33%; graft failure up to 31% with meningitis. No published utility decrement | E+J; Elicit (decrement) |
| `q_noacceptance` | 0 (fixed) | QALYs when an organ is not used | 0 if decline-and-wait is the reference | — |
| `number_donors` | 702 (label) | Cohort label carried over from the 2019 abstract; not used as a multiplier | — | — |
| `wtp` | $100,000 | Decision threshold | Not a prior; report results across a range | — |

Only three current inputs appear in a published document: the $2 test cost, the 0.001 donor probability and the $110,945 disease cost (plus the 702-donor label). All three are from the 2019 American Transplant Congress abstract [@santos2019atc], which cites no sources for them.

The three quantities that most affect the decision have no direct evidence:

- how common CrAg positivity is among donors;
- how often a screen-detected donor transmits infection;
- what happens to a kidney from a CrAg-positive donor.

## Donor and recipient epidemiology

The main US source is the OPTN Disease Transmission Advisory Committee (DTAC) report on donor-derived cryptococcosis, 2009–2019 [@penumarthi2021] (full text).

| Parameter | Evidence | Supported range | Basis |
|---|---|---|---|
| `p_usage` | 2024 kidney nonuse was 29.3% of recovered kidneys; 1.87 kidneys recovered and 1.32 transplanted per donor [@srtr2026adrdod; @srtr2026adrkidney] (secondary). Nonuse is about 69% for KDPI ≥85% or donor age ≥65. Potential-to-actual donor conversion was 18.8% and 48.2% at two OPOs [@johnson2023] (abstract) | Kidney use given recovery 0.65–0.75 (about 0.3 for KDPI ≥85%) | E, once the unit is fixed |
| `p_donor_cryptococcus` | No donor screening study. Recognized US events, 2009–2019: 12 donors with proven or probable transmission, or 23 including donors whose recipients were protected by prophylaxis, among 101,006 deceased donors. That is 1.2–2.3 × 10⁻⁴ per donor [@penumarthi2021], a lower bound because recognition and reporting are incomplete | Scenario range 1 × 10⁻⁴ to 1 × 10⁻³ | Elicit |
| `p_transmission` | Kidney recipients of transmitting donors: 13/19 infected (0.68; exact 95% CI 0.43–0.87, computed), 13/18 excluding the one recipient given prophylaxis. The 19 recipients came from 10 donors [@penumarthi2021, Table 1]. All organs 23/34. All fungal pathogens in DTAC: 41/117 recipients (35%) [@kaul2016atc] (abstract). Single-cluster case reports [@baddley2011; @camargo2018; @macewen2013] select on transmission and are not pooled | 0.35–0.75 for clinically infected donors. No evidence for screen-detected, asymptomatic donors | E+J; Elicit |
| `p_spont_cryptococcus` | US kidney recipients 89/27,218 = 0.33% (exact CI 0.26–0.40%, computed) over about 3.5 years; median onset 616 days [@george2017, Table 1]. Taiwan 10.6 per 10,000 person-years [@chen2019] (abstract). Median onset 30.5 months in US Medicare kidney recipients [@fiorica2025] | 0.002–0.01 cumulative over a stated multi-year horizon; about 0.001–0.002 per year | E |

**Why the transmission estimate is biased upward.** A donor enters the DTAC series only if at least one recipient was infected and the infection was recognized. Those donors were also mostly ill: 10–11 of the 12 had CNS signs. Screening would mostly detect asymptomatic, low-titer donors, for whom no transmission data exist.

**Clustering.** Recipients of the same donor share the donor's infection status. The 19 kidney recipients therefore carry much less information than 19 independent observations.

**Prevalence versus reporting.** Turning the recognized-event rate into a prevalence needs the share of infected donors whose recipients are recognized and reported. No study has measured it. A prevalence of 0.001 would mean only 12–23% of infected donors are ever recognized: plausible, but unsupported.

**Overlapping cohorts.** An IDWeek abstract on DTAC data for 2008–2017 [@mehta2018] overlaps the Penumarthi series and is not pooled with it.

**Background infection timing.** Most background cases occur long after any donor-directed antifungal course would end (median 616 days to 30.5 months). This matters for how prophylaxis efficacy is applied; see Management and prevention.

**Structural notes.**

- Baseline kidney nonuse applies to both arms. Only incremental nonuse caused by a positive screen belongs to screening.
- `p_spont_cryptococcus` needs the same horizon as the QALY and cost endpoints.

## Diagnostic accuracy

No study has screened organ donors with CrAg, so every estimate is transported from other populations. The 2019 abstract gives no accuracy figures.

At a donor prevalence of 0.001, specificity 0.98 gives about 20 false positives per 1,000 donors and a positive predictive value of 4.3% (computed). At 0.995 these become about 5 per 1,000 and 15%.

| Source | Population and reference standard | Sensitivity | Specificity | Access | Note |
|---|---|---|---|---|---|
| [@huang2015] | Meta-analysis, 12 studies, mixed populations; antigen-assay references, mostly case-control | 97.6% (95.6–98.9) | 98.1% (97.4–98.6) | Full text | Design and heterogeneity inflate both values |
| [@macrae2023] | HIV-negative, 9 studies, n=528 | 96% (CrI 68–100) | 96% (CrI 84–100) | Full text | Every study at unclear or high risk of bias |
| [@temfack2021] | HIV-positive, suspected meningitis; CSF culture reference | Lateral flow 97.9% | Lateral flow 89.5% (74.3–98.5) | Full text (tables via publisher page) | Wrong target for donors: antigenemia without meningitis counts as a false positive |
| [@wang2025] | 1,459 HIV-negative inpatients | 97.7% | 99.1% (98.5–99.7) | Abstract | Strongest single low-prevalence estimate |
| [@dubbels2017] | Mayo Clinic laboratory, 3,969 patients; 13 of 38 first-time positives were false, all at titer 1:2–1:5 | — | About 99.67% (99.43–99.82), computed assuming all negatives were true | Abstract and summary | False positives cluster at low titers |
| [@fdak102286] | FDA clearance data, serum | — | 123/123 (97.0–100) | Full text | Small |
| [@miwa2024] | Liver recipients, Tokyo; latex agglutination, about 12,900 tests in 468 patients | — | ≥98.9% per patient | Full text | Different assay |
| [@hansen2013] | 589 ARUP serum specimens against Meridian EIA, no clinical reference | — | — | Full text | Reports agreement between assays, not sensitivity or specificity |

**Sensitivity by disease spectrum.**

- Meningitis in HIV: about 99–100%.
- Disseminated disease, HIV-negative: 82.6% (19/23) [@hevey2020].
- Localized lung disease: 73–91% across studies.
- Isolated lung disease in organ recipients: 73% (22/30) on older assays, including 0 of 4 lung recipients [@singh2008].

In the DTAC series, 11 of 12 transmitting donors had CNS signs, and 5 had a positive serum CrAg on retrospective testing [@penumarthi2021]. No count of donors tested exists, so donor sensitivity cannot be computed.

**What lowers specificity.**

- **Defective IMMY lots.** An FDA recall in December 2016 [@fdarecall2016] and a field safety notice in January 2022 [@immy2022fsn] describe affected lots with about 90–91% specificity. Repeating the test on the same lot does not catch this.
- **Other assays.** Dynamiker 66–86%; IMMY semi-quantitative 93.8–95.8%; Biosynex CryptoPS whole blood 96.6%, with a positive predictive value of 45.5%.
- **Cross-reactions.** *Trichosporon* and related yeasts, and *Aspergillus* (2/29 false positives in one study).
- **Rheumatoid factor and hydroxyethyl starch.** Shown for latex assays; no lateral-flow data. Relevant because donors may receive starch colloids.
- **No data** on critically ill or brain-dead patients.

**Confirmatory testing.**

- Acting only at titer ≥1:10 would remove most false positives, but in one study it kept only 39.6% of true positives among low-titer results.
- Confirming with latex or EIA can wrongly discard true low-titer positives.
- No study reports the joint accuracy of a two-step rule.
- Errors on repeat testing are correlated (lot failures, specimen interference), so independence should not be assumed.

**Supported ranges (judgment ranges, not fitted 95% intervals).**

- Sensitivity: center about 0.90–0.93; scenario range 0.75–0.99. The width reflects the unknown disease spectrum of screen-detected donors, not sampling error.
- Specificity of one IMMY serum test without confirmation: center about 0.99–0.995; range 0.97–0.999.
- Low-specificity scenario (about 0.90): a defective lot or a non-IMMY assay.
- After confirmation: plausibly above 0.997, with an unquantified loss of sensitivity.

**Biases in the accuracy studies.** Most verified only positive results, some reference standards included the index test, and many compared cases with selected controls. A few counted true low-titer positives as false when the reference assay missed them.

## Management and prevention

No published source supports the current values for cancellation (0.8), prophylaxis uptake (0.51) or prophylaxis efficacy (0.88), and the 2019 abstract gives none of them. Cancellation and uptake are better treated as policy or protocol scenarios than as estimated probabilities.

**Guidelines.** The AST guideline on donor-derived infections is Wolfe and Ison 2019 [@wolfe2019]. The AST cryptococcosis guideline is Baddley and Forrest 2019 [@baddley2019]. Neither full text was retrievable, so their donor-specific recommendations are not summarized here. The 2024 global cryptococcosis guideline [@chang2024] makes no recommendation on donor screening.

| Parameter | Evidence | Supported value or range | Basis |
|---|---|---|---|
| `p_cancelled` | No data on what happens to kidneys from known CrAg-positive donors. OPTN/DTAC guidance on donor CNS infection says organs from donors with diagnosed, treatable meningoencephalitis can be used if donor and recipients are treated [@optn2012cns]. No guideline found recommends discarding kidneys from a CrAg-positive donor. Closest proxy: a PHS increased-risk label raised kidney discard 1.13–2.17-fold by donor quality [@bowring2018] (full text), and was associated with discard at OR 1.20 (1.15–1.29) [@ariyamuthu2020] (abstract) | Scenarios 0, 0.1, 0.25, 0.5, 0.8, 1.0; split true versus false positive and decline-and-reallocate versus discard | Policy |
| `p_prophrate` | Only observed rate: DTAC donors whose recipients were protected by prophylaxis, 20/35 recipients (57%) given it. The abstract says 23/35; the Results support 20 [@penumarthi2021] | Status quo 0.41–0.72 (about 0.34–0.78 allowing for clustering within donors). Protocol scenarios 0.8–1.0 | E+J; Policy |
| `p_prophefficacy` | No comparative data in exposed transplant recipients. DTAC counts (disease in 4/7 recipients given any antifungal versus 19/27 not) are confounded and cannot be read as efficacy. Cochrane review of HIV primary prophylaxis: cryptococcal disease RR 0.29 (0.17–0.49), 7 RCTs, n≈5,000, an RRR of 0.71 (0.51–0.83) [@awotiwon2018] (full text). CrAg-negative trial: 1/760 versus 18/759 [@parkesratanshi2011] (abstract; included in the Cochrane pool). Pre-emptive fluconazole in CrAg-positive HIV patients: meningitis 5.7% (3.0–9.7) treated versus 21.4% (11.6–34.4) untreated, across studies [@temfack2019] (abstract). 3/21 treated patients developed meningitis [@meya2010] | Donor-derived risk: center about 0.7; scenario range 0.3–0.95 (judgment) | E+J; Elicit |

**Efficacy against background infection.** Background cryptococcosis in kidney recipients occurs at a median of about 20–30 months, long after any donor-directed course ends. A transparent alternative to applying full efficacy is efficacy multiplied by the share of the background-risk window the course covers: a judgment of about 0.05–0.25, or 0 in the base case.

**Titer.** About a third of asymptomatic CrAg-positive HIV patients already had meningitis, predicted by a blood titer above 1:160 [@wake2018] (abstract). Transmission and prophylaxis efficacy may both depend on donor titer.

**Regimen and interactions.**

- No verified pre-emptive regimen for transplant recipients was found. The global guideline gives HIV regimens only [@chang2024].
- Kidney case reports treat established disease with fluconazole 300–400 mg/day for 9–24 months, an upper bound for prophylaxis.
- Tacrolimus needs about a 20% dose reduction with low-dose fluconazole [@johnston2022] or about one-third with treatment doses [@cheng2022] (both abstract), with close level monitoring.
- No quantified estimate of fluconazole harm in kidney recipients was found.

## Costs

All costs are shown in source-year dollars and converted to 2025 USD by two indices: CPI-U Medical Care [@fred_cpimedns] and PCE Health [@fred_pcehealth]. The factor is index(2025) ÷ index(source year).

| Year | CPI-U Medical Care | Factor | PCE Health (2017 = 100) | Factor |
|---|---|---|---|---|
| 2025 | 580.102 (11-month mean) | 1.0000 | 118.864 | 1.0000 |
| 2024 | 563.841 | 1.0288 | 116.049 | 1.0243 |
| 2019 | 498.413 | 1.1639 | 103.247 | 1.1513 |
| 2018 | 484.707 | 1.1968 | 101.745 | 1.1683 |
| 2017 | 475.322 | 1.2204 | 100.000 | 1.1886 |
| 2016 | 463.675 | 1.2511 | 98.349 | 1.2086 |
| 2015 | 446.752 | 1.2985 | 96.830 | 1.2276 |
| 2011 | 400.257 | 1.4493 | 91.560 | 1.2982 |
| 2009 | 375.613 | 1.5444 | 87.462 | 1.3590 |

- CPI-U Medical Care (BLS series CUUR0000SAM) was read from FRED because the BLS site blocked automated access. Annual values are monthly means. October 2025 was never collected because of the federal shutdown, so 2025 is an 11-month mean, following BLS practice.
- PCE Health is BEA NIPA Table 2.5.4, read from FRED. BEA may revise the 2025 value.

### Cost inputs

| Parameter | Source | Type | Original value (year) | 2025 USD, CPI | 2025 USD, PCE | Access |
|---|---|---|---|---|---|---|
| `cost_test` | Medicare Clinical Lab Fee Schedule, CPT 87327 [@cms2025clfs] | Reimbursement | $13.42 (2025) | $13.42 | $13.42 | Secondary (state copy of the Medicare rate) |
| `cost_test` | IMMY CrAg LFA list price, $1,530 per 50 tests (Fisher Scientific catalog) | Acquisition, list | About $30.60 (Oct 2026) | — | — | Vendor page |
| `cost_test` | Malawi screening model [@hyle2025] | Program cost, low-income setting | $4 | — | — | Full text |
| `cost_fluconazole` | CMS NADAC, fluconazole 200 mg, 2025 mean $0.427 per tablet (range $0.386–0.452) [@cms2025nadac] | Pharmacy acquisition | 400 mg/day × 6 months ≈ $154; × 12 months ≈ $312; 200 mg/day × 3 months ≈ $38 (2025) | same | same | Official data |
| `cost_fluconazole` | GoodRx listed retail price, 200 mg | Retail list | About $14 per tablet; 400 mg/day × 6 months ≈ $5,065 | — | — | Web |
| `cost_disease` | Payments per cryptococcosis hospitalization, national mean ($248.0M ÷ 4,755 stays, computed) [@benedict2019] | Payer payments, whole stay, not attributable | $52,155 (2017) | $63,650 | $61,991 | Full text |
| `cost_disease` | Same, private insurance [@benedict2019] | Payer payments | $69,586 (2017) | $84,923 | $82,710 | Full text |
| `cost_disease` | Same, Medicare supplemental [@benedict2019] | Payer payments | $46,197 (2017) | $56,379 | $54,910 | Full text |
| `cost_disease` | HIV-negative median per stay [@pyrgos2013] | Unclear whether cost or charge | $16,803 (2009, assumed) | $25,951 | $22,835 | Full text |
| `cost_disease` | Modeled first-phase treatment of cryptococcal meningitis [@merry2016] | Modeled cost | $44,605–83,227 (2015) | $57,920–108,070 | $54,757–102,169 | Full text |
| `cost_disease` | Up to 5 years after cryptococcal meningitis, mostly non-transplant patients [@charalambous2018] | Charges, all-cause | $103,236 (year not stated) | — | — | Abstract |
| `cost_disease` | HIV-associated meningitis, Los Angeles, 2020–2024 [@clark2026] | Variable hospital cost | About $50,200 (standard 14-day induction); about $26,600 (single-dose regimen) | — | — | News summary only |
| `cost_disease` | Costing estimate for hospital care of cryptococcal meningitis [@idweek2017crag] | Costing estimate | About $70,000 (2016) | $87,577 | $84,602 | Abstract |

The source of the current $110,945 could not be found in published cost studies. No published US study reports the attributable cost of cryptococcosis in transplant recipients. The cost of running donor testing (staff, round-the-clock availability, confirmatory titer, OPO logistics) has no US source: it is a gap, not zero. Monitoring costs for tacrolimus levels and liver tests during fluconazole had no cost source.

### Costs for accepting now versus declining and waiting

The placeholder −$10 should be the cost difference between accepting now and declining and waiting. The evidence suggests that difference is usually negative over multi-year horizons, because dialysis costs more per year than a functioning graft. Its sign depends on the expected extra wait, so a distribution that allows negative values is needed.

| Source | Item | Original (year) | 2025 USD, CPI | 2025 USD, PCE | Access |
|---|---|---|---|---|---|
| [@usrds2018adr], Fig 9.8 | Medicare per person per year, hemodialysis | $90,971 (2016) | $113,814 | $109,948 | Full text |
| [@usrds2018adr], Fig 9.8 | Medicare per person per year, functioning transplant | $34,780 (2016) | $43,513 | $42,035 | Full text |
| [@ellison2023] (inputs) | Dialysis per year; first transplant year; later transplant years | $82,544; $115,675; $23,208 (2011) | $119,631; $167,648; $33,635 | $107,159; $150,169; $30,129 | Full text |
| [@milliman2025], Fig 3 | Kidney transplant episode | $446,800 (2025) | — | — | Billed charges, not payments |
| [@axelrod2018], Table 3 (10 years, 3%) | Transplant (KDPI ≤85) minus dialysis | +$169 (2016) | $211 | $204 | Full text |
| [@axelrod2018] | Transplant (KDPI >85) minus dialysis | +$38,459 (2016) | $48,116 | $46,482 | Full text |
| [@eckman2020] (lifetime) | Accept HCV-viremic kidney minus wait | −$37,918 (2018) | −$45,380 | −$44,300 | Abstract |

`cost_cancellation` and `cost_nonacceptance` stay at 0 only if decline-and-wait is the zero reference. Every accept-now cost must then be expressed relative to it.

## Health outcomes

The current 5.5-QALY gain is plausible only against a dialysis-forever reference.

- Against decline-and-wait, US models that allow later transplantation put the per-offer gain from accepting a good kidney at roughly 0.1–1.2 QALYs, usually with a cost saving.
- No source estimates the penalty for declining after a positive CrAg screen. Every accept-versus-decline figure comes from another decline reason (high KDPI, an infectious-risk label, HCV viremia, older DCD donors), so applying them here is a judgment.

### Accepting now versus declining and waiting

| Source | Setting and horizon | Result (accept minus decline) | Access |
|---|---|---|---|
| [@eckman2020] | HCV-viremic kidney versus waiting for an HCV-negative one; lifetime; 2018 USD; base waits about 1.56 versus 4 years | +0.91 or +1.19 QALY (secondary sources conflict); −$37,918. Accepting preferred unless the viremic wait exceeds 3.1 years | Abstract and secondary |
| [@eckman2018] | HCV-positive recipients: transplant first versus treat first and wait; lifetime; 2017 USD | +0.50 QALY; −$41,591. Waiting preferred only if the extra wait is under 161 days | Abstract |
| [@kadatz2018] | HCV-NAT-positive kidney versus waiting 1–5 more years | $56,018 per QALY versus 1 more year of waiting; accepting dominates versus ≥2 years | Abstract |
| [@gupta2018] | HCV D+/R−; 5-year horizon | +0.1 life-year; about −$191,000 | Abstract |
| [@kilambi2019] | One example patient aged 64; undiscounted | +255 quality-adjusted days (about 0.70 QALY) at KDPI 0–69; −84 days (about −0.23 QALY) at KDPI ≥95 (computed from Table 6, at utilities 0.82 and 0.70) | Full text |
| [@ellison2023] | High-KDPI kidney versus waiting; 5 years; 2011 USD; 3% | No mean per-patient difference reported. QALY gain in 81% of 129,024 candidate profiles, from 89.9% at KDPI 51–60 to 59.0% at KDPI 91–100 | Full text |
| [@wey2018] | Offer decision tool | 3-year patient survival 3–7 percentage points higher if accepted | Full text (partial) |
| [@cohen2019] | Marginal structural Cox model, all offers | HR for death, accept versus decline: 1.15 (1.01–1.30) at 0–3 months; 0.76 (0.65–0.89) at 3–6 months; 0.53 (0.48–0.59) at 2–3 years. 43% of first-offer decliners were ever transplanted | Full text |
| [@bowring2018] | Declined PHS increased-risk kidney (median KDPI 30) | 5 years after declining: 31.0% received a standard-risk deceased-donor kidney, 6.1% another increased-risk kidney, 8.0% a living-donor kidney; 20.0% died, 19.5% delisted, 15.5% still waiting. The eventual kidney had median KDPI 52 versus 21 declined. Adjusted HR for death, accept versus decline: 2.06 at 0–30 days, 0.67 at 1–6 months, 0.52 after 6 months (P values only) | Full text |
| [@husain2019] | Candidates with declined offers | 29.2% later received a deceased-donor kidney, 11.0% a living-donor kidney; 9.3% died; 21.2% delisted; 29.3% still waiting | Full text |
| [@mohan2025] | Post-2021 allocation system | Median 59 days from first offer to a deceased-donor transplant, among those transplanted | Full text |
| [@yu2021] | Declined older DCD kidneys | At 6 years: 40.9% received another deceased-donor kidney, 22.6% died, 22.0% removed; median 3.7 years to transplant | Full text |

Bowring 2018 is the closest analogue for declining an otherwise good kidney after a false-positive screen.

### Transplant versus dialysis forever

Axelrod et al. [@axelrod2018] (full text, Table 3) is the best US source: 10-year horizon, 3% discounting, 2016 USD. Dialysis yields 4.03 QALYs.

- KDPI ≤85: 6.07 QALYs, +2.04 and +$169 versus dialysis.
- KDPI >85: +1.17 QALYs and +$38,459.
- PHS increased-risk donor: +1.88 QALYs and +$14,935.

Its percentiles describe variation between simulated patients, not parameter uncertainty. No full-text lifetime US estimate confirming or refuting 5.5 QALYs was found.

**Utilities.** Transplant 0.82 (0.74–0.90) and dialysis 0.70 (0.62–0.78) [@wyld2012, Table 2]. Sample their difference, −0.11 (−0.15 to −0.08), rather than the two independently.

### Loss from recipient cryptococcosis

No utility weight or QALY decrement for cryptococcosis after transplant has been published. `q_loss_cryptococcus` has to be built from excess mortality, graft loss with return to dialysis, an acute decrement during treatment and any lasting neurological deficit.

| Source | Population | Outcome | Access |
|---|---|---|---|
| [@fiorica2025] (the CEA author is a co-author) | 1,265 US Medicare kidney recipients, 2006–2016; median onset 30.5 months | 6-month all-cause mortality 22% (280/1,265; about 19.8–24.4%, computed). ICU 44%, AKI 49%. No control group, so not attributable | Full text |
| [@tardieu2022] | 88 French kidney recipients | 12-month mortality 32.8% with meningitis versus 25.9% without; graft failure 19/61 (31.1%) with meningitis at last follow-up (median 9.4 months) | Full text |
| [@penumarthi2021] | Donor-derived, all organs | 5/23 (22%) infected recipients died, 11–368 days after transplant | Full text |
| [@singh2007] | 111 organ recipients | 90-day mortality 14% | Abstract |
| [@sun2009] | 75 recipients with CNS disease | 90-day mortality 10.9% on lipid amphotericin versus 40.0% on conventional | Abstract |
| [@singh2005] | 54 kidney recipients | Graft loss 2/3 with immune reconstitution syndrome versus 3/51 without | Abstract |

Lasting neurological deficits are documented only outside transplantation: residual visual impairment in 54% of HIV-negative survivors in Vietnam [@chau2010], and moderate-to-severe cognitive impairment in 63% of HIV patients in Uganda at 12 weeks [@nsangi2025].

### Dependencies to respect in the model

- **Shared drivers.** ICU admission, AKI and renal failure drive both cryptococcosis mortality and hospital cost, so disease cost and QALY loss should rise together. No source gives a correlation coefficient.
- **One wait-time input.** The accept-now QALY gain and its cost saving both grow with the expected extra wait and shrink as KDPI rises. Model them as joint functions of one wait-time input, not as independent draws.
- **Same endpoint.** `q_loss_cryptococcus` must be measured against the same accept-now endpoint it is subtracted from. If an early death makes the loss exceed the accept-now gain, the cryptococcosis endpoint falls below decline-and-wait; that is plausible and should not be truncated.
- **Not confidence intervals.** Differences between Ellison's candidate profiles are recipient heterogeneity, and microsimulation intervals such as Yanev's [@yanev2021] are Monte Carlo error.

## Evidence gaps

Three gaps carry most of the decision weight, and no published study fills any of them:

1. donor CrAg prevalence;
2. transmission risk from a screen-detected, largely asymptomatic donor;
3. what transplant teams do with a kidney from a CrAg-positive donor.

Other quantities with no usable evidence:

- prophylaxis efficacy against donor-derived infection in kidney recipients;
- a utility decrement or lasting neurological deficit after cryptococcosis in a transplant recipient;
- the expected additional wait after declining a CrAg-positive offer, by candidate group;
- fluconazole uptake when the donor result is known before transplant;
- delays or declines caused by pending donor test results;
- the program cost of donor testing;
- quantified harms of fluconazole prophylaxis in kidney recipients.

## Sources not accessed or only partly accessed

- **Full text not retrievable:** Kaul 2021 (AJT, ten years of donor-derived disease); TRANSNET (Pappas 2010); the AST guidelines [@baddley2019; @wolfe2019]; Eckman 2020 Table 2; Kadatz 2018, Gupta 2018 and Shah 2023 [@shah2023]; Wang 2025; the Benedict 2025 update (2024 USD); USRDS 2024–2025 cost chapters; the Camargo 2018 literature review; George 2018 (CID).
- **Partly retrieved:** Bowring 2018 reports hazard ratios with P values only. The official CMS lab fee file sits behind an AMA license page, so the Medicare rate came from a state copy.
- **Read only through a summarizing tool:** the 2019 ATC abstract and the SRTR 2024 chapters.
- **Blocked for automated access:** Europe PMC, the BLS site and the BEA table; FRED copies of the same series were used.

## Search record

Searches were run on 19 Oct 2026 in PubMed and the general web. Query families:

- donor-derived cryptococcosis;
- cryptococcal antigen with deceased, brain-dead or potential donors;
- cryptococcosis incidence in kidney or solid organ recipients;
- CrAg lateral-flow accuracy, false positives and HIV-negative populations;
- fluconazole prophylaxis and pre-emptive therapy;
- declined kidney offers and outcomes;
- HCV-viremic donor cost-effectiveness analyses;
- cryptococcosis hospitalization costs;
- USRDS Medicare spending.

## References

::: {#refs}
:::
