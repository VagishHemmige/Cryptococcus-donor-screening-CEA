# TODO

Open work for the cryptococcus donor screening CEA, from the parameter literature review (evidence cutoff 19 Oct 2026). The evidence itself is in `evidence/evidence.md`. When an item is done, delete it here and record the outcome there or in the code.

## 1. Decisions to make (these belong in the analysis plan)

- [ ] **Unit of analysis.** Per potential donor, per deceased donor with kidneys recovered, or per kidney offer. This decides what `p_usage` means, whether two kidneys share one test, and how outcomes for two recipients are counted.
- [ ] **Screened population.** All deceased donors, or only potential donors dying of non-traumatic CNS disease. National figures in `evidence.md` describe all donors.
- [ ] **Time horizon and discounting**, shared by `p_spont_cryptococcus`, costs and QALYs.
- [ ] **Perspective and price year** (for example US healthcare sector, 2025 USD, CPI-U Medical Care to match `usRds::adjust_costs_for_inflation`).
- [ ] **Reference arm.** Decline-and-wait (with possible later transplant) or dialysis forever. `q_nocryptococcus`, `cost_nocryptococcus` and `q_loss_cryptococcus` must all use the same one.
- [ ] **Positive-screen protocol.** Act on a single positive, a titer threshold, or a confirmatory test before any decline.
- [ ] **Prophylaxis regimen** (dose, duration, renal adjustment) that `cost_fluconazole` represents.
- [ ] **WTP range** to report, in addition to $100,000.

## 2. Document the source of current values

None of these appear in a published source. Record where each came from, or replace it.

- [ ] `p_usage` 0.43
- [ ] `p_transmission` 0.867. Arithmetically 13/15; no source with that ratio was found.
- [ ] `p_cancelled` 0.8, `p_prophrate` 0.51, `p_prophefficacy` 0.88
- [ ] `p_sensitivity` 0.901, `p_specificity` 0.98
- [ ] `cost_fluconazole` $6,660. Equals 180 days × $37 per day; no source found.
- [ ] `cost_disease` $110,945 and its price year. It is close to, but not exactly, Charalambous 2018 ($103,236 in charges) inflated to 2019. If that is the source, it is multi-year, all-cause charges, not an episode cost.
- [ ] `cost_test` $2 and its price year.

## 3. Model changes the evidence points to

Each needs a decision before code changes.

- [ ] Revise `p_specificity` upward (center 0.99–0.995) and add a defective-lot or non-IMMY scenario (about 0.90).
- [ ] Split `p_cancelled` by true versus false positive, and by decline-and-reallocate versus discard. Add timing: a result that returns after implantation allows only the prophylaxis branch.
- [ ] Separate prophylaxis uptake under the status quo from uptake under a screening protocol.
- [ ] Revise `p_prophefficacy` toward 0.7. Apply only efficacy × (share of background-risk window covered) to background infection, or 0 in the base case.
- [ ] Replace `q_nocryptococcus` and `cost_nocryptococcus` with accept-now minus decline-and-wait values, driven jointly by one expected-wait input. Use a distribution that allows negative costs.
- [ ] Rebuild `q_loss_cryptococcus` from mortality, graft loss and a treatment decrement, measured against the accept-now endpoint.
- [ ] Correlate disease cost with QALY loss, or flag them as independent draws.
- [ ] Consider making transmission and prophylaxis efficacy depend on donor titer.
- [ ] Add a program cost for donor testing (staff, 24/7 availability, confirmatory titer) and monitoring costs during fluconazole.
- [ ] Reprice `cost_test`, `cost_fluconazole` and `cost_disease` from the sources in `evidence.md`.
- [ ] Decide whether `number_donors` (702) stays as a label; it is not used as a multiplier.

## 4. Elicitation targets

- [ ] Factor relating true donor antigenemia to recognized DTAC events (or test stored donor sera).
- [ ] Transmission from a screen-detected donor, ideally by titer (for example ≤1:10 versus ≥1:160).
- [ ] Prophylaxis efficacy against donor-derived infection in kidney recipients.
- [ ] Utility decrement and lasting neurological deficit after cryptococcosis in a transplant recipient.
- [ ] Expected additional wait after declining a CrAg-positive offer, by candidate group.
- [ ] What centers would do with a kidney from a CrAg-positive donor (for `p_cancelled` scenarios).

## 5. Check by hand against the source

These figures were read from an abstract, a summary tool or automated table extraction.

- [ ] 2019 ATC abstract (Santos 2019): all inputs and the break-even probability.
- [ ] SRTR 2024 ADR: kidney nonuse 29.3%, 1.87 recovered and 1.32 transplanted per donor (Figs DD 4, 7, 18).
- [ ] Penumarthi 2021 Tables 1–2 (read by PDF extraction); the 20/35 versus 23/35 prophylaxis discrepancy; 10 versus 8 kidney recipients given prophylaxis in Table 2.
- [ ] Eckman 2020 Table 2: is the QALY gain 0.91 or 1.19?
- [ ] Wang 2025: 2×2 table and reference standard.
- [ ] Dubbels 2017, Hevey 2020, Temfack 2021 table values.
- [ ] Medicare CLFS 2025 rate for CPT 87327 ($13.42) against the official CMS file.
- [ ] USRDS 2024–2025 ADR Chapter 9 per-person-per-year costs (to replace the 2018 values).
- [ ] Global guideline (Chang 2024) HIV fluconazole doses, read by automated extraction.

## 6. Full texts to obtain

- [ ] Baddley and Forrest 2019 (AST cryptococcosis guideline) and Wolfe and Ison 2019 (AST donor-derived infections): donor-specific recommendations.
- [ ] Singh 2012, AJT (AST donor-derived fungal infections guideline).
- [ ] Kaul 2021, AJT (ten years of donor-derived disease): cryptococcus-specific counts.
- [ ] Pappas 2010 (TRANSNET): kidney cryptococcosis counts.
- [ ] Eckman 2020, Kadatz 2018, Gupta 2018, Shah 2023.
- [ ] Wang 2025 (CMI).
- [ ] Benedict 2025 (Med Mycol): cryptococcosis cost per hospitalization in 2024 USD.
- [ ] Camargo 2018 literature review; George 2018 (CID); Charalambous 2018.

## 7. Bibliography

- [ ] Complete the title and authors for `kaul2016atc`, `mehta2018` and `idweek2017crag` in `references.bib` (entered from abstract summaries).
- [ ] Import `references.bib` into Zotero with Better BibTeX so the citation keys are kept, then let Zotero auto-export it.

## 8. Code issues noted during the review (19 Oct 2026; not re-verified)

- [ ] `create_sensitivity_analysis_QC.R` calls `ellipse()`, but no script loads the ellipse package.
- [ ] `cost_nocryptococcus = -10` is hard-coded again in the PSA, so changing `setup.R` alone does not update it.
- [ ] The cost tornado reassigns `nmb_difference_low` at the mean-value step, so `nmb_difference_mean` is missing for cost parameters.
- [ ] The PSA summary export saves `summary_gt_QC` instead of `PSA_summary_gt`.
- [ ] EVPI is labeled "value of partial information" but computed as perfect information.

## 9. Website

- [ ] `index.qmd`: the link describing donor-derived transmission points to Penumarthi 2021, and the `ctr.13547` link is Wolfe and Ison 2019 (donor-derived infections), not a cryptococcosis guideline. Relabel both.
- [ ] Fill in `quarto/about.qmd` (currently "Pending").
