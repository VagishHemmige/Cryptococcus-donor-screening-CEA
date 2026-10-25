# AGENTS.md

Guidance for everyone working on this repository: the maintainer, collaborators, and any AI coding agents working for them. It covers how to work here: conventions, unpublished material, and checks.

Four places divide the documentation:

| Where | Covers |
|---|---|
| `analysis-plan.md` | Study design: what the cost-effectiveness analysis is meant to do |
| `ARCHITECTURE.md` | Code structure: scripts, data flow, and the objects passed between them |
| `AGENTS.md` (this file) | How to work in the repository: rules, conventions, and checks |
| `TODO.md` | Implementation status: where the code does not yet match the plan, and known defects |

Read the three files before making changes. Each covers its own topic and does not repeat the others. The evidence for every model input is in `references/literature-review.md`, with citations in `references.bib`.

## The project

This repository evaluates donor serum cryptococcal antigen (CrAg) screening in US deceased-donor kidney transplantation with a decision-analytic cost-effectiveness model, and it publishes its code, methods and results as a Quarto website.

- **`analysis-plan.md` is the source of truth for design.** It covers the comparators, population, unit of analysis, perspective, time horizon, model structure, input definitions, and sensitivity analyses. Code implements the plan; it does not redefine it.
  - If the code needs to depart from the plan, change the plan in the same pull request.
  - Plan changes are tracked in git, not in a change log inside the plan. Commit each design change on its own, with a message that gives the reason and says whether any results that depend on it had already been seen.
  - The plan lists open design choices as **Decision D*n*** in its "Open decisions" table. Do not settle one in code. Raise it with the maintainer.
- **This is an analysis project, not an R package**, even though the scripts live in `R/` and there is an `.Rbuildignore`. Do not add DESCRIPTION, NAMESPACE, roxygen, or pkgdown infrastructure. Do not apply CRAN conventions.
- **Implementation status lives in `TODO.md`**, not in these files. When you find a gap between the code and the plan, or a defect, add it there. When an item is done, delete it and record the outcome in the code, the plan or the evidence page.

## R scripts

- **Running the analysis.** Open the `.Rproj` and run `R/main.R` from the project root. `main.R` sources the other scripts in dependency order. `ARCHITECTURE.md` describes what each script does and how data flows between them.
- **One job per script:** setup, model functions, base case, scenarios, sensitivity analyses. When you add a script:
  - source it from `main.R` at the right point
  - update `ARCHITECTURE.md` in the same commit
- **Header comment.** Each script begins with a comment saying what it does and which objects it needs from earlier scripts.
- **Constants, libraries, and model inputs** live in `R/setup.R`. Other scripts do not call `library()`, re-source `setup.R`, or hard-code values. Any number a reader might want to change, such as an input's base value or distribution shape, the willingness-to-pay threshold, the seed, or the number of PSA draws, is a named constant in `setup.R`.
- **Helper functions** go in `R/functions.R`, sourced after `setup.R`. A function takes its inputs as arguments and does not read global objects, including through default arguments.
- **No side effects outside the project.** Do not use `setwd()`, absolute paths, or writes outside the project.
- **Exploration code** does not stay in pipeline scripts. Delete it, or move it below a chunk label that no Quarto page uses. The website shows the scripts as they are.

## Chunk labels

Scripts are divided into labelled sections, so that Quarto pages can show the code without copying it:

```r
## ---- calculate-cost-qaly
```

- Every logical step has a label. A label covers the code from its line to the next label.
- Labels are kebab-case, describe the step, and are **unique across all scripts**. Quarto pulls chunks by label, so a duplicate label silently picks up the wrong code.
- Renaming or deleting a label breaks the Quarto page that uses it. Search the `.qmd` files before you change one.
- Scripts must still run end to end through `main.R`. Labels are comments and must never change what the code does.

## Quarto website

The Quarto site documents the code and displays results. It does not run the model.

- **Setup files:** `_quarto.yml` and `index.qmd` sit at the root. There is one page per pipeline script in `quarto/`, named after the script (for example, `quarto/sensitivity_analyses.qmd` for `R/sensitivity_analyses.R`). The `custom-callout` extension in `_extensions/` provides the `Rcode` callout. `_quarto.yml` has an explicit render list; a page that is not on it is not published.
- **Showing code:** each code page loads its script with `knitr::read_chunk("../R/<script>.R")` in a hidden chunk. It then shows each labelled section in a chunk with `#| eval: false`, with prose explaining what the section does and why. Pages link to the script on GitHub and to the scripts it depends on.
- **Results come from saved outputs.** Results pages read `output/` and `figures/`, and methods pages evaluate only the parameter chunks of `R/setup.R`. Every number shown on a page comes from those files, never typed into the prose.
- **Rendering:** the site renders to `docs/` and is served by GitHub Pages. Rerun `R/main.R` before rendering after any change to `R/`, re-render after changing a script's labels or a page's prose, and commit `docs/`.
- **New scripts:** when you add a script, add its page and its sidebar entry in `_quarto.yml` in the same commit.
- **Prose must match the code.** If a change alters what a section does, update that section's explanation in the same pull request.

## Outputs

- **Exported files:** results are written by the pipeline to `output/` (CSV), `figures/` and `tables/`. These folders are tracked, because the website reads them. Commit them together with the code and the rendered `docs/` that they belong to.
- **Figures** need alt text when they are published.
- `README.Rmd` is the source of `README.md`. Edit the `.Rmd` and re-knit; do not edit `README.md` directly.

## Unpublished material (applies to everyone)

The repository and website are public. The rules below apply to people and agents alike.

- **Never commit unpublished material.** That includes manuscript drafts, reviewer or co-author comments, data obtained by personal communication or from an organization without permission to publish, unpublished case reports, patient-level information, and copies of or long quotations from paywalled articles. Keep such material outside the repository.
- **An input that rests on unpublished data** is described as "source to be documented" until there is permission to cite it.
- **Never commit credentials**, tokens, or local `.Renviron` files.

## Coding conventions

- **Probabilities**
  - At every chance node, branch probabilities sum to 1. Complements are computed in code (`1 - p`), never entered as inputs. Every pathway ends in exactly one terminal node.
  - The model is defined once. While the pathway table exists in both `calculate_cost_QALY_QC()` and `create_tree_diagram_QC()` (`ARCHITECTURE.md`), a structural change goes into both, and the two must give the same base-case result.
- **Unit of analysis.** Every result is per the unit the plan defines. An aggregate (for example a cohort total) is an explicit multiplication in code, labelled with its multiplier.
- **Distributions**
  - Each input's family follows the plan: Beta for probabilities, gamma or log-normal for nonnegative costs, and a distribution that allows negative values for any quantity that can be negative.
  - Check that a distribution's parameterization reproduces its intended mean and interval.
  - Inputs are drawn independently unless the plan specifies joint sampling. A draw that feeds a complement must keep every branch within 0 to 1.
- **Costs** carry their price year. Inflation adjustment uses the constants in `setup.R`. Costs, payments and charges are not mixed.
- **Simulation.** The PSA must be reproducible from the seed and number of draws recorded in `setup.R`. The tornado, acceptability curve, EVPI and cost-effectiveness plane use the same inputs, threshold and NMB formula.
- **Evidence.** Every input value and range traces to `references/literature-review.md`. Never invent parameter values, sources, citations or clinical definitions. Add new citations to `references.bib` with author-year keys.
- **Style:** the tidyverse style guide. Style changes go in their own commits, separate from changes that alter analytical behavior.

## Checks for any change

Before opening a pull request, check the items below that apply to your change.

**Model structure**

- Do branch probabilities sum to 1 at every node, with complements computed?
- If the tree changed, were both pathway tables changed, and do they agree?
- Is the unit of analysis the same in the tree, inputs, outputs and website text?
- Do both strategies share the same baseline organ nonuse, so that only nonuse caused by a positive screen is attributed to screening?
- Are true-positive and false-positive paths handled separately for cancellation and prophylaxis?
- Do the scenario trees differ from the base case only in the inputs their descriptions name?

**Inputs**

- Do the values in `setup.R` match the evidence page and the website?
- Is any input hard-coded outside `setup.R`?
- Does each distribution fit its support and reproduce its intended mean and interval?
- Do all costs share one price year and one index?
- Are all terminal QALYs and costs measured against the same reference endpoint?

**Limiting cases** (run them)

- Prevalence 0: screening adds the test cost and false-positive consequences only.
- Sensitivity 1 and specificity 1: no false negatives or false positives.
- Cancellation 0 and prophylaxis efficacy 0: screening has no effect on QALYs.
- A small tree reproduced by hand gives the same expected values.

**Sensitivity analyses**

- Does each tornado input vary alone over its stated range, with others at base values, and do its endpoints match direct reruns?
- Is the PSA mean close to the deterministic base case?
- Are the summaries computed from the same draws?

**Structure and documentation**

- Does `ARCHITECTURE.md` still describe the scripts and the data flow?
- Are the chunk labels unique, and does each script's Quarto page still find every label it uses?
- Does every script have a Quarto page and a sidebar entry, and has `docs/` been re-rendered after rerunning `main.R`?
- Are all code-page chunks `eval: false`?

**Execution**

- Does `main.R` run from a clean session (`Ctrl+Shift+F10` in RStudio, then source `main.R`), with no objects left over from earlier work?
- Are paths relative?
- Is every package the code uses loaded in `setup.R`, and are package versions recorded?
- Are `output/`, `figures/` and `tables/` current with the code?

## Working with agents

- **Autonomy.** The person directing an agent decides how much it may do. That person may also give the agent stricter rules of their own, and those take precedence over this section. By default, an agent may:
  - edit code and documentation
  - add scripts and tests
  - run code and render the site
  - commit to a branch and open a pull request
- **Collaborators:** `master` is protected. Changes reach it only through a pull request that the maintainer reviews.
- **Every agent, under any level of autonomy:**
  - follows the unpublished-material rules above
  - asks before running the full pipeline (including the full PSA) or making expensive calls
  - does not settle an open decision in the plan
- Keep pull requests focused. In each one, state which section of the plan or which `TODO.md` item it addresses, and what was checked. Say whether it was checked by running code or by reading only. Never describe results from a reduced number of PSA draws as results.
- For references, use OPTN/SRTR, CDC, AST and global cryptococcosis guidelines, FDA labeling, USRDS, and PubMed. Cite what you used.
