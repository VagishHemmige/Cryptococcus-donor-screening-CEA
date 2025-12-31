
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Cryptococcus donor screening CEA

<!-- badges: start -->

<!-- badges: end -->

The goal of Cryptococcus donor screening CEA is to perform a CEA. The
full analysis Github website is available at
<https://vagishhemmige.github.io/Cryptococcus-donor-screening-CEA/>

# Base case analysis

## Model parameters

The parameters of the model are as below:

<img src="figures/parameter_table_QC.png"
     style="display:block; margin-left:auto; margin-right:auto; max-width:150px; height:auto;">

## Decision tree

This model leads to the following decision tree: ![CrAg decision
tree](figures/crag_tree_QC_annotated.svg)

## Path table

The decision tree above yields the following path table:

<figure>
<img src="figures/output_table_QC.png" alt="CrAg tree table" />
<figcaption aria-hidden="true">CrAg tree table</figcaption>
</figure>

## Summary table

The analysis above yields the following final summary table: ![Summary
table](figures/summary_table_QC.png)

# Sensitivity analyses

## Tornado diagrams

One-way sensitivity analysis yields the following tornado diagrams for
cost and QALYs: ![Cost tornado diagram](figures/tornado_cost.svg) ![QALY
tornado diagram](figures/tornado_qaly.svg)

## Probabilistic sensitivity analysis

We perform 10,000 simulations, yielding the following results for
sensitivity analysis: ![PSA plot](figures/PSA_plot.svg)
