#Creates the tree diagram manually in R using grviz language

#First, we source the helper functions for the analysis
source("R/functions.R")

#Create the plot that shows how the parameters appear in the diagram:
plot_parameters_in_tree<-plot_tree_diagram_QC(p_usage="p_usage", 
                     p_donor_cryptococcus="p_donor_cryptococcus", 
                     p_transmission="p_transmission", 
                     p_spont_cryptococcus="p_spont_cryptococcus", 
                     p_sensitivity="p_sensitivity",
                     p_specificity="p_specificity", 
                     p_cancelled="p_cancelled", 
                     p_prophrate="p_prophrate", 
                     p_prophefficacy="p_prophefficacy",
                     number_donors="number_donors",
                     cost_test="cost_test",
                     cost_disease="cost_disease",
                     cost_fluconazole="cost_fluconazole",
                     cost_cancellation="cost_cancellation",
                     cost_nocryptococcus="cost_nocryptococcus",
                     cost_nonacceptance="cost_nonacceptance",
                     q_nocryptococcus='q_nocryptococcus',
                     q_noacceptance="q_noacceptance",
                     q_loss_cryptococcus="q_loss_cryptococcus",
                     circle_diameter=1.4,
                     box_width=1.9,
                     box_height=1,
                     diamond_width=7,
                     diamond_height=2.5,
                     p_nonusage="1-p_usage",
                     p_donor_nocryptococcus="1-p_donor_cryptococcus",
                     p_nontransmission="1-p_transmission",
                     p_nospont_cryptococcus="1-p_spont_cryptococcus",
                     p_falsenegative="1-p_sensitivity",
                     p_falsepositive="1-p_specificity",
                     p_nocancelled="1-p_cancelled",
                     p_noprophrate="1-p_prophrate",
                     p_noprophefficacy="1-p_prophefficacy",
                     p_breakthrough_donorpos="(1-p_prophefficacy)*p_transmission",
                     p_nobreakthrough_donorpos="1-(1-p_prophefficacy)*p_transmission",
                     p_breakthrough_donorneg="(1-p_prophefficacy)*p_spont_cryptococcus",
                     p_nobreakthrough_donorneg="1-(1-p_prophefficacy)*p_spont_cryptococcus",
                     q_cryptococcus="q_nocryptococcus-q_loss_cryptococcus")

export_svg(plot_parameters_in_tree)%>%
  writeLines(., "figures/crag_tree_parameters.svg")

#Run the tree diagram
g_QC<-create_tree_diagram_QC()

#Print plot
g_QC$plot

#Save the tree diagram as svg
svg_QC <- export_svg(g_QC$plot)
writeLines(svg_QC, "figures/crag_tree_QC.svg")

#Annotate figure
svg_doc <- read_xml(svg_QC)
add_svg_annotation(
  svg_doc,
  c("Screening vs no screening:",
    glue("• ΔC = {scales::number(g_QC$cost_difference, accuracy = 0.01)}"),
    glue("• ΔQ = {formatC(g_QC$qaly_difference, format = 'f', digits = 4)}")  )
)
write_xml(svg_doc, "figures/crag_tree_QC_annotated.svg")

#Now, we look at the path table
result_tibble_QC<-g_QC$path_table

#Let's look at the tibble as a gt table
crag_table_QC<-result_tibble_QC%>%gt()%>%
  cols_label(
    strategy           = "Strategy",
    acceptance         = "Acceptance",
    donor_dz_status    = "Donor status",
    donor_test_result  = "Test result",
    cancellation       = "Transplant cancelled",
    proph              = "Prophylaxis",
    outcome            = "Outcome",
    probability        = "Path probability",
    cost_total         = "Cost if outcome ($)",
    cost_expected      = "Expected cost ($)",
    qaly_total         = "QALY if outcome",
    qaly_expected      = "Expected QALY"
  )%>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(everything())
  )%>%
  fmt_missing(everything(), missing_text = "")

#Print the table
crag_table_QC

#Save table
crag_table_QC%>%
  gtsave("figures/output_table_QC.png",
         vwidth = 2000,   # try 1600–2400
         vheight = 1200,
         expand = 10)
crag_table_QC%>%
  gtsave("tables/output_table_QC.docx")

#Let's create a table from the parameters
parameter_table_QC<-g_QC$parameter_tibble%>%
  gt()%>%
  cols_label(
    parameter = "Parameter",
    value     = "Value"
  )%>%
  fmt_number(
    columns = value,
    rows = parameter %in% c("number_donors"),
    decimals = 0
  )%>%
  fmt_number(
    columns = value,
    rows = parameter %in% c("cost_test", "cost_disease", "cost_fluconazole", "cost_cancellation", 
                            "cost_cryptococcus", "cost_nonacceptance", "cost_nocryptococcus"),
    decimals = 2
  )%>%
  fmt_number(
    columns = value,
    rows = parameter %in% c("q_nocryptococcus", "q_noacceptance", "q_loss_cryptococcus"),
    decimals = 1
  )

parameter_table_QC
parameter_table_QC%>%
  gtsave("figures/parameter_table_QC.png")
parameter_table_QC%>%
  gtsave("tables/parameter_table_QC.docx")

#Let's also look at the summary statistics for this table
summary_tibble_QC<-result_tibble_QC%>%
  group_by(strategy)%>%
  summarize(total_probability=sum(probability), total_expected_cost=sum(cost_expected),total_expected_qaly=sum(qaly_expected))

summary_gt_QC<-summary_tibble_QC%>%
  gt()%>%
  cols_label(
    strategy = "Strategy",
    total_probability = "Total Probability",
    total_expected_cost = "Total Expected Cost",
    total_expected_qaly = "Total Expected QALY"
  )%>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(everything())
  )

summary_gt_QC
summary_gt_QC%>%
  gtsave("figures/summary_table_QC.png")
summary_gt_QC%>%
  gtsave("tables/summary_table_QC.docx")
