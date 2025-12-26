#Creates the tree diagram manually in R using grviz language

library(DiagrammeR)
library(DiagrammeRsvg)
library(glue)
library(tibble)
library(gt)
library(tidyverse)

#Define size parameters for the diagram itself
circle_diameter<-1.4
box_width<-1.7
box_height<-1
diamond_width<-3
diamond_height<-1.5

create_tree_diagram_QC<-function(p_usage=0.43, 
                              p_donor_cryptococcus=0.001, 
                              p_transmission=0.867, 
                              p_spont_cryptococcus=0.005, 
                              p_sensitivity=0.901,
                              p_specificity=0.98, 
                              p_cancelled=0.8, 
                              p_prophrate=0.51, 
                              p_prophefficacy=0.88,
                              number_donors=702,
                              cost_test=2,
                              cost_disease=110945,
                              cost_fluconazole=6660,
                              cost_cancellation=0,
                              cost_nocryptococcus=-10,
                              cost_nonacceptance=0,
                              q_nocryptococcus=5.5,
                              q_noacceptance=0,
                              q_loss_cryptococcus=2.5
                              
                              
)
{
  
  #Define parameters
  p_nonusage<-1-p_usage
  p_donor_nocryptococcus<-1-p_donor_cryptococcus
  p_nontransmission<-1-p_transmission
  p_nospont_cryptococcus<-1-p_spont_cryptococcus
  p_falsenegative<-1-p_sensitivity
  p_falsepositive<-1-p_specificity
  p_nocancelled<-1-p_cancelled
  p_noprophrate<-1-p_prophrate
  p_noprophefficacy<-1-p_prophefficacy
  p_breakthrough_donorpos<-(1-p_prophefficacy)*p_transmission
  p_nobreakthrough_donorpos<-1-p_breakthrough_donorpos
  p_breakthrough_donorneg<-(1-p_prophefficacy)*p_spont_cryptococcus
  p_nobreakthrough_donorneg<-1-p_breakthrough_donorneg
  q_cryptococcus<-q_nocryptococcus-q_loss_cryptococcus
  
  #Text to define tree diagram for grviz
  grviz_text<-glue("
digraph crag {{

  graph [rankdir=LR]
  node  [fontname=Helvetica, fontsize=14.5]
  edge  [fontname=Helvetica, fontsize=15]

  # ---- Nodes ----
  start [shape=box, label='Potential\ndonors\nN = {number_donors}', fixedsize=TRUE, width={box_width}, height={box_height}]

  no_screen [shape=box, fillcolor=palegreen, style=filled,
             label='No CrAg\nscreening\nΔC = $0', fixedsize=TRUE, width={box_width}, height={box_height}]

  screen [shape=box, fillcolor=palegreen, style=filled,
          label='CrAg\nscreening\nΔC = ${cost_test}', fixedsize=TRUE, width={box_width}, height={box_height}]

  used_ns [shape=circle, fillcolor=lightyellow, style=filled,
           label='Organs\nused', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]
           
  notused_ns [shape=diamond, fillcolor=lightblue, style=filled,
           label='Organs\nnot used\nΔC = ${cost_nonacceptance}\nΔQ = {q_noacceptance}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]

  used_s [shape=circle, fillcolor=lightyellow, style=filled,
           label='Organs\nused', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]
           
  notused_s [shape=diamond, fillcolor=lightblue, style=filled,
           label='Organs\nnot used\nΔC = ${cost_nonacceptance}\nΔQ = {q_noacceptance}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]

  donor_pos_ns [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nwith\ncryptococcus', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  donor_neg_ns [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nwithout\ncryptococcus', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]
  
  donor_pos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nwith\ncryptococcus', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  donor_neg_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nwithout\ncryptococcus', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  inf_yes_agpos_ns [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nΔC = ${cost_disease}\nΔQ = {q_cryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
           
  inf_yes_agneg_ns [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nΔC = ${cost_disease}\nΔQ = {q_cryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]

  inf_no_agpos_ns [shape=diamond, label='No\ncryptococcus\nΔC = ${cost_nocryptococcus}\nΔQ = {q_nocryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  
  inf_no_agneg_ns [shape=diamond, label='No\ncryptococcus\nΔC = ${cost_nocryptococcus}\nΔQ = {q_nocryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  
  ag_truepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nCrAg+', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  ag_falseneg_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nCrAg-', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  ag_falsepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nCrAg+', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  ag_trueneg_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nCrAg-', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  transplant_cancelled_ag_truepos_s [shape=diamond, fillcolor=lightblue, style=filled,
                label='Transplant\ncancelled\nΔC = ${cost_cancellation}\nΔQ = {q_noacceptance}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]

  transplant_notcancelled_ag_truepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Transplant\nnot\ncancelled', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]
  
  transplant_cancelled_ag_falsepos_s [shape=diamond, fillcolor=lightblue, style=filled,
                label='Transplant\ncancelled\nΔC = ${cost_cancellation}\nΔQ = {q_noacceptance}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]

  transplant_notcancelled_ag_falsepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Transplant\nnot\ncancelled', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]
                
  proph_yes_transplant_notcancelled_ag_truepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Fluconazole\nprophylaxis\nΔC = ${cost_fluconazole}', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  proph_no_transplant_notcancelled_ag_truepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='No\nprophylaxis', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  proph_yes_transplant_notcancelled_ag_falsepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Fluconazole\nprophylaxis\nΔC = ${cost_fluconazole}', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  proph_no_transplant_notcancelled_ag_falsepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='No\nprophylaxis', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  inf_y_proph_yes_transplant_notcancelled_ag_truepos_s [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nΔC = ${cost_disease}\nΔQ = {q_cryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  
  inf_n_proph_yes_transplant_notcancelled_ag_truepos_s [shape=diamond, label='No\ncryptococcosis\nΔC = ${cost_nocryptococcus}\nΔQ = {q_nocryptococcus}', 
           fixedsize=TRUE, width={diamond_width}, height={diamond_height}]

  inf_y_proph_no_transplant_notcancelled_ag_truepos_s [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nΔC = ${cost_disease}\nΔQ = {q_cryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  inf_n_proph_no_transplant_notcancelled_ag_truepos_s [shape=diamond, label='No\ncryptococcosis\nΔC = ${cost_nocryptococcus}\nΔQ = {q_nocryptococcus}', 
           fixedsize=TRUE, width={diamond_width}, height={diamond_height}]


  inf_y_proph_yes_transplant_notcancelled_ag_falsepos_s [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nΔC = ${cost_disease}\nΔQ = {q_cryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  inf_n_proph_yes_transplant_notcancelled_ag_falsepos_s [shape=diamond, label='No\ncryptococcus\nΔC = ${cost_nocryptococcus}\nΔQ = {q_nocryptococcus}', 
           fixedsize=TRUE, width={diamond_width}, height={diamond_height}]

  inf_y_proph_no_transplant_notcancelled_ag_falsepos_s [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nΔC = ${cost_disease}\nΔQ = {q_cryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  inf_n_proph_no_transplant_notcancelled_ag_falsepos_s [shape=diamond, label='No\ncryptococcus\nΔC = ${cost_nocryptococcus}\nΔQ = {q_nocryptococcus}', 
           fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  
  
  inf_y_ag_falseneg_s [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nΔC = ${cost_disease}\nΔQ = {q_cryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  inf_n_ag_falseneg_s [shape=diamond, label='No\ncryptococcus\nΔC = ${cost_nocryptococcus}\nΔQ = {q_nocryptococcus}', 
           fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  
  inf_y_ag_trueneg_s [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nΔC = ${cost_disease}\nΔQ = {q_cryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  inf_n_ag_trueneg_s [shape=diamond, label='No\ncryptococcus\nΔC = ${cost_nocryptococcus}\nΔQ = {q_nocryptococcus}', 
           fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
                
  # ---- Structure ----
  start -> no_screen
  start -> screen

  no_screen -> used_ns [label='p = {p_usage}']
  no_screen -> notused_ns [label='p = {p_nonusage}']

  used_ns -> donor_pos_ns [label='p = {p_donor_cryptococcus}']
  used_ns -> donor_neg_ns [label='p = {p_donor_nocryptococcus}']

  donor_pos_ns -> inf_yes_agpos_ns [label='p = {p_transmission}']
  donor_pos_ns -> inf_no_agpos_ns  [label='p = {p_nontransmission}']

  donor_neg_ns -> inf_no_agneg_ns  [label='p = {p_nospont_cryptococcus}']
  donor_neg_ns -> inf_yes_agneg_ns  [label='p = {p_spont_cryptococcus}']
  
  screen -> used_s [label='p = {p_usage}']
  screen -> notused_s [label='p = {p_nonusage}']
  
  used_s -> donor_pos_s [label='p = {p_donor_cryptococcus}']
  used_s -> donor_neg_s [label='p = {p_donor_nocryptococcus}']
  
  donor_pos_s -> ag_truepos_s [label='p = {p_sensitivity}']
  donor_pos_s -> ag_falseneg_s [label='p = {p_falsenegative}']
  
  ag_truepos_s -> transplant_cancelled_ag_truepos_s [label='p = {p_cancelled}']
  ag_truepos_s -> transplant_notcancelled_ag_truepos_s [label='p = {p_nocancelled}']
  
  transplant_notcancelled_ag_truepos_s-> proph_yes_transplant_notcancelled_ag_truepos_s [label='p = {p_prophrate}']
  transplant_notcancelled_ag_truepos_s-> proph_no_transplant_notcancelled_ag_truepos_s [label='p = {p_noprophrate}']
  
  donor_neg_s -> ag_trueneg_s [label='p = {p_specificity}']
  donor_neg_s -> ag_falsepos_s [label='p = {p_falsepositive}']
  
  ag_falsepos_s -> transplant_cancelled_ag_falsepos_s [label='p = {p_cancelled}']
  ag_falsepos_s -> transplant_notcancelled_ag_falsepos_s [label='p = {p_nocancelled}']
  
  transplant_notcancelled_ag_falsepos_s -> proph_yes_transplant_notcancelled_ag_falsepos_s [label='p = {p_prophrate}']
  transplant_notcancelled_ag_falsepos_s -> proph_no_transplant_notcancelled_ag_falsepos_s [label='p = {p_noprophrate}']
  
  proph_yes_transplant_notcancelled_ag_truepos_s-> inf_y_proph_yes_transplant_notcancelled_ag_truepos_s [label='p = {p_breakthrough_donorpos}']
  proph_yes_transplant_notcancelled_ag_truepos_s-> inf_n_proph_yes_transplant_notcancelled_ag_truepos_s [label='p = {p_nobreakthrough_donorpos}']
  
  proph_no_transplant_notcancelled_ag_truepos_s-> inf_y_proph_no_transplant_notcancelled_ag_truepos_s [label='p = {p_transmission}']
  proph_no_transplant_notcancelled_ag_truepos_s-> inf_n_proph_no_transplant_notcancelled_ag_truepos_s [label='p = {p_nontransmission}']
  
  ag_falseneg_s->inf_y_ag_falseneg_s [label='p = {p_transmission}']
  ag_falseneg_s->inf_n_ag_falseneg_s [label='p = {p_nontransmission}']
  
  ag_trueneg_s->inf_y_ag_trueneg_s [label='p = {p_spont_cryptococcus}']
  ag_trueneg_s->inf_n_ag_trueneg_s [label='p = {p_nospont_cryptococcus}']
  
  proph_yes_transplant_notcancelled_ag_falsepos_s-> inf_y_proph_yes_transplant_notcancelled_ag_falsepos_s [label='p = {p_breakthrough_donorneg}']
  proph_yes_transplant_notcancelled_ag_falsepos_s-> inf_n_proph_yes_transplant_notcancelled_ag_falsepos_s [label='p = {p_nobreakthrough_donorneg}']
  
  proph_no_transplant_notcancelled_ag_falsepos_s-> inf_y_proph_no_transplant_notcancelled_ag_falsepos_s [label='p = {p_spont_cryptococcus}']
  proph_no_transplant_notcancelled_ag_falsepos_s-> inf_n_proph_no_transplant_notcancelled_ag_falsepos_s [label='p = {p_nospont_cryptococcus}']
  

}}
")
  
  #Define return list
  return_list<-list()
  
  #Add grviz object to return_list
  return_list$plot<-grViz(grviz_text)
  
  #Define tibble of outcomes
  return_list$path_table<-tribble(
    ~strategy, ~acceptance, ~donor_dz_status, ~donor_test_result, ~cancellation, ~proph, ~outcome, ~probability, ~cost_total, ~qaly_total,
    "No Screening","Accept","Positive",NA,NA,NA,"Recipient cryptococcus",p_usage*p_donor_cryptococcus*p_transmission,cost_disease,q_cryptococcus,
    "No Screening","Accept","Positive",NA,NA,NA,"No cryptococcus",p_usage*p_donor_cryptococcus*p_nontransmission,cost_nocryptococcus,q_nocryptococcus,
    "No Screening","Accept","Negative",NA,NA,NA,"Recipient cryptococcus",p_usage*p_donor_nocryptococcus*p_spont_cryptococcus,cost_disease,q_cryptococcus,
    "No Screening","Accept","Negative",NA,NA,NA,"No cryptococcus",p_usage*p_donor_nocryptococcus*p_nospont_cryptococcus,cost_nocryptococcus,q_nocryptococcus,
    "No Screening","Non-accept",NA,NA,NA,NA,NA,p_nonusage,cost_nonacceptance,q_noacceptance,
    
    
    "Screening","Accept","Positive","CrAg+","Cancelled",NA,"Cancelled",p_usage*p_donor_cryptococcus*p_sensitivity*p_cancelled,cost_cancellation+cost_test,q_noacceptance,
    "Screening","Accept","Positive","CrAg+","Not Cancelled","Fluconazole","Recipient cryptococcus",p_usage*p_donor_cryptococcus*p_sensitivity*p_nocancelled*p_prophrate*p_breakthrough_donorpos,cost_test+cost_disease+cost_fluconazole,q_cryptococcus,
    "Screening","Accept","Positive","CrAg+","Not Cancelled","Fluconazole","No cryptococcus",p_usage*p_donor_cryptococcus*p_sensitivity*p_nocancelled*p_prophrate*p_nobreakthrough_donorpos,cost_test+cost_nocryptococcus+cost_fluconazole,q_nocryptococcus,
    "Screening","Accept","Positive","CrAg+","Not Cancelled","None","Recipient cryptococcus",p_usage*p_donor_cryptococcus*p_sensitivity*p_nocancelled*p_noprophrate*p_transmission,cost_test+cost_disease,q_cryptococcus,
    "Screening","Accept","Positive","CrAg+","Not Cancelled","None","No cryptococcus",p_usage*p_donor_cryptococcus*p_sensitivity*p_nocancelled*p_noprophrate*p_nontransmission,cost_test+cost_nocryptococcus,q_nocryptococcus,
    "Screening","Accept","Positive","CrAg-",NA,NA,"Recipient cryptococcus",p_usage*p_donor_cryptococcus*p_falsenegative*p_transmission,cost_test+cost_disease,q_cryptococcus,
    "Screening","Accept","Positive","CrAg-",NA,NA,"No cryptococcus",p_usage*p_donor_cryptococcus*p_falsenegative*p_nontransmission,cost_test+cost_nocryptococcus,q_nocryptococcus,
    
    "Screening","Accept","Negative","CrAg+","Cancelled",NA,"Cancelled",p_usage*p_donor_nocryptococcus*p_falsepositive*p_cancelled,cost_cancellation+cost_test,q_noacceptance,
    "Screening","Accept","Negative","CrAg+","Not Cancelled","Fluconazole","Recipient cryptococcus",p_usage*p_donor_nocryptococcus*p_falsepositive*p_nocancelled*p_prophrate*p_breakthrough_donorneg,cost_test+cost_disease+cost_fluconazole,q_cryptococcus,
    "Screening","Accept","Negative","CrAg+","Not Cancelled","Fluconazole","No cryptococcus",p_usage*p_donor_nocryptococcus*p_falsepositive*p_nocancelled*p_prophrate*p_nobreakthrough_donorneg,cost_test+cost_nocryptococcus+cost_fluconazole,q_nocryptococcus,
    "Screening","Accept","Negative","CrAg+","Not Cancelled","None","Recipient cryptococcus",p_usage*p_donor_nocryptococcus*p_falsepositive*p_nocancelled*p_noprophrate*p_spont_cryptococcus,cost_test+cost_disease,q_cryptococcus,
    "Screening","Accept","Negative","CrAg+","Not Cancelled","None","No cryptococcus",p_usage*p_donor_nocryptococcus*p_falsepositive*p_nocancelled*p_noprophrate*p_nospont_cryptococcus,cost_test+cost_nocryptococcus,q_nocryptococcus,
    "Screening","Accept","Negative","CrAg-",NA,NA,"Recipient cryptococcus",p_usage*p_donor_nocryptococcus*p_specificity*p_spont_cryptococcus,cost_test+cost_disease,q_cryptococcus,
    "Screening","Accept","Negative","CrAg-",NA,NA,"No cryptococcus",p_usage*p_donor_nocryptococcus*p_specificity*p_nospont_cryptococcus,cost_test+cost_nocryptococcus,q_nocryptococcus,
    "Screening","Non-accept",NA,NA,NA,NA,NA,p_nonusage,cost_nonacceptance,q_noacceptance,
    
  )%>%
    mutate(cost_expected=probability*cost_total,
           qaly_expected=probability*qaly_total)
  
  #Summary object ofr
  return_list$summary_tibble<-return_list$path_table%>%
    group_by(strategy)%>%
    summarize(total_probability=sum(probability), total_expected_cost=sum(cost_expected), total_expected_qaly=sum(qaly_expected))
  
  #Final return
  return(return_list)
}

#Run the tree diagram
g_QC<-create_tree_diagram_QC()

#Pring plot
g_QC$plot

#Save as svg
svg_QC <- export_svg(g_QC$plot)
writeLines(svg_QC, "figures/crag_tree_QC.svg")

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
  )

crag_table_QC

#Save table
crag_table_QC%>%
  gtsave("figures/output_table_QC.png",
         vwidth = 2000,   # try 1600–2400
         vheight = 1200,
         expand = 10)

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
