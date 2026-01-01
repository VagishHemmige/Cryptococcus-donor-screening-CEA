#Functions for CEA

#Function for the calculations needed for PSA. This function is used to calculate the cost and QALY
#of each strategy for each simulation
calculate_cost_QALY_QC<-function(p_usage=expected_value$p_usage, 
                                 p_donor_cryptococcus=expected_value$p_donor_cryptococcus, 
                                 p_transmission=expected_value$p_transmission, 
                                 p_spont_cryptococcus=expected_value$p_spont_cryptococcus, 
                                 p_sensitivity=expected_value$p_sensitivity,
                                 p_specificity=expected_value$p_specificity, 
                                 p_cancelled=expected_value$p_cancelled, 
                                 p_prophrate=expected_value$p_prophrate, 
                                 p_prophefficacy=expected_value$p_prophefficacy,
                                 number_donors=expected_value$number_donors,
                                 cost_test=expected_value$cost_test,
                                 cost_disease=expected_value$cost_disease,
                                 cost_fluconazole=expected_value$cost_fluconazole,
                                 cost_cancellation=expected_value$cost_cancellation,
                                 cost_nocryptococcus=expected_value$cost_nocryptococcus,
                                 cost_nonacceptance=expected_value$cost_nonacceptance,
                                 q_nocryptococcus=expected_value$q_nocryptococcus,
                                 q_noacceptance=expected_value$q_noacceptance,
                                 q_loss_cryptococcus=expected_value$q_loss_cryptococcus
)
{
  
  #Define parameters which are derived from the input parameters
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
  
  #Define tibble of outcomes
  path_table<-tribble(
    ~strategy, ~acceptance, ~donor_dz_status, ~donor_test_result, ~cancellation, ~proph, ~outcome, ~probability, ~cost_total, ~qaly_total,
    "No Screening","Accept","Positive",NA,NA,NA,"Recipient cryptococcus",p_usage*p_donor_cryptococcus*p_transmission,cost_disease,q_cryptococcus,
    "No Screening","Accept","Positive",NA,NA,NA,"No cryptococcus",p_usage*p_donor_cryptococcus*p_nontransmission,cost_nocryptococcus,q_nocryptococcus,
    "No Screening","Accept","Negative",NA,NA,NA,"Recipient cryptococcus",p_usage*p_donor_nocryptococcus*p_spont_cryptococcus,cost_disease,q_cryptococcus,
    "No Screening","Accept","Negative",NA,NA,NA,"No cryptococcus",p_usage*p_donor_nocryptococcus*p_nospont_cryptococcus,cost_nocryptococcus,q_nocryptococcus,
    "No Screening","Non-accept",NA,NA,NA,NA,"Non-accept",p_nonusage,cost_nonacceptance,q_noacceptance,
    
    
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
    "Screening","Non-accept",NA,NA,NA,NA,"Non-accept",p_nonusage,cost_nonacceptance+cost_test,q_noacceptance,
    
  )%>%
    mutate(cost_expected=probability*cost_total,
           qaly_expected=probability*qaly_total)
  
  #Summary object is created via summarize and then converted into long format
  summary_tibble<-path_table%>%
    group_by(strategy)%>%
    summarize(total_expected_cost=sum(cost_expected), total_expected_qaly=sum(qaly_expected))%>%
    mutate(
      strategy = recode(
        strategy,
        "No Screening" = "ns",
        "Screening"    = "s"
      )
    ) %>%
    tidyr::pivot_wider(
      names_from  = strategy,
      values_from = c(
        total_expected_cost,
        total_expected_qaly
      ),
      names_sep = "_"
    )
  
  #Final return
  return(summary_tibble)
}

#Creates the GrViz object from the parameters.  Used both directly in the create_diagram_QC.R script as well as by the decision tree function below.
plot_tree_diagram_QC<-function(p_usage, 
                                 p_donor_cryptococcus, 
                                 p_transmission, 
                                 p_spont_cryptococcus, 
                                 p_sensitivity,
                                 p_specificity, 
                                 p_cancelled, 
                                 p_prophrate, 
                                 p_prophefficacy,
                                 number_donors,
                                 cost_test,
                                 cost_disease,
                                 cost_fluconazole,
                                 cost_cancellation,
                                 cost_nocryptococcus,
                                 cost_nonacceptance,
                                 q_nocryptococcus,
                                 q_noacceptance,
                                 q_loss_cryptococcus,
                                 circle_diameter=1.4,
                                 box_width=1.7,
                                 box_height=1,
                                 diamond_width=3,
                                 diamond_height=1.5,
                                 p_nonusage,
                                 p_donor_nocryptococcus,
                                 p_nontransmission,
                                 p_nospont_cryptococcus,
                                 p_falsenegative,
                                 p_falsepositive,
                                 p_nocancelled,
                                 p_noprophrate,
                                 p_noprophefficacy,
                                 p_breakthrough_donorpos,
                                 p_nobreakthrough_donorpos,
                                 p_breakthrough_donorneg,
                                 p_nobreakthrough_donorneg,
                                 q_cryptococcus)
                                 
{
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
  
  #Return plot
  grViz(grviz_text)%>%
    return()
  
  
}
                                 


#Function that creates tree diagram.  It returns the diagram created via the GraphViz language, a table of input parameters
#and the output results from running the CEA on the parameterized decision tree.
#Default parameters for the decision tree are those of the base case as determined by the values of the expected_value
#list defined in the setup.R file
create_tree_diagram_QC<-function(p_usage=expected_value$p_usage, 
                                 p_donor_cryptococcus=expected_value$p_donor_cryptococcus, 
                                 p_transmission=expected_value$p_transmission, 
                                 p_spont_cryptococcus=expected_value$p_spont_cryptococcus, 
                                 p_sensitivity=expected_value$p_sensitivity,
                                 p_specificity=expected_value$p_specificity, 
                                 p_cancelled=expected_value$p_cancelled, 
                                 p_prophrate=expected_value$p_prophrate, 
                                 p_prophefficacy=expected_value$p_prophefficacy,
                                 number_donors=expected_value$number_donors,
                                 cost_test=expected_value$cost_test,
                                 cost_disease=expected_value$cost_disease,
                                 cost_fluconazole=expected_value$cost_fluconazole,
                                 cost_cancellation=expected_value$cost_cancellation,
                                 cost_nocryptococcus=expected_value$cost_nocryptococcus,
                                 cost_nonacceptance=expected_value$cost_nonacceptance,
                                 q_nocryptococcus=expected_value$q_nocryptococcus,
                                 q_noacceptance=expected_value$q_noacceptance,
                                 q_loss_cryptococcus=expected_value$q_loss_cryptococcus,
                                 circle_diameter=1.4,
                                 box_width=1.7,
                                 box_height=1,
                                 diamond_width=3,
                                 diamond_height=1.5
                                 
)
{
  
  #Define parameters that are derived from parameters passed to the function
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
  
  
  #Define return list
  return_list<-list()
  
  #Define tibble of outcomes
  return_list$path_table<-tribble(
    ~strategy, ~acceptance, ~donor_dz_status, ~donor_test_result, ~cancellation, ~proph, ~outcome, ~probability, ~cost_total, ~qaly_total,
    "No Screening","Accept","Positive",NA,NA,NA,"Recipient cryptococcus",p_usage*p_donor_cryptococcus*p_transmission,cost_disease,q_cryptococcus,
    "No Screening","Accept","Positive",NA,NA,NA,"No cryptococcus",p_usage*p_donor_cryptococcus*p_nontransmission,cost_nocryptococcus,q_nocryptococcus,
    "No Screening","Accept","Negative",NA,NA,NA,"Recipient cryptococcus",p_usage*p_donor_nocryptococcus*p_spont_cryptococcus,cost_disease,q_cryptococcus,
    "No Screening","Accept","Negative",NA,NA,NA,"No cryptococcus",p_usage*p_donor_nocryptococcus*p_nospont_cryptococcus,cost_nocryptococcus,q_nocryptococcus,
    "No Screening","Non-accept",NA,NA,NA,NA,"Non-accept",p_nonusage,cost_nonacceptance,q_noacceptance,
    
    
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
    "Screening","Non-accept",NA,NA,NA,NA,"Non-accept",p_nonusage,cost_nonacceptance+cost_test,q_noacceptance,
    
  )%>%
    mutate(cost_expected=probability*cost_total,
           qaly_expected=probability*qaly_total,
           nmv=wtp*qaly_expected-cost_expected)
  
  #Summary object
  return_list$summary_tibble<-return_list$path_table%>%
    group_by(strategy)%>%
    summarize(total_probability=sum(probability), total_expected_cost=sum(cost_expected), total_expected_qaly=sum(qaly_expected))
  
  #Create parameter table to return
  return_list$parameter_tibble<-tribble(
    ~parameter, ~value,
    "p_usage", p_usage, 
    "p_donor_cryptococcus",p_donor_cryptococcus, 
    "p_transmission",p_transmission, 
    "p_spont_cryptococcus",p_spont_cryptococcus, 
    "p_sensitivity",p_sensitivity,
    "p_specificity",p_specificity, 
    "p_cancelled",p_cancelled, 
    "p_prophrate",p_prophrate, 
    "p_prophefficacy",p_prophefficacy,
    "number_donors",number_donors,
    "cost_test",cost_test,
    "cost_disease",cost_disease,
    "cost_fluconazole",cost_fluconazole,
    "cost_cancellation",cost_cancellation,
    "cost_nocryptococcus",cost_nocryptococcus,
    "cost_nonacceptance",cost_nonacceptance,
    "q_nocryptococcus",q_nocryptococcus,
    "q_noacceptance",q_noacceptance,
    "q_loss_cryptococcus",q_loss_cryptococcus)

  return_list$cost_difference<-return_list$summary_tibble[[2,3]]-return_list$summary_tibble[[1,3]]
  return_list$qaly_difference<-return_list$summary_tibble[[2,4]]-return_list$summary_tibble[[1,4]]
  return_list$nmb<-wtp*return_list$qaly_difference-return_list$cost_difference
  

  #Add grviz object to return_list
  return_list$plot<-plot_parameters_in_tree<-plot_tree_diagram_QC(p_usage=p_usage, 
                                                                  p_donor_cryptococcus=p_donor_cryptococcus, 
                                                                  p_transmission=p_transmission, 
                                                                  p_spont_cryptococcus=p_spont_cryptococcus, 
                                                                  p_sensitivity=p_sensitivity,
                                                                  p_specificity=p_specificity, 
                                                                  p_cancelled=p_cancelled, 
                                                                  p_prophrate=p_prophrate, 
                                                                  p_prophefficacy=p_prophefficacy,
                                                                  number_donors=number_donors,
                                                                  cost_test=cost_test,
                                                                  cost_disease=cost_disease,
                                                                  cost_fluconazole=cost_fluconazole,
                                                                  cost_cancellation=cost_cancellation,
                                                                  cost_nocryptococcus=cost_nocryptococcus,
                                                                  cost_nonacceptance=cost_nonacceptance,
                                                                  q_nocryptococcus=q_nocryptococcus,
                                                                  q_noacceptance=q_noacceptance,
                                                                  q_loss_cryptococcus=q_loss_cryptococcus,
                                                                  circle_diameter=circle_diameter,
                                                                  box_width=box_width,
                                                                  box_height=box_height,
                                                                  diamond_width=diamond_width,
                                                                  diamond_height=diamond_height,
                                                                  p_nonusage="1-p_usage",
                                                                  p_donor_nocryptococcus=p_donor_nocryptococcus,
                                                                  p_nontransmission=p_nontransmission,
                                                                  p_nospont_cryptococcus=p_nospont_cryptococcus,
                                                                  p_falsenegative=p_falsenegative,
                                                                  p_falsepositive=p_falsepositive,
                                                                  p_nocancelled=p_nocancelled,
                                                                  p_noprophrate=p_noprophrate,
                                                                  p_noprophefficacy=p_noprophefficacy,
                                                                  p_breakthrough_donorpos=p_breakthrough_donorpos,
                                                                  p_nobreakthrough_donorpos=p_nobreakthrough_donorpos,
                                                                  p_breakthrough_donorneg=p_breakthrough_donorneg,
                                                                  p_nobreakthrough_donorneg=p_nobreakthrough_donorneg,
                                                                  q_cryptococcus=q_cryptococcus)
  
  
    
  #Final return
  return(return_list)
}

#This function adds annotations to SVGs and is used to add the results of the analysis to the SVG for the decision tree.
add_svg_annotation <- function(svg, lines,
                               x = 40, y = 1100,
                               fontsize = 50,
                               lineheight = 1.3) {
  
  text_node <- xml_add_child(
    svg,
    "text",
    x = x,
    y = y,
    fill = "black",
    `font-size` = fontsize,
    `font-family` = "Helvetica"
  )
  
  for (i in seq_along(lines)) {
    xml_add_child(
      text_node,
      "tspan",
      lines[[i]],
      x = x,
      dy = if (i == 1) "0" else as.character(fontsize * lineheight)
    )
  }
  
  invisible(svg)
}