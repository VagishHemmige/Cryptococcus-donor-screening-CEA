#Creates the tree diagram manually in R using grviz language

library(DiagrammeR)
library(DiagrammeRsvg)
library(glue)

#Define size parameters
circle_diameter<-1.4
box_width<-1.7
box_height<-1
diamond_width<-3
diamond_height<-1.5

#Fixed parameters for the diagram
#Costs
cost_test<-2
cost_disease<-110945
cost_fluconazole<-6660
cost_cancellation<-0
cost_nocryptococcus<--10

#N donors
number_donors<-702

# Probabilities: 
# p_usage, p_donor_cryptococcus, p_transmission, p_spont_cryptococcus, p_sensitivity
# p_specificity, p_cancelled, p_prophrate, p_prophefficacy
p_usage<-0.43
p_nonusage<-1-p_usage
p_donor_cryptococcus<-0.001
p_donor_nocryptococcus<-1-p_donor_cryptococcus
p_transmission<-0.867
p_nontransmission<-1-p_transmission
p_spont_cryptococcus<-0.005
p_nospont_cryptococcus<-1-p_spont_cryptococcus
p_sensitivity<-0.901
p_falsenegative<-1-p_sensitivity
p_specificity<-0.98
p_falsepositive<-1-p_specificity
p_cancelled<-0.8
p_nocancelled<-1-p_cancelled
p_prophrate<-0.51
p_noprophrate<-1-p_prophrate
p_prophefficacy<-0.88
p_noprophefficacy<-1-p_prophefficacy
p_breakthrough_donorpos<-(1-p_prophefficacy)*p_transmission
p_nobreakthrough_donorpos<-1-p_breakthrough_donorpos
p_breakthrough_donorneg<-(1-p_prophefficacy)*p_spont_cryptococcus
p_nobreakthrough_donorneg<-1-p_breakthrough_donorneg

#Text to define tree diagram for grviz
grviz_text<-glue("
digraph crag {{

  graph [rankdir=LR]
  node  [fontname=Helvetica, fontsize=14.5]
  edge  [fontname=Helvetica, fontsize=15]

  # ---- Nodes ----
  start [shape=box, label='Potential\ndonors\nN = {number_donors}', fixedsize=TRUE, width={box_width}, height={box_height}]

  no_screen [shape=box, fillcolor=palegreen, style=filled,
             label='No CrAg\nscreening\nCost = $0', fixedsize=TRUE, width={box_width}, height={box_height}]

  screen [shape=box, fillcolor=palegreen, style=filled,
          label='CrAg\nscreening\nCost = ${cost_test}', fixedsize=TRUE, width={box_width}, height={box_height}]

  used_ns [shape=circle, fillcolor=lightyellow, style=filled,
           label='Organs\nused', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]
           
  notused_ns [shape=circle, fillcolor=lightyellow, style=filled,
           label='Organs\nnot\nused', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  used_s [shape=circle, fillcolor=lightyellow, style=filled,
           label='Organs\nused', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]
           
  notused_s [shape=circle, fillcolor=lightyellow, style=filled,
           label='Organs\nnot\nused', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  donor_pos_ns [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nwith\ncryptococcus', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  donor_neg_ns [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nwithout\ncryptococcus', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]
  
  donor_pos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nwith\ncryptococcus', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  donor_neg_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nwithout\ncryptococcus', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  inf_yes_agpos_ns [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nCost = ${cost_disease}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
           
  inf_yes_agneg_ns [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nCost = ${cost_disease}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]

  inf_no_agpos_ns [shape=diamond, label='No\ncryptococcus\nCost = ${cost_nocryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  
  inf_no_agneg_ns [shape=diamond, label='No\ncryptococcus\nCost = ${cost_nocryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  
  ag_truepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nCrAg+', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  ag_falseneg_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nCrAg-', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  ag_falsepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nCrAg+', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  ag_trueneg_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Donor\nCrAg-', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  transplant_cancelled_ag_truepos_s [shape=diamond, fillcolor=lightblue, style=filled,
                label='Transplant\ncancelled\nCost = ${cost_cancellation}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  transplant_notcancelled_ag_truepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Transplant\nnot\ncancelled', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]
  
  transplant_cancelled_ag_falsepos_s [shape=diamond, fillcolor=lightblue, style=filled,
                label='Transplant\ncancelled\nCost = ${cost_cancellation}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  transplant_notcancelled_ag_falsepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Transplant\nnot\ncancelled', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]
                
  proph_yes_transplant_notcancelled_ag_truepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Fluconazole\nprophylaxis\nCost = ${cost_fluconazole}', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]
  proph_no_transplant_notcancelled_ag_truepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='No\nprophylaxis', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  proph_yes_transplant_notcancelled_ag_falsepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='Fluconazole\nprophylaxis\nCost = ${cost_fluconazole}', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]
  proph_no_transplant_notcancelled_ag_falsepos_s [shape=circle, fillcolor=lightyellow, style=filled,
                label='No\nprophylaxis', fixedsize=TRUE, width={circle_diameter}, height={circle_diameter}]

  inf_y_proph_yes_transplant_notcancelled_ag_truepos_s [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nCost = ${cost_disease}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  inf_n_proph_yes_transplant_notcancelled_ag_truepos_s [shape=diamond, label='No\ncryptococcosis\nCost = ${cost_nocryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]

  inf_y_proph_no_transplant_notcancelled_ag_truepos_s [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nCost = ${cost_disease}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  inf_n_proph_no_transplant_notcancelled_ag_truepos_s [shape=diamond, label='No\ncryptococcosis\nCost = ${cost_nocryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]


  inf_y_proph_yes_transplant_notcancelled_ag_falsepos_s [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nCost = ${cost_disease}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  inf_n_proph_yes_transplant_notcancelled_ag_falsepos_s [shape=diamond, label='No\ncryptococcus\nCost = ${cost_nocryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]

  inf_y_proph_no_transplant_notcancelled_ag_falsepos_s [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nCost = ${cost_disease}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  inf_n_proph_no_transplant_notcancelled_ag_falsepos_s [shape=diamond, label='No\ncryptococcus\nCost = ${cost_nocryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  
  
  inf_y_ag_falseneg_s [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nCost = ${cost_disease}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  inf_n_ag_falseneg_s [shape=diamond, label='No\ncryptococcus\nCost = ${cost_nocryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  
  inf_y_ag_trueneg_s [shape=diamond, fillcolor=mistyrose, style=filled,
           label='Recipient\ncryptococcosis\nCost = ${cost_disease}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
  inf_n_ag_trueneg_s [shape=diamond, label='No\ncryptococcus\nCost = ${cost_nocryptococcus}', fixedsize=TRUE, width={diamond_width}, height={diamond_height}]
                
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
  
  donor_neg_s -> ag_trueneg_s [label='p = {p_donor_cryptococcus}']
  donor_neg_s -> ag_falsepos_s [label='p = {p_donor_cryptococcus}']
  
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

grViz(grviz_text)
