#This file performs setup and other tasks for the CEA analysis

library(DiagrammeR)
library(DiagrammeRsvg)
library(glue)
library(tibble)
library(gt)
library(tidyverse)
library(ggtext)

#Save simulation parameters

#Expected values
expected_value<-list()
expected_value$p_usage<-0.43
expected_value$p_donor_cryptococcus<-0.001
expected_value$p_transmission<-0.867
expected_value$p_spont_cryptococcus<-0.005 
expected_value$p_sensitivity<-0.901
expected_value$p_specificity<-0.98 
expected_value$p_cancelled<-0.8
expected_value$p_prophrate<-0.51 
expected_value$p_prophefficacy<-0.88
expected_value$number_donors<-702
expected_value$cost_test<-2
expected_value$cost_disease<-110945
expected_value$cost_fluconazole<-6660
expected_value$cost_cancellation<-0
expected_value$cost_nocryptococcus<--10
expected_value$cost_nonacceptance<-0
expected_value$q_nocryptococcus<-5.5
expected_value$q_noacceptance<-0
expected_value$q_loss_cryptococcus<-2.5