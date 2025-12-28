#This file performs setup and other tasks for the CEA analysis

library(DiagrammeR)
library(DiagrammeRsvg)
library(glue)
library(tibble)
library(gt)
library(tidyverse)
library(ggtext)
library(xml2)

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

#Shape parameters
shape_parameter<-list()
shape_parameter$p_usage<-10
shape_parameter$p_donor_cryptococcus<-10
shape_parameter$p_transmission<-10
shape_parameter$p_spont_cryptococcus<-10 
shape_parameter$p_sensitivity<-10
shape_parameter$p_specificity<-10 
shape_parameter$p_cancelled<-10
shape_parameter$p_prophrate<-10 
shape_parameter$p_prophefficacy<-10
shape_parameter$cost_test<-4
shape_parameter$cost_disease<-4
shape_parameter$cost_fluconazole<-4
shape_parameter$cost_cancellation<-NA
shape_parameter$cost_nocryptococcus<-NA
shape_parameter$cost_nonacceptance<-NA
shape_parameter$q_nocryptococcus<-4
shape_parameter$q_noacceptance<-NA
shape_parameter$q_loss_cryptococcus<-4


# willingness-to-pay threshold
wtp <- 100000  

#Set seed for reproducibility
set.seed(42)