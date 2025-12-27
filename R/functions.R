#Functions for CEA

#
calculate_cost_QALY_QC<-function(p_usage=0.43, 
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
  

  #Define return list
  return_list<-list()
  
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
  
  #Summary object ofr
  summary_tibble<-path_table%>%
    group_by(strategy)%>%
    summarize(total_probability=sum(probability), total_expected_cost=sum(cost_expected), total_expected_qaly=sum(qaly_expected))
  
  #Final return
  return(return_list)
}






